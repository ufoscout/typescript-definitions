// Copyright 2019 Ian Castleden
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Exports serde-serializable structs and enums to Typescript definitions.
//!
//! please see documentation at [crates.io](https://crates.io/crates/typescript-definitions)

extern crate proc_macro;

#[macro_use]
extern crate quote;

#[macro_use]
// #[allow(unused_imports)]
extern crate lazy_static;

// extern crate proc_macro2;
// extern crate regex;
// extern crate serde_derive_internals;
// extern crate syn;

#[cfg(feature = "bytes")]
extern crate serde_bytes;

use proc_macro2::Ident;

use serde_derive_internals::{ast, Ctxt, Derive};
use std::str::FromStr;
use syn::DeriveInput;

mod derive_enum;
mod derive_struct;
mod patch;
mod quotet;
mod utils;

use utils::*;
use std::cell::Cell;

use patch::patch;

// too many TokenStreams around! give it a different name
type QuoteT = proc_macro2::TokenStream;

type QuoteMaker = quotet::QuoteT<'static>;

type Bounds = Vec<TSType>;

/// derive proc_macro to expose typescript definitions to `wasm-bindgen`.
///
/// please see documentation at [crates.io](https://crates.io/crates/typescript-definitions)
///
#[proc_macro_derive(TypescriptDefinition)]
pub fn derive_typescript_definition(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    if cfg!(any(debug_assertions, feature = "export-typescript")) {
        let parsed = Typescriptify::parse(input);
        let export_string = parsed.to_export_body();

        let export_ident = ident_from_str(&format!(
            "TS_EXPORT_{}",
            parsed.ident.to_string().to_uppercase()
        ));

        // eprintln!(
        //     "....[typescript] export type {}={};",
        //     parsed.ident, typescript_string
        // );
        let mut q = quote! {

            #[wasm_bindgen(typescript_custom_section)]
            pub const #export_ident : &'static str = #export_string;
        };

        if cfg!(any(test, feature = "test")) {
            let typescript_ident =
                ident_from_str(&format!("{}___typescript_definition", &parsed.ident));
            let ts = proc_macro2::TokenStream::from_str(&export_string)
                .unwrap()
                .to_string()
                .replace("\n", " ");

            q.extend(quote!(
                fn #typescript_ident ( ) -> &'static str {
                   #ts
                }

            ));
        }

        q.into()
    } else {
        proc_macro::TokenStream::new()
    }
}

/// derive proc_macro to expose typescript definitions to as a static function.
///
/// please see documentation at [crates.io](https://crates.io/crates/typescript-definitions)
///
#[proc_macro_derive(TypeScriptify)]
pub fn derive_type_script_ify(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    if cfg!(any(debug_assertions, feature = "export-typescript")) {
        let parsed = Typescriptify::parse(input);
        let export_body = parsed.to_export_body();
        // eprintln!("{}", export_string);
        let map = parsed.map();

        let ident = &parsed.ident;
        let ret = if parsed.ts_generics.len() == 0 {
            quote! {

                impl ::typescript_definitions::TypeScriptifyTrait for #ident {
                    fn type_script_ify() ->  String {
                        #export_body.to_owned()
                    }

                    fn type_script_fields() -> Option<Vec<&'static str>> {
                            #map
                    }
                }
            }
        } else {
            let generics = parsed.generic_args_with_lifetimes();
            let rustg = &parsed.rust_generics;
            quote! {

                impl#rustg ::typescript_definitions::TypeScriptifyTrait for #ident<#(#generics),*> {
                    fn type_script_ify() ->  String {
                         #export_body.to_owned()
                    }

                    fn type_script_fields() -> Option<Vec<&'static str>> {
                        #map
                    }
                }
            }
        };
        if let Some("1") = option_env!("TFY_SHOW_CODE") {
            eprintln!("{}", patch(&ret.to_string()));
        }

        ret.into()
    } else {
        proc_macro::TokenStream::new()
    }
}

struct ParseContext<'a> {

    ctxt: Option<&'a Ctxt>,
    is_enum: Cell<bool>,
}
impl<'a>  ParseContext<'a> {
    fn new(ctxt : &'a Ctxt) -> ParseContext<'a> {
        ParseContext {is_enum: Cell::new(false), ctxt: Some(ctxt)}
    } 
}
struct Typescriptify {
    ctxt: ParseContext<'static>,
    ident: syn::Ident,
    ts_generics: Vec<Option<(Ident, Bounds)>>, // None means a lifetime parameter
    body: QuoteMaker,
    rust_generics: syn::Generics,
}
impl Typescriptify {
    fn to_export_body(&self) -> String {
        let ts = self.body.to_string();
        let ts = patch(&ts);
        let ts_ident = self.ts_ident().to_string();
        let ts_ident = patch(&ts_ident);
        let e = if self.ctxt.is_enum.get() {
            format!("export enum {} {};", ts_ident, ts)
        } else {
            format!("export type {} = {};", ts_ident, ts)
        };

        e

    }

    /// type name suitable for typescript i.e. *no* 'a lifetimes
    fn ts_ident(&self) -> QuoteT {
        let ident = &self.ident;

        // currently we ignore trait bounds
        let args_wo_lt: Vec<_> = self.ts_generic_args_wo_lifetimes(false).collect();
        if args_wo_lt.len() == 0 {
            quote!(#ident)
        } else {
            quote!(#ident<#(#args_wo_lt),*>)
        }
    }

    fn ts_generic_args_wo_lifetimes(&self, with_bounds: bool) -> impl Iterator<Item = QuoteT> + '_ {
        self.ts_generics.iter().filter_map(move |g| match g {
            Some((ref ident, ref bounds)) => {
                // we ignore trait bounds for typescript
                if bounds.len() == 0 || !with_bounds {
                    Some(quote! (#ident))
                } else {
                    let bounds = bounds.iter().map(|ts| &ts.ident);
                    if with_bounds {
                        Some(quote! { #ident extends #(#bounds)&* })
                    } else {
                        Some(quote! { #ident : #(#bounds)+* })
                    }
                }
            }

            _ => None,
        })
    }

    fn generic_args_with_lifetimes(&self) -> impl Iterator<Item = QuoteT> + '_ {
        // suitable for impl<...> Trait for T<#generic_args_with_lifetime> ...
        // we need to return quotes because '_ is not an Ident
        self.ts_generics.iter().map(|g| match g {
            Some((ref i, ref _bounds)) => quote!(#i),
            None => quote!('_), // only need '_
        })
    }

    fn map(&self) -> QuoteT {
        match &self.body {
            quotet::QuoteT::Builder(b) => 
                match b.map() { 
                    Some(t) => t,
                    _ => quote! (None)
                }
            _ => quote!(None)
        }
    }

    fn parse<'a>(input: proc_macro::TokenStream) -> Self {
        let input: DeriveInput = syn::parse(input).unwrap();

        let cx = Ctxt::new();
        let container = ast::Container::from_ast(&cx, &input, Derive::Serialize);
        
        let (typescript, ctxt) = {
            let pctxt = ParseContext::new(&cx);

            let typescript = match container.data {
                ast::Data::Enum(ref variants) => {
                    derive_enum::derive_enum(variants, &container, &pctxt)
                },
                ast::Data::Struct(style, ref fields) => {
                    derive_struct::derive_struct(style, fields, &container, &pctxt)
                }
            };
            (typescript, ParseContext { ctxt: None, ..pctxt  })
        };

        let ts_generics = ts_generics(container.generics);


        // consumes context panics with errors
        cx.check().unwrap();
        Self {
            ctxt: ctxt,
            ident: container.ident,
            ts_generics: ts_generics,
            body: typescript,
            rust_generics: container.generics.clone(), // keep original type generics around for type_script_ify
        }
    }
}

fn ts_generics(g: &syn::Generics) -> Vec<Option<(Ident, Bounds)>> {
    // lifetime params are represented by None since we are only going
    // to translate the to '_

    // TODO: since `type_script_ify` needs to generate an impl of TypeScriptTrait
    // we really need to get *all* the information out of this so as to
    // correctly regenerate the generic args. Also syn:Generics implements
    // ToTokens so it can go straight into quote! Still need to replace 'a with '_
    // etc...
    // impl#generics TypeScriptTrait for A<... lifetimes to '_ and T without bounds>

    use syn::{ConstParam, GenericParam, LifetimeDef, TypeParam, TypeParamBound};
    g.params
        .iter()
        .map(|p| match p {
            GenericParam::Lifetime(LifetimeDef { /* lifetime,*/ .. }) => None,
            GenericParam::Type(TypeParam { ident, bounds, ..}) => {
                let bounds = bounds.iter()
                    .map(|b| match b {
                        TypeParamBound::Trait(t) => Some(&t.path),
                        _ => None // skip lifetimes for bounds
                    })
                    .filter_map(|b| b)
                    .map(last_path_element)
                    .filter_map(|b| b)
                    .collect::<Vec<_>>();

                Some((ident.clone(), bounds))
            },
            GenericParam::Const(ConstParam { ident, ..}) => Some((ident.clone(), vec![])),

        })
        .collect()
}

fn return_type(rt: &syn::ReturnType) -> Option<syn::Type> {
    match rt {
        syn::ReturnType::Default => None, // e.g. undefined
        syn::ReturnType::Type(_, tp) => Some(*tp.clone()),
    }
}

// represents a typescript type T<A,B>
struct TSType {
    ident: syn::Ident,
    args: Vec<syn::Type>,
    return_type: Option<syn::Type>, // only if function
}
fn last_path_element(path: &syn::Path) -> Option<TSType> {
    match path.segments.last().map(|p| p.into_value()) {
        Some(t) => {
            let ident = t.ident.clone();
            let args = match &t.arguments {
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) => args,
                // closures Fn(A,B) -> C
                syn::PathArguments::Parenthesized(syn::ParenthesizedGenericArguments {
                    output,
                    inputs,
                    ..
                }) => {
                    let args: Vec<_> = inputs.iter().map(|ty| ty.clone()).collect();
                    let ret = return_type(output);
                    return Some(TSType {
                        ident: ident,
                        args: args,
                        return_type: ret,
                    });
                }
                syn::PathArguments::None => {
                    return Some(TSType {
                        ident: ident,
                        args: vec![],
                        return_type: None,
                    });
                }
            };
            // ignore lifetimes
            let args = args
                .iter()
                .filter_map(|p| match p {
                    syn::GenericArgument::Type(t) => Some(t),
                    _ => None, // bindings A=I, expr, constraints A : B ... skip!
                })
                .map(|ty| ty.clone())
                .collect::<Vec<_>>();

            Some(TSType {
                ident: ident,
                args: args,
                return_type: None,
            })
        }
        None => None,
    }
}

fn generic_to_ts(ts: TSType, ctxt: &ParseContext) -> QuoteT {
    let tp_to_ts = |ty : &syn::Type| -> QuoteT {
        type_to_ts(ty, ctxt)
    };
    match ts.ident.to_string().as_ref() {
        "u8" | "u16" | "u32" | "u64" | "u128" | "usize" | "i8" | "i16" | "i32" | "i64" | "i128"
        | "isize" | "f64" | "f32" => quote! { number },
        "String" | "str" => quote! { string },
        "bool" => quote! { boolean },
        "Box" | "Cow" | "Rc" | "Arc" if ts.args.len() == 1 => tp_to_ts(&ts.args[0]),

        // std::collections
        "Vec" | "VecDeque" | "LinkedList" if ts.args.len() == 1 => {
            let t = tp_to_ts(&ts.args[0]);
            quote! { #t[] }
        }
        "HashMap" | "BTreeMap" if ts.args.len() == 2 => {
            let k = tp_to_ts(&ts.args[0]);
            let v = tp_to_ts(&ts.args[1]);
            // quote!(Map<#k,#v>)
            quote!( { [key: #k]:#v } )
        }
        "HashSet" | "BTreeSet" if ts.args.len() == 1 => {
            let k = tp_to_ts(&ts.args[0]);
            //quote!(Set<#k>)
            quote! ( #k[] )
        }
        "Option" if ts.args.len() == 1 => {
            let k = tp_to_ts(&ts.args[0]);
            quote!(  #k | undefined  )
        }
        "Result" if ts.args.len() == 2 => {
            let k = tp_to_ts(&ts.args[0]);
            let v = tp_to_ts(&ts.args[1]);
            // ugh!
            // see patch.rs...
            let bar = ident_from_str(patch::PATCH);
            quote!(  { Ok : #k } #bar { Err : #v }  )
        }
        "Fn" | "FnOnce" | "FnMut" => {
            let args = ts.args.iter().map(|ty| tp_to_ts(ty));
            if let Some(ref rt) = ts.return_type {
                let rt = tp_to_ts(rt);
                quote! { (#(#args),*) => #rt }
            } else {
                quote! { (#(#args),*) => undefined }
            }
        }
        _ => {
            let ident = ts.ident;
            if ts.args.len() > 0 {
                let args = ts.args.iter().map(|ty| tp_to_ts(ty));
                quote! { #ident<#(#args),*> }
            } else {
                quote! {#ident}
            }
        }
    }
}

/// # convert a `syn::Type` rust type to a
/// `TokenStream` of typescript type: basically i32 => number etc.
fn type_to_ts(ty: &syn::Type, ctxt : &ParseContext) -> QuoteT {
    // `type_to_ts` recursively calls itself occationally
    // finding a Path which it hands to last_path_element
    // which generates a "simplified" TSType struct which
    // is handed to `generic_to_ts` which possibly "bottoms out"
    // by generating tokens for typescript types.

    let type_to_array = |elem: &syn::Type| -> QuoteT {
        let tp = type_to_ts(elem, ctxt);
        quote! { #tp[] }
    };

    use syn::Type::*;
    use syn::{
        BareFnArgName, TypeArray, TypeBareFn, TypeGroup, TypeImplTrait, TypeParamBound, TypeParen,
        TypePath, TypePtr, TypeReference, TypeSlice, TypeTraitObject, TypeTuple,
    };
    match ty {
        Slice(TypeSlice { elem, .. }) => type_to_array(elem),
        Array(TypeArray { elem, .. }) => type_to_array(elem),
        Ptr(TypePtr { elem, .. }) => type_to_array(elem),
        Reference(TypeReference { elem, .. }) => type_to_ts(elem, ctxt),
        // fn(a: A,b: B, c:C) -> D
        BareFn(TypeBareFn { output, inputs, .. }) => {
            let mut args: Vec<Ident> = Vec::with_capacity(inputs.len());
            let mut typs: Vec<&syn::Type> = Vec::with_capacity(inputs.len());

            for (idx, t) in inputs.iter().enumerate() {
                let i = match t.name {
                    Some((ref n, _)) => match n {
                        BareFnArgName::Named(m) => m.clone(),
                        _ => ident_from_str("_"), // Wild token '_'
                    },
                    _ => ident_from_str(&format!("_dummy{}", idx)),
                };
                args.push(i);
                typs.push(&t.ty); // TODO: check type is known
            }
            // typescript lambda (a: A, b:B) => C

            let typs = typs.iter().map(|ty| type_to_ts(ty, ctxt));
            if let Some(ref rt) = return_type(&output) {
                let rt = type_to_ts(rt, ctxt);
                quote! { ( #(#args: #typs),* ) => #rt }
            } else {
                quote! { ( #(#args: #typs),* ) => undefined}
            }
        }
        Never(..) => quote! { never },
        Tuple(TypeTuple { elems, .. }) => {
            let elems = elems.iter().map(|t| type_to_ts(t, ctxt));
            quote!([ #(#elems),* ])
        }

        Path(TypePath { path, .. }) => match last_path_element(&path) {
            Some(ts) => generic_to_ts(ts, ctxt),
            _ => quote! { any },
        },
        TraitObject(TypeTraitObject { bounds, .. }) | ImplTrait(TypeImplTrait { bounds, .. }) => {
            let elems = bounds
                .iter()
                .filter_map(|t| match t {
                    TypeParamBound::Trait(t) => last_path_element(&t.path),
                    _ => None, // skip lifetime etc.
                })
                .map(|t| generic_to_ts(t, ctxt));

            // TODO check for zero length?
            // A + B + C => A & B & C
            quote!(#(#elems)&*)
        }
        Paren(TypeParen { elem, .. }) | Group(TypeGroup { elem, .. }) => {
            let tp = type_to_ts(elem, ctxt);
            quote! { ( #tp ) }
        }
        Infer(..) | Macro(..) | Verbatim(..) => quote! { any },
    }
}
