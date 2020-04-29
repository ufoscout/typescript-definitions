// Copyright 2019 Ian Castleden
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Exports serde-serializable structs and enums to Typescript definitions.
//!
//! Please see documentation at [crates.io](https://crates.io/crates/typescript-definitions)

extern crate proc_macro;
#[macro_use]
extern crate cfg_if;
use proc_macro2::Ident;
use quote::quote;
use serde_derive_internals::{ast, Ctxt, Derive};
// use std::str::FromStr;
use std::cell::RefCell;
use syn::DeriveInput;

mod attrs;
mod derive_enum;
mod derive_struct;
mod guards;
mod patch;
mod tests;
mod tots;
mod typescript;
mod utils;

use attrs::Attrs;
use utils::*;

use patch::patch;

// too many TokenStreams around! give it a different name
type QuoteT = proc_macro2::TokenStream;

type Bounds = Vec<TSType>;

struct QuoteMaker {
    pub source: QuoteT,
    /// type guard quote token stream
    pub verify: Option<QuoteT>,
    pub kind: QuoteMakerKind,
}

enum QuoteMakerKind {
    Object,
    Enum,
    Union {
        /// enum factory quote token stream
        enum_factory: Option<QuoteT>,
        /// enum handler quote token stream
        enum_handler: Option<QuoteT>,
    },
}

/* #region helpers */

#[allow(unused)]
fn is_wasm32() -> bool {
    use std::env;
    match env::var("WASM32") {
        Ok(ref v) => return v == "1",
        _ => {}
    }
    let mut t = env::args().skip_while(|t| t != "--target").skip(1);
    if let Some(target) = t.next() {
        if target.contains("wasm32") {
            return true;
        }
    };
    false
}

/// derive proc_macro to expose Typescript definitions to `wasm-bindgen`.
///
/// Please see documentation at [crates.io](https://crates.io/crates/typescript-definitions).
///
cfg_if! {
    if #[cfg(any(debug_assertions, feature = "export-typescript"))] {

        #[proc_macro_derive(TypescriptDefinition, attributes(ts))]
        pub fn derive_typescript_definition(input: proc_macro::TokenStream) -> proc_macro::TokenStream {

            if !(is_wasm32() || cfg!(feature="test")) {
                return proc_macro::TokenStream::new();
            }

            let input = QuoteT::from(input);
            do_derive_typescript_definition(input).into()
        }
    } else {

        #[proc_macro_derive(TypescriptDefinition, attributes(ts))]
        pub fn derive_typescript_definition(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
            proc_macro::TokenStream::new()
        }
    }
}

/// derive proc_macro to expose Typescript definitions as a static function.
///
/// Please see documentation at [crates.io](https://crates.io/crates/typescript-definitions).
///
cfg_if! {
    if #[cfg(any(debug_assertions, feature = "export-typescript"))] {

        #[proc_macro_derive(TypeScriptify, attributes(ts))]
        pub fn derive_type_script_ify(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
            let input = QuoteT::from(input);
            do_derive_type_script_ify(input).into()

        }
    } else {

        #[proc_macro_derive(TypeScriptify, attributes(ts))]
        pub fn derive_type_script_ify(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
            proc_macro::TokenStream::new()
        }
    }
}

#[allow(unused)]
fn do_derive_typescript_definition(input: QuoteT) -> QuoteT {
    let verify = cfg!(feature = "type-guards");
    let tsy = Typescriptify::new(input);
    let parsed = tsy.parse(verify);
    let export_string = parsed.export_type_definition_source();
    let name = tsy.ident.to_string().to_uppercase();

    let export_ident = ident_from_str(&format!("TS_EXPORT_{}", name));

    let mut q = quote! {

        #[wasm_bindgen(typescript_custom_section)]
        pub const #export_ident : &'static str = #export_string;
    };

    if let Some(ref verify) = tsy.verify_source() {
        let export_ident = ident_from_str(&format!("TS_EXPORT_VERIFY_{}", name));
        q.extend(quote!(
            #[wasm_bindgen(typescript_custom_section)]
            pub const #export_ident : &'static str = #verify;
        ))
    }

    // just to allow testing... only `--features=test` seems to work
    if cfg!(any(test, feature = "test")) {
        let typescript_ident = ident_from_str(&format!("{}___typescript_definition", &tsy.ident));

        q.extend(quote!(
            fn #typescript_ident ( ) -> &'static str {
                #export_string
            }

        ));
    }
    if let Some("1") = option_env!("TFY_SHOW_CODE") {
        eprintln!("{}", patch(&q.to_string()));
    }

    q
}

#[allow(unused)]
fn do_derive_type_script_ify(input: QuoteT) -> QuoteT {
    // panic!("after here");
    let verify = cfg!(feature = "type-guards");
    let tsy = Typescriptify::new(input);
    let parsed = tsy.parse(verify);
    let export_string = parsed.export_type_definition_source();
    let ident = &tsy.ident;

    let (impl_generics, ty_generics, where_clause) = tsy.generics.split_for_impl();

    let type_script_guard = if cfg!(feature = "type-guards") {
        let verifier = match tsy.verify_source() {
            Some(ref txt) => quote!(Some(::std::borrow::Cow::Borrowed(#txt))),
            None => quote!(None),
        };
        quote!(
            fn type_script_guard() -> Option<::std::borrow::Cow<'static,str>> {
                    #verifier
            }
        )
    } else {
        quote!()
    };

    let type_script_enum_factory = if cfg!(feature = "type-enum-factories") {
        let verifier = match tsy.verify_source() {
            Some(ref txt) => quote!(Some(::std::borrow::Cow::Borrowed(#txt))),
            None => quote!(None),
        };
        quote!(
            fn type_script_enum_factory() -> Option<::std::borrow::Cow<'static,str>> {
                    #verifier
            }
        )
    } else {
        quote!()
    };

    let type_script_enum_handlers = if cfg!(feature = "type-enum-handlers") {
        let verifier = match tsy.verify_source() {
            Some(ref txt) => quote!(Some(::std::borrow::Cow::Borrowed(#txt))),
            None => quote!(None),
        };
        quote!(
            fn type_script_enum_handlers() -> Option<::std::borrow::Cow<'static,str>> {
                    #verifier
            }
        )
    } else {
        quote!()
    };
    let ret = quote! {

        impl #impl_generics ::typescript_definitions::TypeScriptifyTrait for #ident #ty_generics #where_clause {
            fn type_script_ify() ->  ::std::borrow::Cow<'static,str> {
                ::std::borrow::Cow::Borrowed(#export_string)
            }
            #type_script_guard
            #type_script_enum_factory
            #type_script_enum_handlers
        }

    };
    if let Some("1") = option_env!("TFY_SHOW_CODE") {
        eprintln!("{}", patch(&ret.to_string()));
    }

    ret
}

/* #endregion helpers */

pub(crate) struct Typescriptify {
    ident: syn::Ident,
    generics: syn::Generics,
    /// quoted identifier { obj }
    arg_name: QuoteT,
    input: DeriveInput,
}

impl Typescriptify {
    pub fn new(input: QuoteT) -> Self {
        let input: DeriveInput = syn::parse2(input).unwrap();

        let cx = Ctxt::new();

        let mut attrs = attrs::Attrs::new();
        attrs.push_doc_comment(&input.attrs);
        attrs.push_attrs(&input.ident, &input.attrs, Some(&cx));

        let container = ast::Container::from_ast(&cx, &input, Derive::Serialize);

        // must track this in case of errors so we can check them
        // if we don't consume the errors, we'll get an "unhandled errors" panic whether or not there were errors
        cx.check().unwrap();

        return Self {
            arg_name: quote!(obj),
            generics: container.generics.clone(),
            ident: container.ident.clone(),
            input,
        };
    }

    pub fn parse_verify(&self) -> Option<VerifySourceResult> {
        let tsout = self.parse(true);
        let statements = tsout.pctxt.extra_guard.borrow();

        let TSOutput { q_maker, .. } = self.parse(true);

        q_maker.verify.map(|verifier| VerifySourceResult {
            body: verifier,
            extra_source: statements
                .iter()
                .map(|extra| {
                    let e = extra.to_string();

                    let extra = patch(&e);
                    "// generic test  \n".to_string() + &extra
                })
                .collect(),
        })
    }

    fn parse(&self, gen_verifier: bool) -> TSOutput {
        let input = &self.input;
        let cx = Ctxt::new();

        // collect and check #[ts(...attrs)]
        let attrs = {
            let mut attrs = attrs::Attrs::new();
            attrs.push_doc_comment(&input.attrs);
            attrs.push_attrs(&input.ident, &input.attrs, Some(&cx));
            attrs
        };

        let container = ast::Container::from_ast(&cx, &input, Derive::Serialize);
        let ts_generics = ts_generics(&container.generics);
        let gv = gen_verifier && attrs.guard;

        let (typescript, pctxt) = {
            let pctxt = ParseContext {
                ctxt: Some(cx),
                arg_name: quote!(obj),
                global_attrs: attrs,
                gen_guard: gv,
                ident: container.ident.clone(),
                ts_generics,
                extra_guard: RefCell::new(vec![]),
            };

            let typescript = match container.data {
                ast::Data::Enum(ref variants) => pctxt.derive_enum(variants, &container),
                ast::Data::Struct(style, ref fields) => {
                    pctxt.derive_struct(style, fields, &container)
                }
            };

            // erase serde context
            (typescript, pctxt)
        };

        TSOutput {
            ident: patch(&container.ident.clone().to_string()).into(),
            pctxt: pctxt,
            q_maker: typescript,
        }
    }
}

struct VerifySourceResult {
    body: QuoteT,
    extra_source: Vec<String>,
}

impl Typescriptify {
    /* #region verify-and-ts-help */

    fn verify_source(&self) -> Option<String> {
        match self.parse_verify() {
            None => None,
            Some(ref result) => {
                let mut s = {
                    let ident = &self.ident;
                    let obj = &self.arg_name;
                    let body = result.body.to_string();
                    let body = patch(&body);

                    let generics = self.ts_generics(false);
                    let generics_wb = &generics; // self.ts_generics(true);
                    let is_generic = !ts_generics(&self.generics).is_empty();
                    let name = guard_name(&ident);
                    if is_generic {
                        format!(
                            "export const {name} = {generics_wb}({obj}: any, typename: string): \
                                 {obj} is {ident}{generics} => {body}",
                            name = name,
                            obj = obj,
                            body = body,
                            generics = generics,
                            generics_wb = generics_wb,
                            ident = ident
                        )
                    } else {
                        format!(
                            "export const {name} = {generics_wb}({obj}: any): \
                                 {obj} is {ident}{generics} => {body}",
                            name = name,
                            obj = obj,
                            body = body,
                            generics = generics,
                            generics_wb = generics_wb,
                            ident = ident
                        )
                    }
                };
                for txt in &result.extra_source {
                    s.push('\n');
                    s.push_str(&txt);
                }
                Some(s)
            }
        }
    }

    fn ts_generics(&self, with_bound: bool) -> QuoteT {
        let args_wo_lt = self.ts_generic_args_wo_lifetimes(with_bound);
        if args_wo_lt.is_empty() {
            quote!()
        } else {
            quote!(<#(#args_wo_lt),*>)
        }
    }

    fn ts_generic_args_wo_lifetimes(&self, with_bounds: bool) -> Vec<QuoteT> {
        ts_generics(&self.generics)
            .iter()
            .filter_map(move |g| match g {
                Some((ref ident, ref bounds)) => {
                    // we ignore trait bounds for typescript
                    if bounds.is_empty() || !with_bounds {
                        Some(quote! (#ident))
                    } else {
                        let bounds = bounds.iter().map(|ts| &ts.ident);
                        Some(quote! { #ident extends #(#bounds)&* })
                    }
                }

                None => None,
            })
            .collect()
    }
    /* #endregion verify-and-ts-help */
}

struct TSOutput {
    ident: String,
    pctxt: ParseContext,
    q_maker: QuoteMaker,
}

impl TSOutput {
    fn export_type_handler_source(&self) -> Option<String> {
        if let QuoteMakerKind::Enum = self.q_maker.kind {
            Some(format!(
                "{}export enum {} {};",
                self.pctxt.global_attrs.to_comment_str(),
                self.ident,
                patch(&self.q_maker.source.to_string())
            ))
        } else {
            None
        }
    }

    fn export_type_definition_source(&self) -> String {
        if let QuoteMakerKind::Enum = self.q_maker.kind {
            format!(
                "{}export enum {} {};",
                self.pctxt.global_attrs.to_comment_str(),
                self.ident,
                patch(&self.q_maker.source.to_string())
            )
        } else {
            format!(
                "{}export type {} = {};",
                self.pctxt.global_attrs.to_comment_str(),
                self.ident,
                patch(&self.q_maker.source.to_string())
            )
        }
    }
}

fn ts_generics(g: &syn::Generics) -> Vec<Option<(Ident, Bounds)>> {
    // lifetime params are represented by None since we are only going
    // to translate them to '_

    // impl#generics TypeScriptTrait for A<... lifetimes to '_ and T without bounds>

    use syn::{GenericParam, TypeParamBound};
    g.params
        .iter()
        .map(|p| match p {
            GenericParam::Lifetime(..) => None,
            GenericParam::Type(ref ty) => {
                let bounds = ty
                    .bounds
                    .iter()
                    .filter_map(|b| match b {
                        TypeParamBound::Trait(t) => Some(&t.path),
                        _ => None, // skip lifetimes for bounds
                    })
                    .map(last_path_element)
                    .filter_map(|b| b)
                    .collect::<Vec<_>>();

                Some((ty.ident.clone(), bounds))
            }
            GenericParam::Const(ref param) => {
                let ty = TSType {
                    ident: param.ident.clone(),
                    path: vec![],
                    args: vec![param.ty.clone()],
                    return_type: None,
                };
                Some((param.ident.clone(), vec![ty]))
            }
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
    path: Vec<syn::Ident>,          // full path
    return_type: Option<syn::Type>, // only if function
}

impl TSType {
    fn path(&self) -> Vec<String> {
        self.path.iter().map(|i| i.to_string()).collect() // hold the memory
    }
}

fn last_path_element(path: &syn::Path) -> Option<TSType> {
    let fullpath = path
        .segments
        .iter()
        .map(|s| s.ident.clone())
        .collect::<Vec<_>>();
    match path.segments.last().map(|p| p.into_value()) {
        Some(t) => {
            let ident = t.ident.clone();
            let args = match &t.arguments {
                syn::PathArguments::AngleBracketed(ref path) => &path.args,
                // closures Fn(A,B) -> C
                syn::PathArguments::Parenthesized(ref path) => {
                    let args: Vec<_> = path.inputs.iter().cloned().collect();
                    let ret = return_type(&path.output);
                    return Some(TSType {
                        ident,
                        args,
                        path: fullpath,
                        return_type: ret,
                    });
                }
                syn::PathArguments::None => {
                    return Some(TSType {
                        ident,
                        args: vec![],
                        path: fullpath,
                        return_type: None,
                    });
                }
            };
            // ignore lifetimes
            let args = args
                .iter()
                .filter_map(|p| match p {
                    syn::GenericArgument::Type(t) => Some(t),
                    syn::GenericArgument::Binding(t) => Some(&t.ty),
                    syn::GenericArgument::Constraint(..) => None,
                    syn::GenericArgument::Const(..) => None,
                    _ => None, // lifetimes, expr, constraints A : B ... skip!
                })
                .cloned()
                .collect::<Vec<_>>();

            Some(TSType {
                ident,
                path: fullpath,
                args,
                return_type: None,
            })
        }
        None => None,
    }
}

pub(crate) struct FieldContext<'a> {
    pub ctxt: &'a ParseContext,    // global parse context
    pub field: &'a ast::Field<'a>, // field being parsed
    pub attrs: Attrs,              // field attributes
}

impl<'a> FieldContext<'a> {
    pub fn get_path(&self, ty: &syn::Type) -> Option<TSType> {
        use syn::Type::Path;
        use syn::TypePath;
        match ty {
            Path(TypePath { path, .. }) => last_path_element(&path),
            _ => None,
        }
    }
}

pub(crate) struct ParseContext {
    ctxt: Option<Ctxt>,  // serde parse context for error reporting
    arg_name: QuoteT,    // top level "name" of argument for verifier
    global_attrs: Attrs, // global #[ts(...)] attributes
    gen_guard: bool,     // generate type guard for this struct/enum
    ident: syn::Ident,   // name of enum struct
    ts_generics: Vec<Option<(Ident, Bounds)>>, // None means a lifetime parameter
    extra_guard: RefCell<Vec<QuoteT>>, // for generic verifier hack!
}

impl Drop for ParseContext {
    fn drop(&mut self) {
        if !std::thread::panicking() {
            // must track this in case of errors so we can check them
            // if we don't consume the errors, we'll get an "unhandled errors" panic whether or not there were errors
            if let Some(ctxt) = self.ctxt.take() {
                ctxt.check().expect("no errors")
            }
        }
    }
}

impl<'a> ParseContext {
    // Some helpers

    fn err_msg(&self, msg: &str) {
        if let Some(ref ctxt) = self.ctxt {
            ctxt.error(msg);
        } else {
            panic!(msg.to_string())
        }
    }

    /// returns { #ty } of
    fn field_to_ts(&self, field: &ast::Field<'a>) -> QuoteT {
        let attrs = Attrs::from_field(field, self.ctxt.as_ref());
        // if user has provided a type ... use that
        if attrs.ts_type.is_some() {
            use std::str::FromStr;
            let s = attrs.ts_type.unwrap();
            return match QuoteT::from_str(&s) {
                Ok(tokens) => tokens,
                Err(..) => {
                    self.err_msg(&format!("{}: can't parse type {}", self.ident, s));
                    quote!()
                }
            };
        }

        let fc = FieldContext {
            attrs,
            ctxt: &self,
            field,
        };
        if let Some(ref ty) = fc.attrs.ts_as {
            fc.type_to_ts(ty)
        } else {
            fc.type_to_ts(&field.ty)
        }
    }

    /// returns { #field_name: #ty }
    fn derive_field(&self, field: &ast::Field<'a>) -> QuoteT {
        let field_name = field.attrs.name().serialize_name(); // use serde name instead of field.member
        let field_name = ident_from_str(&field_name);

        let ty = self.field_to_ts(&field);

        quote! {
            #field_name: #ty
        }
    }
    fn derive_fields(
        &'a self,
        fields: &'a [&'a ast::Field<'a>],
    ) -> impl Iterator<Item = QuoteT> + 'a {
        fields.iter().map(move |f| self.derive_field(f))
    }
    fn derive_field_tuple(
        &'a self,
        fields: &'a [&'a ast::Field<'a>],
    ) -> impl Iterator<Item = QuoteT> + 'a {
        fields.iter().map(move |f| self.field_to_ts(f))
    }

    fn check_flatten(&self, fields: &[&'a ast::Field<'a>], ast_container: &ast::Container) -> bool {
        let has_flatten = fields.iter().any(|f| f.attrs.flatten()); // .any(|f| f);
        if has_flatten {
            self.err_msg(&format!(
                "{}: #[serde(flatten)] does not work for typescript-definitions.",
                ast_container.ident
            ));
        };
        has_flatten
    }
}
