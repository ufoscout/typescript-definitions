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
use quote::quote;
use serde_derive_internals::{ast, Ctxt, Derive};
use syn::DeriveInput;

mod attrs;
mod derive_enum;
mod derive_struct;
mod patch;
mod tests;
mod tots;
mod utils;

use attrs::Attrs;
use utils::*;

use patch::patch;

// too many TokenStreams around! give it a different name
type QuoteT = proc_macro2::TokenStream;

struct QuoteMaker {
    pub source: QuoteT,
    /// enum factory quote token stream
    pub enum_factory: Result<QuoteT, &'static str>,
    /// enum handler quote token stream
    pub enum_handler: Result<QuoteT, &'static str>,
    pub kind: QuoteMakerKind,
}

enum QuoteMakerKind {
    Object,
    Enum,
    Union,
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

// derive proc_macro to expose Typescript definitions to `wasm-bindgen`.
//
// Please see documentation at [crates.io](https://crates.io/crates/typescript-definitions).
cfg_if! {
    if #[cfg(any(debug_assertions, feature = "export-typescript"))] {

        #[proc_macro_derive(TypeScriptDefinition, attributes(ts))]
        pub fn derive_typescript_definition(input: proc_macro::TokenStream) -> proc_macro::TokenStream {

            if !(is_wasm32() || cfg!(feature="test")) {
                return proc_macro::TokenStream::new();
            }

            let input = QuoteT::from(input);
            do_derive_typescript_definition(input).into()
        }
    } else {

        #[proc_macro_derive(TypeScriptDefinition, attributes(ts))]
        pub fn derive_typescript_definition(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
            proc_macro::TokenStream::new()
        }
    }
}

// derive proc_macro to expose Typescript definitions as a static function.
//
// Please see documentation at [crates.io](https://crates.io/crates/typescript-definitions).
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
    let tsy = Typescriptify::new(input);
    let parsed = tsy.parse();
    let export_source = parsed.export_type_definition_source();
    let export_string = format!(
        // we're still going to include the values so we can separate them out in an additional step for webpack
        // the alternative to this seems like it might be to fork wasm-bindgen... which I don't want to do.
        "{}\ntype __StartValuesFor__{}__ = `\n{}\n`/*EndValuesFor__{}__*/",
        export_source.declarations,
        parsed.ident.as_str(),
        export_source.values,
        parsed.ident.as_str()
    );
    let name = tsy.ident.to_string().to_uppercase();

    let export_ident = ident_from_str(&format!("TS_EXPORT_{}", name));

    let mut q = quote! {
        #[wasm_bindgen(typescript_custom_section)]
        pub const #export_ident : &'static str = #export_string;
    };

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
    let tsy = Typescriptify::new(input);
    let parsed = tsy.parse();
    let export_source = parsed.export_type_definition_source();
    let export_string = format!("{}\n{}", export_source.declarations, export_source.values);
    let ident = &tsy.ident;

    let (impl_generics, ty_generics, where_clause) = tsy.generics.split_for_impl();

    let type_script_enum_factory = if cfg!(feature = "type-enum-factories") {
        let factory = match parsed.export_type_factory_source() {
            Ok(ref txt) => quote!(Ok(::std::borrow::Cow::Borrowed(#txt))),
            Err(err_msg) => quote!(Err(#err_msg)),
        };
        quote!(
            fn type_script_enum_factory() -> Result<::std::borrow::Cow<'static,str>, &'static str> {
                    #factory
            }
        )
    } else {
        quote!()
    };

    let type_script_enum_handlers = if cfg!(feature = "type-enum-handlers") {
        let handlers = match parsed.export_type_handler_source() {
            Ok(ref txt) => quote!(Ok(::std::borrow::Cow::Borrowed(#txt))),
            Err(err_msg) => quote!(Err(#err_msg)),
        };
        quote!(
            fn type_script_enum_handlers() -> Result<::std::borrow::Cow<'static,str>, &'static str> {
                    #handlers
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
    input: DeriveInput,
}

impl Typescriptify {
    pub fn new(input: QuoteT) -> Self {
        let input: DeriveInput = syn::parse2(input).unwrap();

        let cx = Ctxt::new();

        let mut attrs = attrs::Attrs::new();
        attrs.push_doc_comment(&input.attrs);
        attrs.push_attrs(&input.ident, &input.attrs, Some(&cx));

        let container = ast::Container::from_ast(&cx, &input, Derive::Serialize).expect("container was derived from AST");

        // must track this in case of errors so we can check them
        // if we don't consume the errors, we'll get an "unhandled errors" panic whether or not there were errors
        cx.check().unwrap();

        Self {
            generics: container.generics.clone(),
            ident: container.ident.clone(),
            input,
        }
    }

    fn parse(&self) -> TSOutput {
        let input = &self.input;
        let cx = Ctxt::new();

        // collect and check #[ts(...attrs)]
        let attrs = {
            let mut attrs = attrs::Attrs::new();
            attrs.push_doc_comment(&input.attrs);
            attrs.push_attrs(&input.ident, &input.attrs, Some(&cx));
            attrs
        };

        let container = ast::Container::from_ast(&cx, &input, Derive::Serialize).expect("container was derived from AST");

        let (typescript, pctxt) = {
            let pctxt = ParseContext {
                ctxt: Some(cx),
                global_attrs: attrs,
                ident: container.ident.clone(),
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
            pctxt,
            q_maker: typescript,
        }
    }
}

struct TSOutput {
    ident: String,
    pctxt: ParseContext,
    q_maker: QuoteMaker,
}

/// We have multiple kinds of exports that we need to differentiate between when using something like
/// WASM Bindgen. For WASM-Bindgen, we have types needed for inputs (in the index.d.ts file), but we
/// also want to provide helper values which cannot exist in the .d.ts file. For this, we have to separate
/// what are simply type declarations, and what are helper values (functions, etc)
struct TSDefinitions {
    /// export type A = [number];
    pub declarations: String,
    /// export const a: A = [1];
    pub values: String,
}

impl TSOutput {
    fn export_type_handler_source(&self) -> Result<String, &'static str> {
        self.q_maker
            .enum_handler
            .as_ref()
            .map(|content| {
                format!(
                    "{}{}",
                    self.pctxt.global_attrs.to_comment_str(),
                    patch(&content.to_string())
                )
            })
            .map_err(|e| *e)
    }

    fn export_type_factory_source(&self) -> Result<String, &'static str> {
        self.q_maker
            .enum_factory
            .as_ref()
            .map(|content| {
                format!(
                    "{}{}",
                    self.pctxt.global_attrs.to_comment_str(),
                    patch(&content.to_string())
                )
            })
            .map_err(|e| *e)
    }

    fn export_type_definition_source(&self) -> TSDefinitions {
        match self.q_maker.kind {
            QuoteMakerKind::Enum => TSDefinitions {
                declarations: format!(
                    "{}export enum {} {}",
                    self.pctxt.global_attrs.to_comment_str(),
                    self.ident,
                    patch(&self.q_maker.source.to_string())
                ),
                values: format!(
                    "{}export enum {} {}",
                    self.pctxt.global_attrs.to_comment_str(),
                    self.ident,
                    patch(&self.q_maker.source.to_string())
                ),
            },
            QuoteMakerKind::Union => TSDefinitions {
                declarations: format!(
                    "{}export type {} = {}",
                    self.pctxt.global_attrs.to_comment_str(),
                    self.ident,
                    patch(&self.q_maker.source.to_string()),
                ),
                values: format!(
                    "{}\n{}",
                    self.export_type_factory_source()
                        .expect("factory exists for union"),
                    self.export_type_handler_source()
                        .expect("handler exists for union"),
                ),
            },
            QuoteMakerKind::Object => TSDefinitions {
                declarations: format!(
                    "{}export type {} = {}",
                    self.pctxt.global_attrs.to_comment_str(),
                    self.ident,
                    patch(&self.q_maker.source.to_string()),
                ),
                values: format!(
                    "{}export const {} = (check: {}) => check\n",
                    // check create function
                    self.pctxt.global_attrs.to_comment_str(),
                    self.ident,
                    self.ident,
                ),
            },
        }
    }
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
    match path.segments.last() {
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
    global_attrs: Attrs, // global #[ts(...)] attributes
    ident: syn::Ident,   // name of enum struct
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

    fn err_msg<A: quote::ToTokens>(&self, tokens: A, msg: &str) {
        if let Some(ref ctxt) = self.ctxt {
            ctxt.error_spanned_by(tokens, msg);
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
                    self.err_msg(field.original, &format!("{}: can't parse type {}", self.ident, s));
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
            self.err_msg(&self.ident, &format!(
                "{}: #[serde(flatten)] does not work for typescript-definitions.",
                ast_container.ident
            ));
        };
        has_flatten
    }
}
