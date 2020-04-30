// Copyright 2019 Ian Castleden
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
use super::patch::{eq, nl};
use super::QuoteT;
use super::{filter_visible, ident_from_str, ParseContext, QuoteMaker, QuoteMakerKind};
use crate::patch::tsignore;
use proc_macro2::Literal;
use quote::quote;
use serde_derive_internals::{ast, ast::Variant, attr::EnumTag};
const CONTENT: &str = "fields"; // default content tag
                                // const TAG: &'static str = "kind"; // default tag tag
struct TagInfo<'a> {
    /// #[serde(tag = "...")]
    tag: Option<&'a str>,
    /// #[serde(content = "...")]
    content: Option<&'a str>,
    /// flattened without tag `{ "key1": "", "key2": "" }`
    untagged: bool,
    /// externally tagged
    externally_tagged: bool,
}
impl<'a> TagInfo<'a> {
    fn from_enum(e: &'a EnumTag) -> Self {
        match e {
            EnumTag::Internal { tag, .. } => TagInfo {
                tag: Some(tag),
                content: None,
                untagged: false,
                externally_tagged: false,
            },
            EnumTag::Adjacent { tag, content, .. } => TagInfo {
                tag: Some(tag),
                content: Some(&content),
                untagged: false,
                externally_tagged: false,
            },
            EnumTag::External => TagInfo {
                tag: None,
                content: None,
                untagged: false,
                externally_tagged: true,
            },
            EnumTag::None => TagInfo {
                tag: None,
                content: None,
                untagged: true,
                externally_tagged: false,
            },
        }
    }
}

struct HandlerSource {
    /// None indicates Unit
    input_type: Option<QuoteT>,
    /// None indicates no input should be passed
    input_post_accessor: Option<QuoteT>,
    /// get the variant type
    input_tag_accessor: QuoteT,
}

struct VariantQuoteMaker {
    /// message type possibly including tag key value
    pub source: QuoteT,
    /// type guard quote token stream
    pub verify: Option<QuoteT>,
    /// enum factory quote token stream
    // pub enum_factory: Result<QuoteT, &'static str>,
    /// inner type token stream
    pub inner_type: Option<QuoteT>,
}

impl<'a> ParseContext {
    pub(crate) fn derive_enum(
        &self,
        variants: &[ast::Variant<'a>],
        ast_container: &ast::Container,
    ) -> QuoteMaker {
        // https://serde.rs/enum-representations.html
        let taginfo = TagInfo::from_enum(ast_container.attrs.tag());

        // remove skipped ( check for #[serde(skip)] )
        let variants: Vec<&ast::Variant<'a>> = variants
            .into_iter()
            .filter(|v| !v.attrs.skip_serializing())
            .collect();

        // is typescript enum compatible
        let is_enum = variants.iter().all(|v| matches!(v.style, ast::Style::Unit));

        if is_enum {
            let v = &variants
                .into_iter()
                .map(|v| v.attrs.name().serialize_name()) // use serde name instead of v.ident
                .collect::<Vec<_>>();

            let k = v.iter().map(|v| ident_from_str(&v)).collect::<Vec<_>>();
            let verify = if self.gen_guard {
                let obj = &self.arg_name;
                let o = (0..v.len()).map(|_| obj.clone());
                let eq = (0..v.len()).map(|_| eq());

                Some(quote!(
                    {

                        if (!((#(#o #eq #v)||*))) return false;
                        return true;
                    }
                ))
            } else {
                None
            };

            return QuoteMaker {
                source: quote! ( { #(#k = #v),* } ),
                verify,
                enum_factory: Err("factory cannot be created with raw enum type"),
                enum_handler: Err("handler cannot be created with raw enum type"),
                kind: QuoteMakerKind::Enum,
            };
        }

        let content: Vec<(&Variant, VariantQuoteMaker)> = variants
            .iter()
            .map(|variant| {
                (
                    *variant,
                    match variant.style {
                        ast::Style::Struct => self.derive_struct_variant(
                            &taginfo,
                            variant,
                            &variant.fields,
                            ast_container,
                        ),
                        ast::Style::Newtype => {
                            self.derive_newtype_variant(&taginfo, variant, &variant.fields[0])
                        }
                        ast::Style::Tuple => {
                            self.derive_tuple_variant(&taginfo, variant, &variant.fields)
                        }
                        ast::Style::Unit => self.derive_unit_variant(&taginfo, variant),
                    },
                )
            })
            .collect::<Vec<_>>();

        // OK generate A | B | C etc
        let newl = nl();
        let tsignore = tsignore();
        let body = content.iter().map(|(_, q)| q.source.clone());
        let verify = if self.gen_guard {
            let v = content.iter().map(|(_, q)| q.verify.clone().unwrap());
            let newls = std::iter::repeat(quote!(#newl));

            let obj = &self.arg_name;
            // let newls = std::iter::from_fn(|| Some(quote!(#newl)));
            // obj can't be null or undefined
            Some(quote!(
                {
                    if (#obj == undefined) return false;

                    #( #newls if ( ( () => #v )() ) return true; )*
                    #newl return false;
                }
            ))
        } else {
            None
        };

        let enum_factory = taginfo
            .tag
            .as_ref()
            .ok_or("serde tag must be specified to create enum factory")
            .and_then(|tag_key| -> Result<QuoteT, &'static str> {
                let args = content.iter().map(|(_, q)| {
                    q.inner_type
                        .as_ref()
                        .map(|inner_type| quote!(content: #inner_type))
                        .unwrap_or(quote!())
                });
                let has_args = content.iter().map(|(_, q)| q.inner_type.is_some());
                let ret_constructs = content.iter().zip(has_args).map(
                    |((v, _), has_args): (&(&Variant, VariantQuoteMaker), bool)| {
                        let tag_name_str = Literal::string(&v.ident.to_string());
                        let tag_key_str = Literal::string(tag_key);
                        if has_args {
                            taginfo
                                .content
                                .map(|content_key| {
                                    let content_key_str = Literal::string(content_key);
                                    quote!({ #tag_key_str: #tag_name_str, #content_key_str: content })
                                })
                                .unwrap_or(quote!({ #tag_key: #tag_name_str, ...content }))
                        } else {
                            quote!({ #tag_key_str: #tag_name_str })
                        }
                    },
                );
                let tag_name = variants.iter().map(|v| v.ident.clone());
                // let tag_key_dq_1 = Literal::string(tag_key);
                // let ret_type = std::iter::repeat(ret_type_1.clone());

                let newls = std::iter::repeat(quote!(#newl));

                let type_ident_str = super::patch(&self.ident.to_string()).to_string();
                let type_ident_1 = ident_from_str(&type_ident_str);
                // let type_ident = std::iter::repeat(type_ident_1);
                let export_factory_ident_1 = ident_from_str(
                    self.global_attrs
                        .ts_factory_name
                        .as_ref()
                        // default naming
                        .unwrap_or(&format!("{}Factory", &type_ident_str)),
                );
                let export_factory_type_ident_1 = ident_from_str(
                    &format!("{}ReturnType", &export_factory_ident_1)
                );


                let args_copy = args.clone();
                let tag_name_copy = tag_name.clone();
                let newls_copy = newls.clone();
                Ok(
                    quote!(export const #export_factory_ident_1 = (fn: (message: #type_ident_1) => any): #export_factory_type_ident_1 => Object.freeze({
                            #( #newls  #tag_name(#args): void {
                                return fn(#ret_constructs)
                            },)*#newl
                        });#newl
                        export type #export_factory_type_ident_1 = {
                            #( #newls_copy  #tag_name_copy(#args_copy): void;)*#newl
                        };#newl
                    ),
                )
            });

        let enum_handler = taginfo
            .tag
            .as_ref()
            .ok_or("serde tag must be specified to create enum handler")
            .and_then(|tag_key| -> Result<QuoteT, &'static str> {
                let args = content.iter().map(|(_, q)|
                    q.inner_type
                    .as_ref()
                    .map(|inner_type|
                        quote!(message: #inner_type)
                    ).unwrap_or(
                        quote!()
                    ));
                let on_tag_name = variants.iter().map(|v| ident_from_str(&format!("on{}",(v.ident.to_string()))));
                let tag_key_dq_1 = Literal::string(tag_key);
                let ret_type_1 = ident_from_str(
                    self.global_attrs
                        .ts_handler_return
                        .as_ref()
                        // default return type to any
                        .unwrap_or(&String::from("any")),
                );
                let ret_type = std::iter::repeat(ret_type_1.clone());

                let newls = std::iter::repeat(quote!(#newl));
                let handle_prefix_dq_1 = Literal::string("on");

                let type_ident = super::patch(&self.ident.to_string()).to_string();
                let export_interface_1 = ident_from_str(
                    self.global_attrs
                    .ts_handler_name
                    .as_ref()
                    // default naming
                    .unwrap_or(&format!("Handle{}", &type_ident)));

                let ident_1 = ident_from_str(&type_ident);
                let apply_ident_1 = ident_from_str(&format!("apply{}", &type_ident));
                let access_input_content_1 = taginfo.content.map(|content_key| quote!(input[#content_key])).unwrap_or(quote!(input));

                Ok(quote!(export interface #export_interface_1 {
                        #( #newls  #on_tag_name(#args): #ret_type;)*#newl
                    }#newl
                    export function #apply_ident_1(handler: #export_interface_1): (input: #ident_1) => #ret_type_1 {#newl
                        #tsignore
                        return input => handler[#handle_prefix_dq_1 + input[#tag_key_dq_1]](#access_input_content_1);#newl
                    }#newl
                ))
            });

        let newls = std::iter::repeat(quote!(#newl));
        QuoteMaker {
            source: quote! ( #( #newls | #body)* ),
            verify,
            enum_factory,
            enum_handler,
            kind: QuoteMakerKind::Union,
        }
    }

    /// Depends on TagInfo for layout
    fn derive_unit_variant(&self, taginfo: &TagInfo, variant: &Variant) -> VariantQuoteMaker {
        let variant_name = variant.attrs.name().serialize_name(); // use serde name instead of variant.ident
        let eq = eq();

        if taginfo.tag.is_none() {
            let verify = if self.gen_guard {
                let obj = &self.arg_name;
                Some(quote!(
                    {
                        return #obj #eq #variant_name;
                    }
                ))
            } else {
                None
            };
            return VariantQuoteMaker {
                source: quote!(#variant_name),
                verify,
                inner_type: None,
            };
        }
        let tag = ident_from_str(taginfo.tag.unwrap());
        let verify = if self.gen_guard {
            let obj = &self.arg_name;
            Some(quote!(
                {
                    return #obj.#tag #eq #variant_name;
                }
            ))
        } else {
            None
        };
        VariantQuoteMaker {
            source: quote! (
                { #tag: #variant_name }
            ),
            verify,
            inner_type: None,
        }
    }

    /// Depends on TagInfo for layout
    /// example variant: `C(u32)`
    fn derive_newtype_variant(
        &self,
        taginfo: &TagInfo,
        variant: &Variant,
        field: &ast::Field<'a>,
    ) -> VariantQuoteMaker {
        if field.attrs.skip_serializing() {
            return self.derive_unit_variant(taginfo, variant);
        };
        let ty = self.field_to_ts(field);
        let variant_name = self.variant_name(variant);
        let obj = &self.arg_name;

        if taginfo.tag.is_none() {
            if taginfo.untagged {
                let verify = if self.gen_guard {
                    let v = self.verify_type(&obj, field);

                    Some(quote!( { #v; return true }))
                } else {
                    None
                };
                return VariantQuoteMaker {
                    source: quote! ( #ty ),
                    verify,
                    inner_type: Some(ty.clone()),
                };
            };
            let tag = ident_from_str(&variant_name);

            let verify = if self.gen_guard {
                let v = quote!(v);
                let verify = self.verify_type(&v, field);
                let eq = eq();
                // #ty might be a Option None and therefore null
                // OTOH #verify might be assuming not null and not undefined
                Some(quote!(
                    {
                        const v = #obj.#tag;
                        if (v #eq undefined) return false;
                        #verify;
                        return true;
                    }
                ))
            } else {
                None
            };
            return VariantQuoteMaker {
                source: quote! (
                    { #tag : #ty }

                ),
                verify,
                inner_type: Some(ty.clone()),
            };
        };
        let tag = ident_from_str(taginfo.tag.unwrap());

        let content = if let Some(content) = taginfo.content {
            ident_from_str(&content)
        } else {
            ident_from_str(CONTENT) // should not get here...
        };

        let verify = if self.gen_guard {
            let eq = eq();
            let verify = self.verify_type(&quote!(val), field);
            Some(quote!(
            {
                if (!(#obj.#tag #eq #variant_name)) return false;
                const val = #obj.#content;
                if (val #eq undefined) return false;
                #verify;
                return true;
            }))
        } else {
            None
        };

        VariantQuoteMaker {
            source: quote! (
                { #tag: #variant_name; #content: #ty }
            ),
            verify,
            inner_type: Some(ty.clone()),
        }
    }

    /// Depends on TagInfo for layout
    /// `C { a: u32, b: u32 }` => `C: { a: number, b: number }`
    fn derive_struct_variant(
        &self,
        taginfo: &TagInfo,
        variant: &Variant,
        fields: &[ast::Field<'a>],
        ast_container: &ast::Container,
    ) -> VariantQuoteMaker {
        use std::collections::HashSet;
        let fields = filter_visible(fields);
        if fields.is_empty() {
            return self.derive_unit_variant(taginfo, variant);
        }

        self.check_flatten(&fields, ast_container);

        let contents = self.derive_fields(&fields).collect::<Vec<_>>();
        let variant_name = self.variant_name(variant);

        let ty_inner = quote!(#(#contents);*);
        let ty = quote! (
            { #ty_inner }
        );

        let last = nl();
        let nl = std::iter::repeat(quote!(#last));
        if taginfo.tag.is_none() {
            if taginfo.untagged {
                let verify = if self.gen_guard {
                    let verify = self.verify_fields(&self.arg_name, &fields);

                    Some(quote!(
                        {
                            #( #nl #verify;)*
                            #last return true;
                        }
                    ))
                } else {
                    None
                };
                return VariantQuoteMaker {
                    source: quote!(#ty),
                    verify,
                    inner_type: Some(ty.clone()),
                };
            };
            let v = &quote!(v);
            let tag = ident_from_str(&variant_name);
            let verify = if self.gen_guard {
                let obj = &self.arg_name;
                let verify = self.verify_fields(&v, &fields);
                Some(quote!(
                    {
                        const v = #obj.#tag;
                        if (v == undefined) return false;
                        #(#nl #verify;)*
                        #last return true;
                    }
                ))
            } else {
                None
            };
            return VariantQuoteMaker {
                source: quote! (
                    { #tag : #ty  }
                ),
                verify,
                inner_type: Some(ty.clone()),
            };
        }
        let tag_str = taginfo.tag.unwrap();
        let tag = ident_from_str(tag_str);

        if let Some(content) = taginfo.content {
            let content = ident_from_str(&content);

            let verify = if self.gen_guard {
                let obj = &self.arg_name;
                let v = quote!(v);
                let verify = self.verify_fields(&v, &fields);
                let eq = eq();
                Some(quote!(
                {
                    if (!(#obj.#tag #eq #variant_name)) return false;
                    const v = #obj.#content;
                    if (v == undefined) return false;
                    #(#nl #verify;)*
                    #last return true;
                }
                ))
            } else {
                None
            };
            VariantQuoteMaker {
                source: quote! (
                    { #tag: #variant_name; #content: #ty }
                ),
                verify,
                inner_type: Some(ty.clone()),
            }
        } else {
            if let Some(ref cx) = self.ctxt {
                let fnames = fields
                    .iter()
                    .map(|field| field.attrs.name().serialize_name())
                    .collect::<HashSet<_>>();
                if fnames.contains(tag_str) {
                    cx.error(format!(
                        "clash with field in \"{}::{}\". \
                         Maybe use a #[serde(content=\"...\")] attribute.",
                        ast_container.ident, variant_name
                    ));
                }
            };
            let verify = if self.gen_guard {
                let obj = &self.arg_name;
                let verify = self.verify_fields(&obj, &fields);
                let eq = eq();
                Some(quote!(
                {
                    if (!(#obj.#tag #eq #variant_name)) return false;
                    #(#nl #verify;)*
                    #last return true;
                }
                ))
            } else {
                None
            };
            // spread together tagged no content
            VariantQuoteMaker {
                source: quote! (
                    { #tag: #variant_name; #ty_inner }
                ),
                verify,
                inner_type: Some(ty.clone()),
            }
        }
    }

    #[inline]
    fn variant_name(&self, variant: &Variant) -> String {
        variant.attrs.name().serialize_name() // use serde name instead of variant.ident
    }

    /// `B(u32, u32)` => `B: [number, number]`
    fn derive_tuple_variant(
        &self,
        taginfo: &TagInfo,
        variant: &Variant,
        fields: &[ast::Field<'a>],
    ) -> VariantQuoteMaker {
        let variant_name = self.variant_name(variant);
        let fields = filter_visible(fields);
        let contents = self.derive_field_tuple(&fields);
        let ty = quote!([ #(#contents),* ]);

        if taginfo.tag.is_none() {
            if taginfo.untagged {
                let verify = if self.gen_guard {
                    let obj = &self.arg_name;
                    let verify = self.verify_field_tuple(&obj, &fields);
                    let eq = eq();
                    let len = Literal::usize_unsuffixed(fields.len());

                    Some(quote!({
                        if (!Array.isArray(#obj) || !(#obj.length #eq #len)) return false;
                        #(#verify;)*
                        return true;
                    }))
                } else {
                    None
                };
                return VariantQuoteMaker {
                    source: quote! (#ty),
                    verify,
                    inner_type: Some(ty.clone()),
                };
            }
            let tag = ident_from_str(&variant_name);
            let verify = if self.gen_guard {
                let obj = &self.arg_name;
                let v = quote!(v);
                let verify = self.verify_field_tuple(&v, &fields);
                let len = Literal::usize_unsuffixed(fields.len());
                let eq = eq();
                Some(quote!({
                    const v = #obj.#tag;
                    if (!Array.isArray(v) || !(v.length #eq #len)) return false;
                    #(#verify;)*
                    return true;
                }))
            } else {
                None
            };
            return VariantQuoteMaker {
                source: quote! ({ #tag : #ty }),
                verify,
                inner_type: Some(ty.clone()),
            };
        };

        let tag = ident_from_str(taginfo.tag.unwrap());
        let content = if let Some(content) = taginfo.content {
            ident_from_str(&content)
        } else {
            ident_from_str(CONTENT)
        };

        let verify = if self.gen_guard {
            let eq = eq();
            let obj = &self.arg_name;
            let v = quote!(v);
            let verify = self.verify_field_tuple(&v, &fields);
            let len = Literal::usize_unsuffixed(fields.len());
            Some(quote!({
                if (!(#obj.#tag #eq #variant_name)) return false;
                const v = #obj.#content;
                if (!Array.isArray(v) || !(v.length #eq #len)) return false;
                #(#verify;)*
                return true;
            }))
        } else {
            None
        };
        VariantQuoteMaker {
            source: quote! (
            { #tag: #variant_name; #content : #ty }
            ),
            verify,
            inner_type: Some(ty.clone()),
        }
    }
}

fn get_enum_handler(
    taginfo: &TagInfo,
    variant_name: &str,
    tag_key: &str,
    ty: Option<&QuoteT>,
) -> HandlerSource {
    // default external tagging { "Variant1" : { "a": 1 } }
    HandlerSource {
        input_type: ty.map(|typ| quote! ( #typ )),
        input_tag_accessor: quote!([#tag_key]),
        input_post_accessor: {
            // unit type should be None
            ty.map(|_| {
                if taginfo.untagged {
                    // content mixed in with type together
                    quote!()
                } else if taginfo.externally_tagged {
                    // located at
                    quote!([#variant_name])
                } else {
                    match taginfo.content {
                        // located at
                        Some(content_key) => quote!([ #content_key ]),
                        // spread
                        None => quote!(),
                    }
                }
            })
        },
    }
}
