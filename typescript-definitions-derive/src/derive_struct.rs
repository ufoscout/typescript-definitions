// Copyright 2019 Ian Castleden
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use quote::quote;
use serde_derive_internals::ast;

use super::{filter_visible, ParseContext, QuoteMaker, QuoteMakerKind};

const DEFAULT_ERROR: Result<super::QuoteT, &'static str> =
    Err("struct cannot have a handler or factory");

impl<'a> ParseContext {
    pub(crate) fn derive_struct(
        &self,
        style: ast::Style,
        fields: &[ast::Field<'a>],
        container: &ast::Container,
    ) -> QuoteMaker {
        match style {
            ast::Style::Struct => self.derive_struct_named_fields(fields, container),
            ast::Style::Newtype => self.derive_struct_newtype(&fields[0], container),
            ast::Style::Tuple => self.derive_struct_tuple(fields, container),
            ast::Style::Unit => self.derive_struct_unit(),
        }
    }

    fn derive_struct_newtype(
        &self,
        field: &ast::Field<'a>,
        ast_container: &ast::Container,
    ) -> QuoteMaker {
        if field.attrs.skip_serializing() {
            return self.derive_struct_unit();
        }
        self.check_flatten(&[field], ast_container);

        QuoteMaker {
            source: self.field_to_ts(field),
            enum_factory: DEFAULT_ERROR,
            enum_handler: DEFAULT_ERROR,
            kind: QuoteMakerKind::Object,
        }
    }

    fn derive_struct_unit(&self) -> QuoteMaker {
        QuoteMaker {
            source: quote!({}),
            enum_factory: DEFAULT_ERROR,
            enum_handler: DEFAULT_ERROR,
            kind: QuoteMakerKind::Object,
        }
    }

    fn derive_struct_named_fields(
        &self,
        fields: &[ast::Field<'a>],
        ast_container: &ast::Container,
    ) -> QuoteMaker {
        let fields = filter_visible(fields);
        if fields.is_empty() {
            return self.derive_struct_unit();
        };

        if fields.len() == 1 && ast_container.attrs.transparent() {
            return self.derive_struct_newtype(&fields[0], ast_container);
        };
        self.check_flatten(&fields, ast_container);
        let content = self.derive_fields(&fields);

        QuoteMaker {
            source: quote!({ #(#content);* }),
            enum_factory: DEFAULT_ERROR,
            enum_handler: DEFAULT_ERROR,
            kind: QuoteMakerKind::Object,
        }
    }

    fn derive_struct_tuple(
        &self,
        fields: &[ast::Field<'a>],
        ast_container: &ast::Container,
    ) -> QuoteMaker {
        let fields = filter_visible(fields);
        if fields.is_empty() {
            return self.derive_struct_unit();
        }

        if fields.len() == 1 && ast_container.attrs.transparent() {
            return self.derive_struct_newtype(&fields[0], ast_container);
        };
        self.check_flatten(&fields, ast_container);
        let content = self.derive_field_tuple(&fields);

        QuoteMaker {
            source: quote!([#(#content),*]),
            enum_factory: DEFAULT_ERROR,
            enum_handler: DEFAULT_ERROR,
            kind: QuoteMakerKind::Object,
        }
    }
}
