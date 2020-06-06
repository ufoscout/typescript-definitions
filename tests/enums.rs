#[allow(unused)]

use serde::Serialize;
use typescript_definitions::{TypeScriptify, TypeScriptifyTrait};

#[test]
fn serialize_enum() {
    #[derive(Serialize, TypeScriptify)]
    enum TestEnum {
        VariantA,
        VariantB
    }

    let result = TestEnum::type_script_ify();

    assert_eq!(result, r#"export enum TestEnum { VariantA = "VariantA", VariantB = "VariantB" }"#);
}

#[test]
fn serialize_enum_variants() {
    let expected = concat!("export type Enum = ", '\n', r#" | { V1: { Foo: boolean } } "#, '\n', r#" | { V2: { Bar: number; Baz: number } } "#, '\n', r#" | { V3: { Quux: string } };"#);
    #[derive(Serialize, TypeScriptify)]
    pub enum Enum {
        V1 {
            #[serde(rename = "Foo")]
            foo: bool,
        },
        V2 {
            #[serde(rename = "Bar")]
            bar: i64,
            #[serde(rename = "Baz")]
            baz: u64,
        },
        V3 {
            #[serde(rename = "Quux")]
            quux: String,
        },
    }

    let result = Enum::type_script_ify();

    assert_eq!(result, expected);
}

#[test]
fn serialize_complex_enum() {
    let expected = concat!("export type S = ", '\n', r#" | { kind: "A" } "#, '\n', r#" | { kind: "E2"; fields: { key: number; a: number } } "#, '\n', r#" | { kind: "F"; fields: [number, string] };"#);
    #[derive(Serialize, TypeScriptify)]
    #[serde(tag = "kind", content = "fields")]
    enum S {
        A,
        E2 {
            key: i32,
            a: i32,
            #[serde(skip)]
            b: f64,
        },
        F(i32, #[serde(skip)] f64, String),
        #[serde(skip)]
        Z,
    }

    let result = S::type_script_ify();

    assert_eq!(result, expected)
}
