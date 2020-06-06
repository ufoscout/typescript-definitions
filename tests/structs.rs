#[allow(unused)]

use serde::Serialize;

use typescript_definitions::{TypeScriptify, TypeScriptifyTrait};

#[test]
fn serialize_basic_struct() {
    #[derive(Serialize, TypeScriptify, Debug)]
    pub struct Point {
        #[serde(rename = "X")]
        pub x: i64,
        #[serde(rename = "Y")]
        pub y: i64,
        pub z: i64,
    }

    let result = Point::type_script_ify();

    assert_eq!(result, r#"export interface Point { X: number; Y: number; z: number; }"#);
}

#[test]
fn serialize_unit_struct() {
    #[derive(Serialize, TypeScriptify)]
    pub struct Newtype(pub i64);

    let result = Newtype::type_script_ify();

    assert_eq!(result, r#"export type Newtype = number;"#)
}

#[test]
fn serialize_generic_struct() {
    #[derive(Serialize, TypeScriptify)]
    pub struct Value<T : ToString> {
        value: T,
    }

    let result = Value::<String>::type_script_ify();

    assert_eq!(result, r#"export interface Value<T> { value: T; }"#)
}

#[test]
fn serialize_byte_string() {
    #[derive(Serialize, TypeScriptify)]
    pub struct MyBytes {
        #[serde(serialize_with = "typescript_definitions::as_byte_string")]
        pub buffer: Vec<u8>,

    }

    let result = MyBytes::type_script_ify();

    assert_eq!(result, r#"export interface MyBytes { buffer: string; }"#)
}
