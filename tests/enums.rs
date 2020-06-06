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
