use speculoos::{self, assert_that, Spec};

#[derive(Debug, Spec)]
struct Person<'a> {
    name: &'a str,
    age: u16,
}

#[test]
fn test_struct_with_named_fields() {
    let alice = Person {
        name: "Alice",
        age: 42,
    };

    assert_that!(alice)
        .name(|name| name.is_equal_to("Alice"))
        .age(|age| age.is_equal_to(42));
}

#[derive(Debug, Spec)]
enum Value<'a> {
    Unit,
    Null(),
    Undefined {},

    Lit(i32),
    Add(&'a Value<'a>, &'a Value<'a>),

    Not {
        v: &'a Value<'a>,
    },
    Sub {
        l: &'a Value<'a>,
        r: &'a Value<'a>,
    },
}

#[test]
fn test_enum() {
    assert_that!(Value::Unit).is_unit();
    assert_that!(Value::Null()).is_null();
    assert_that!(Value::Undefined {}).is_undefined();

    assert_that!(Value::Lit(42)).is_lit(|v| v.is_equal_to(42));
    assert_that!(Value::Lit(42)).as_add(|_, _| {}).as_lit(|v| v.is_equal_to(42));

    // TODO: trait is not defined for &Value, Box<Value>, ...
    // assert_that!(Value::Add(&Value::Lit(1), &Value::Lit(2)))
    //     .is_add(|a, b| { a.is_lit(|v| v.is_equal_to(1)); b.is_lit(|v| v.is_equal_to(2)) });
}

#[test]
#[should_panic(expected = "TODO")]
fn test_enum_panic() {
    assert_that!(Value::Null()).is_unit();
}

#[derive(Spec)]
union U {
    a: u32,
    b: &'static str,
}

impl std::fmt::Debug for U {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("U").finish_non_exhaustive()
    }
}

#[test]
fn test_union() {
    unsafe {
        assert_that!(U { a: 1 }).a(|a| a.is_equal_to(1));
        assert_that!(U { b: "foo" }).b(|b| b.is_equal_to("foo"));
    }
}
