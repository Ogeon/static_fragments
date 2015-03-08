#![feature(plugin)]
#![plugin(static_fragments)]

extern crate fragments;
use std::borrow::ToOwned;

static PETER: &'static str = "Peter";
static NICE: &'static str = "nice";

template! template1 ("Hello, [[:name]]! This is a [[:something]] template.");

#[test]
fn wrap_identical() {
    let mut template = template1::new();
    template.insert_name(PETER);
    template.insert_something(NICE);
    let shell = template.dynamic_wrap();
    assert_eq!(template.to_string(), shell.to_string());
}

#[test]
fn wrap_set() {
    let mut template = template1::new();
    template.insert_name(PETER);
    template.insert_something(NICE);
    let mut shell = template.dynamic_wrap();
    shell.insert("name".to_owned(), "Olivia");
    assert_eq!(shell.to_string(), "Hello, Olivia! This is a nice template.".to_owned());
}