#![feature(plugin, core)]

#[plugin]
#[no_link]
extern crate static_fragments;
extern crate fragments;
use std::borrow::ToOwned;

template! template1 ("Hello, [[:name]]! This is a [[:something]] template.");
template! template2 ("really [[:something]]");

#[test]
fn template_in_template() {
    let mut template1 = template1::new();
    let mut template2 = template2::new();

    template1.insert_name("Peter");
    template2.insert_something("nice");

    template1.insert_something(template2);

    assert_eq!(template1.to_string(), "Hello, Peter! This is a really nice template.".to_owned());
}