#![feature(phase)]

#[phase(plugin)]
extern crate static_fragments;
extern crate fragments;

template! greeting {"Hello, [[:name]]!"}

fn main() {
    let mut template = greeting::new();

    //Insert something into the `name` placeholder
    template.insert_name("Peter");

    //Templates can be printed as they are
    //Result: 'Hello, Peter!'
    println!("Result: '{}'", template);
}