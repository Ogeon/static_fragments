#![feature(plugin)]
#![plugin(static_fragments)]

extern crate fragments;

template_file! greeting {"examples/hello.txt"}

fn main() {
    //Create a new Template
    let mut template = greeting::new();

    //Insert something into the `name` placeholder
    template.insert_name("Peter");

    //Templates can be printed as they are
    //Result: 'Hello, Peter!'
    println!("Result: '{}'", template);
}