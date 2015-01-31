#![feature(plugin, core)]

#[plugin]
#[no_link]
extern crate static_fragments;
extern crate fragments;
use std::fmt;

template! greeting {"Hello, [[:name]]! Is it written as 'white space' or '[[+join white space]]'?"}

fn main() {
    //Create a new Template
    let mut template = greeting::new();

    //Insert something into the `name` placeholder
    template.insert_name("Peter");

    //Closures and functions with the signature
    //`fn(&[String], &mut fmt::Formatter) -> fmt::Result`
    //will automatically implement the `Generator` trait.
    //This generator will just concatenate the arguments.
    //I expect you to make cooler generators, yourself ;)
    template.insert_generator_join(
        |&: parts: &[String], f: &mut fmt::Formatter| {
            fmt::Display::fmt(&parts.concat(), f)
        }
    );

	//Result: "Hello, Peter! Is it written as 'white space' or 'whitespace'?"
	println!("Result: '{}'", template);
}