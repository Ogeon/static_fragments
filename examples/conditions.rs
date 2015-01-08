#![feature(plugin)]

#[plugin]
#[no_link]
extern crate static_fragments;
extern crate fragments;

template! greeting {"Hello, [[:name]]![[?show_hidden]] The condition is true.[[/]]"}

fn main() {
    //Create a new Template
    let mut template = greeting::new();

    //Insert something into the `name` placeholder
    template.insert_name("Peter");

    //Conditions are false by default, so the second sentence will be disabled
    //Result: 'Hello, Peter!'
    println!("Result: '{}'", template);

    //Let's enable the hidden part of the template
    template.set_show_hidden(true);

    //Result: 'Hello, Peter! The condition is true.'
    println!("Result: '{}'", template);
}