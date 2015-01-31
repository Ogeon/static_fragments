Static Fragments
================

[![Build Status](https://travis-ci.org/Ogeon/static_fragments.png?branch=master)](https://travis-ci.org/Ogeon/static_fragments)

Static Fragments is an addition to the
[Fragments](https://github.com/Ogeon/fragments) template engine, that can be
used to generate precompiled templates from string literals or files. Some of
benefits of precompiled templates are:

* No runtime parsing. Parsing errors becomes compile time errors.
* Static content becomes static strings and dynamic content is stored in struct fields.
* No need for any resource files after compilation.

#How Is It Used?

`template! module_name {"Template string"}` is used to create a template from
a string literal and `template_file! module_name {"path/to/template/file"}` is
used to read it from a file.

The following example is the same as the `conditions` example from Fragments
and it works more or less the same, with a few exceptions. The
`static_fragments` crate doesn't come with anything more that the compiler
plugin, so the `fragments` crate is still necessary.

```Rust
#![feature(plugin, core)]

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
```

#How Does It Work?

A static template is a data structure with the dynamic content stored in
fields. The template in the example has one placeholder (`name`) and one condition
(`show_hidden`):

```Rust
template! greeting {"Hello, [[:name]]![[?show_hidden]] The condition is true.[[/]]"}
```

Each label is used to create fields in the template structure and the expanded
result would look somewhat like this:

```Rust
mod greeting {
	pub fn new<'c>() -> Template<'c> {
		Template<'c> {
			content_name: None,
			condition_show_hidden: false
		}
	}

	pub struct Template<'c> {
		pub content_name: Option<::fragments::ContentType<'c>>,
		pub condition_show_hidden: bool
	}

	//...
}
```

Notice how the fields got their names from the labels? This makes it trivial
to access them or their corresponding set-methods, knowing the pattern:

* Placeholders:
  - Struct field: `content_{label}` (ex. `name -> content_name`)
  - Set-method: `insert_{label}` (ex. `name -> insert_name`)
* Conditions:
  - Struct field: `condition_{label}` (ex. `show_hidden -> condition_show_hidden`)
  - Set-method: `set_{label}` (ex. `show_hidden -> set_show_hidden`)
* Generators:
  - Struct field: `generator_{label}` (ex. `join -> generator_join`)
  - Set-method: `insert_generator_{label}` (ex. `join -> insert_generator_join`)

This can be quite handy, compared to passing a string key, but does come with
a tiny downside: labels must be `snake_case` to fit the Rust naming
convention.
