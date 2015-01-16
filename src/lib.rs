#![feature(plugin_registrar, quote)]
#![allow(unstable)]

extern crate syntax;
extern crate rustc;
extern crate fragments;

use std::collections::{HashMap, HashSet};
use std::io::{File, BufferedReader};

use syntax::{ast, codemap};
use syntax::ext::base::{
    ExtCtxt, MacResult, MacItems,
    IdentTT, IdentMacroExpander
};
use syntax::owned_slice::OwnedSlice;
use syntax::ext::build::AstBuilder;
use syntax::parse::token;
use syntax::ptr::P;

use rustc::plugin::Registry;

use fragments::{Template, Token, InnerTemplate};

use utils::SelfType;

mod utils;

struct IdentGroup {
    field: ast::Ident,
    set_function: ast::Ident,
    get_function: ast::Ident,
    unset_function: Option<ast::Ident>
}

#[plugin_registrar]
#[doc(hidden)]
pub fn macro_registrar(reg: &mut Registry) {
    let from_string_expander = Box::new(from_string) as Box<IdentMacroExpander>;
    reg.register_syntax_extension(token::intern("template"), IdentTT(from_string_expander, None));
    let from_file_expander = Box::new(from_file) as Box<IdentMacroExpander>;
    reg.register_syntax_extension(token::intern("template_file"), IdentTT(from_file_expander, None));
}

fn from_string<'cx>(cx: &'cx mut ExtCtxt, _sp: codemap::Span, module_ident: ast::Ident, tts: Vec<ast::TokenTree>) -> Box<MacResult + 'cx> {
    let mut parser = cx.new_parser_from_tts(tts.as_slice());
    let sp = parser.span;
    let (string, _) = parser.parse_str();
    let template = match Template::from_chars(&mut string.get().chars()) {
        Ok(template) => template,
        Err(e) => cx.span_fatal(sp, e.as_slice())
    };
    build_template(cx, sp, module_ident, template.get_tokens())
}

fn from_file<'cx>(cx: &'cx mut ExtCtxt, _sp: codemap::Span, module_ident: ast::Ident, tts: Vec<ast::TokenTree>) -> Box<MacResult + 'cx> {
    let mut parser = cx.new_parser_from_tts(tts.as_slice());
    let sp = parser.span;
    let (path, _) = parser.parse_str();
    let file = File::open(&Path::new(path.get()));
    let template = match Template::from_buffer(&mut BufferedReader::new(file)) {
        Ok(template) => template,
        Err(e) => cx.span_fatal(sp, e.as_slice())
    };
    build_template(cx, sp, module_ident, template.get_tokens())
}

fn build_template<'cx>(cx: &'cx mut ExtCtxt, sp: codemap::Span, module_ident: ast::Ident, tokens: &[Token]) -> Box<MacResult + 'cx> {
    let mut items = Vec::new();

    let mut placeholders = HashMap::new();
    let mut expected_placeholders = HashSet::new();
    let mut conditions = HashMap::new();
    let mut generators = HashMap::new();
    let stmts_render = parse_tokens(cx, sp, tokens, &mut placeholders, &mut expected_placeholders, &mut conditions, &mut generators);

    for label in expected_placeholders.into_iter() {
        if !placeholders.contains_key(label) {
            cx.span_err(sp, format!("the missing placeholder '{}' is used in a condition", label).as_slice());
        }
    }
    
    let ident_template = cx.ident_of("Template");
    let ident_inner = cx.ident_of("InnerTemplate");
    let ident_new = cx.ident_of("new");
    let lifetime_content = cx.lifetime_def(sp, cx.name_of("'c"), Vec::new());
    let lifetime_ref = cx.lifetime_def(sp, cx.name_of("'r"), Vec::new());
    let lifetime_traitobj = cx.lifetime_def(sp, cx.name_of("'p"), Vec::new());
    let ty_template = cx.ty_path(cx.path_all(
        sp,
        false,
        vec![ident_template],
        vec![lifetime_content.lifetime],
        Vec::new(),
        Vec::new()
    ));
    let ty_content = cx.ty_path(cx.path_all(
        sp,
        true,
        vec![cx.ident_of("fragments"), cx.ident_of("ContentType")],
        vec![lifetime_content.lifetime],
        Vec::new(),
        Vec::new()
    ));
    let ty_option_content = cx.ty_option(ty_content.clone());
    let ty_option_ref_content = cx.ty_option(cx.ty_rptr(sp, ty_content.clone(), Some(lifetime_ref.lifetime), ast::MutImmutable));
    let ty_generator = cx.ty_sum(
        cx.path_all(
            sp,
            true,
            vec![cx.ident_of("fragments"), cx.ident_of("Generator")],
            Vec::new(),
            Vec::new(),
            Vec::new()
        ),
        OwnedSlice::from_vec(vec![ast::RegionTyParamBound(lifetime_content.lifetime)])
    );
    let ty_generator_ref = cx.ty_sum(
        cx.path_all(
            sp,
            true,
            vec![cx.ident_of("fragments"), cx.ident_of("Generator")],
            Vec::new(),
            Vec::new(),
            Vec::new()
        ),
        OwnedSlice::from_vec(vec![ast::RegionTyParamBound(lifetime_ref.lifetime)])
    );
    let ty_option_box_generator = cx.ty_option(cx.ty_path(cx.path_all(
        sp,
        true,
        vec![cx.ident_of("std"), cx.ident_of("boxed"), cx.ident_of("Box")],
        Vec::new(),
        vec![ty_generator.clone()],
        Vec::new()
    )));
    let ty_option_ref_generator = cx.ty_option(cx.ty_rptr(sp, ty_generator_ref.clone(), Some(lifetime_ref.lifetime), ast::MutImmutable));
    let ty_bool = cx.ty_path(cx.path(sp, vec![cx.ident_of("bool")]));
    let ty_dynamic_inner = cx.ty_sum(
        cx.path_all(
            sp,
            true,
            vec![cx.ident_of("fragments"), cx.ident_of("InnerTemplate")],
            vec![lifetime_content.lifetime],
            Vec::new(),
            Vec::new()
        ),
        OwnedSlice::from_vec(vec![ast::RegionTyParamBound(lifetime_content.lifetime)])
    );
    let ty_traitobj_dynamic_inner = cx.ty_rptr(sp, ty_dynamic_inner.clone(), Some(lifetime_traitobj.lifetime), ast::MutImmutable);
    let trait_inner = cx.trait_ref(cx.path_all(
        sp,
        false,
        vec![ident_inner],
        vec![lifetime_content.lifetime],
        Vec::new(),
        Vec::new()
    ));
    let expr_none = cx.expr_none(sp);
    let expr_false = cx.expr_bool(sp, false);

    let generics_content = ast::Generics {
        lifetimes: vec![lifetime_content.clone()],
        ty_params: OwnedSlice::empty(),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    let generics_traitobj_content = ast::Generics {
        lifetimes: vec![lifetime_traitobj.clone(), lifetime_content.clone()],
        ty_params: OwnedSlice::empty(),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    let mut trait_inner_methods = Vec::new();

    let mut template_fields = Vec::new();
    let mut template_constructor = Vec::new();
    let mut template_methods = Vec::new();
    let mut template_get_methods = Vec::new();

    let mut dynamic_get_methods = Vec::new();

    for (label, idents) in placeholders.into_iter() {
        let ident_field = idents.field;
        let block_template_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.$ident_field.as_ref())));
        let block_dynamic_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.get_content($label))));

        let (field_def, field_assign) = utils::mk_field(sp, idents.field, ty_option_content.clone(), expr_none.clone());

        trait_inner_methods.push(construct_abstract_getter(cx, sp, idents.get_function, Some(lifetime_ref.clone()), ty_option_ref_content.clone()));

        template_fields.push(field_def);
        template_constructor.push(field_assign);
        template_methods.push(
            construct_content_setter(cx, sp, idents.set_function, lifetime_content.clone(), |cx, ident_param| {
                cx.block(sp, vec![cx.stmt_expr(quote_expr!(cx, self.$ident_field = Some($ident_param.into_template_content())))], None)
            })
        );
        template_get_methods.push(construct_getter(cx, sp, idents.get_function, Some(lifetime_ref.clone()), ty_option_ref_content.clone(), block_template_get));
        dynamic_get_methods.push(construct_getter(cx, sp, idents.get_function, Some(lifetime_ref.clone()), ty_option_ref_content.clone(), block_dynamic_get));
    }

    for (label, idents) in conditions.into_iter() {
        let ident_field = idents.field;
        let block_template_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.$ident_field)));
        let block_dynamic_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.get_condition($label))));

        let (field_def, field_assign) = utils::mk_field(sp, idents.field, ty_bool.clone(), expr_false.clone());
        
        trait_inner_methods.push(construct_abstract_getter(cx, sp, idents.get_function, None, ty_bool.clone()));

        template_fields.push(field_def);
        template_constructor.push(field_assign);
        template_methods.push(
            construct_condition_setter(cx, sp, idents.set_function, |cx, ident_param| {
                cx.block(sp, vec![cx.stmt_expr(quote_expr!(cx, self.$ident_field = $ident_param))], None)
            })
        );
        template_get_methods.push(construct_getter(cx, sp, idents.get_function, None, ty_bool.clone(), block_template_get));
        dynamic_get_methods.push(construct_getter(cx, sp, idents.get_function, None, ty_bool.clone(), block_dynamic_get));
    }

    for (label, idents) in generators.into_iter() {
        let ident_field = idents.field;
        let block_template_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.$ident_field.as_ref().map(|g| &**g))));
        let block_dynamic_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.get_generator($label).map(|g| &*g))));

        let (field_def, field_assign) = utils::mk_field(sp, idents.field, ty_option_box_generator.clone(), expr_none.clone());
        
        trait_inner_methods.push(construct_abstract_getter(cx, sp, idents.get_function, Some(lifetime_ref.clone()), ty_option_ref_generator.clone()));

        template_fields.push(field_def);
        template_constructor.push(field_assign);
        template_methods.push(
            construct_generator_setter(cx, sp, idents.set_function, lifetime_content.clone(), |cx, ident_param, ty| {
                cx.block(sp, vec![cx.stmt_expr(quote_expr!(cx, self.$ident_field = Some(Box::new($ident_param) as Box<$ty>)))], None)
            })
        );
        template_get_methods.push(construct_getter(cx, sp, idents.get_function, Some(lifetime_ref.clone()), ty_option_ref_generator.clone(), block_template_get));
        dynamic_get_methods.push(construct_getter(cx, sp, idents.get_function, Some(lifetime_ref.clone()), ty_option_ref_generator.clone(), block_dynamic_get));
    }

    let template_struct = cx.item_struct_poly(sp, ident_template,
        ast::StructDef {
        ctor_id: if template_fields.len() == 0 {
                Some(ast::DUMMY_NODE_ID)
            } else {
                None
            },
            fields: template_fields
        },
        generics_content.clone()
    ).map(|mut s| {
        s.vis = ast::Public;
        s
    });
    items.push(template_struct);
    
    let block_new = cx.block(
        sp, vec![],
        Some(cx.expr_struct_ident(sp, ident_template, template_constructor))
    );
    let function_new = cx.item_fn_poly(sp, ident_new, Vec::new(), ty_template.clone(), generics_content.clone(), block_new).map(|mut f| {
        f.vis = ast::Public;
        f
    });
    items.push(function_new);

    let block_render = cx.block(sp, stmts_render, Some(cx.expr_ok(sp, cx.expr_tuple(sp, Vec::new()))));
    items.push(construct_render_funtion(cx, sp, block_render));

    items.push(construct_trait_inner(cx, sp, ident_inner, lifetime_content.clone(), trait_inner_methods));

    let template_impl = utils::mk_impl(cx, sp, generics_content.clone(), None, ty_template.clone(), template_methods);
    items.push(template_impl);

    let template_impl_inner = utils::mk_impl(cx, sp, generics_content.clone(), Some(trait_inner.clone()), ty_template.clone(), template_get_methods);
    items.push(template_impl_inner);

    let dynamic_impl_inner = utils::mk_impl(cx, sp, generics_traitobj_content.clone(), Some(trait_inner.clone()), ty_traitobj_dynamic_inner.clone(), dynamic_get_methods);
    items.push(dynamic_impl_inner);

    items.push(utils::implement_fmt(cx, generics_content.clone(), ty_template.clone()));

    items.push(utils::implement_template_content(cx, generics_content.clone(), ty_template.clone()));

    let module = cx.item_mod(sp, sp, module_ident, Vec::new(), Vec::new(), items);
    MacItems::new(vec![module].into_iter())
}

fn parse_tokens<'a>(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    tokens: &'a [Token],
    placeholders: &mut HashMap<&'a str, IdentGroup>,
    expected_placeholders: &mut HashSet<&'a str>,
    conditions: &mut HashMap<&'a str, IdentGroup>,
    generators: &mut HashMap<&'a str, IdentGroup>
) -> Vec<P<ast::Stmt>> {
    tokens.iter().map(|token| {
        match token {
            &Token::String(ref string) => {
                let string = string.as_slice();
                quote_stmt!(cx, try!(::std::fmt::String::fmt($string, f)))
            },
            &Token::Placeholder(ref label) => {
                let get = cx.ident_of(format!("get_content_{}", label).as_slice());

                if !expected_placeholders.contains(label.as_slice()) {
                    if !utils::is_snake_case(label.as_slice()) {
                        cx.span_err(sp, format!("non snake case label: '{}'", label).as_slice());
                    }

                    placeholders.insert(label.as_slice(), IdentGroup {
                        field: cx.ident_of(format!("content_{}", label).as_slice()),
                        set_function: cx.ident_of(format!("insert_{}", label).as_slice()),
                        get_function: get,
                        unset_function: Some(cx.ident_of(format!("unset_{}", label).as_slice()))
                    });
                    expected_placeholders.insert(label.as_slice());
                }

                quote_stmt!(cx, if let Some(content) = template.$get() {
                    try!(::std::fmt::String::fmt(content, f));
                })
            },
            &Token::Conditional(ref label, expected, ref tokens) => {
                let get = cx.ident_of(format!("get_condition_{}", label).as_slice());

                if !conditions.contains_key(label.as_slice()) {
                    if !utils::is_snake_case(label.as_slice()) {
                        cx.span_err(sp, format!("non snake case label: '{}'", label).as_slice());
                    }
                    
                    conditions.insert(label.as_slice(), IdentGroup {
                        field: cx.ident_of(format!("condition_{}", label).as_slice()),
                        set_function: cx.ident_of(format!("set_{}", label).as_slice()),
                        get_function: get,
                        unset_function: None
                    });
                }

                let subsequence = parse_tokens(cx, sp, tokens.as_slice(), placeholders, expected_placeholders, conditions, generators);

                quote_stmt!(cx, if template.$get() == $expected {
                    $subsequence
                })
            },
            &Token::ContentConditional(ref label, expected, ref tokens) => {
                let get = cx.ident_of(format!("get_content_{}", label).as_slice());

                if !expected_placeholders.contains(label.as_slice()) {
                    if !utils::is_snake_case(label.as_slice()) {
                        cx.span_err(sp, format!("non snake case label: '{}'", label).as_slice());
                    }

                    expected_placeholders.insert(label.as_slice());
                }

                let subsequence = parse_tokens(cx, sp, tokens.as_slice(), placeholders, expected_placeholders, conditions, generators);

                quote_stmt!(cx, if template.$get().is_some() == $expected {
                    $subsequence
                })
            },
            &Token::Generated(ref label, ref args) => {
                let get = cx.ident_of(format!("get_generator_{}", label).as_slice());


                if !generators.contains_key(label.as_slice()) {
                    if !utils::is_snake_case(label.as_slice()) {
                        cx.span_err(sp, format!("non snake case label: '{}'", label).as_slice());
                    }

                    generators.insert(label.as_slice(), IdentGroup {
                        field: cx.ident_of(format!("generator_{}", label).as_slice()),
                        set_function: cx.ident_of(format!("insert_generator_{}", label).as_slice()),
                        get_function: get,
                        unset_function: Some(cx.ident_of(format!("unset_generator_{}", label).as_slice()))
                    });
                }

                let args = cx.expr_vec(sp, args.iter().map(|arg| {
                    let arg = arg.as_slice();
                    quote_expr!(cx, ::std::borrow::ToOwned::to_owned($arg))
                }).collect());

                quote_stmt!(cx, if let Some(generator) = template.$get() {
                    try!(generator.generate($args.as_slice(), f));
                })
            }
        }
    }).collect()
}

fn construct_render_funtion(cx: &mut ExtCtxt, sp: codemap::Span, block: P<ast::Block>) -> P<ast::Item> {
    let ident_render = cx.ident_of("render");
    let ident_template = cx.ident_of("template");
    let ident_f = cx.ident_of("f");
    let ty_template = cx.ty_rptr(
        sp,
        cx.ty_path(cx.path(
            sp,
            vec![cx.ident_of("InnerTemplate")]
        )),
        None,
        ast::MutImmutable
    );
    let ty_result = cx.ty_path(cx.path_all(
        sp,
        true,
        vec![cx.ident_of("std"), cx.ident_of("fmt"), cx.ident_of("Result")],
        Vec::new(),
        Vec::new(),
        Vec::new()
    ));
    let ty_formatter = cx.ty_path(cx.path_all(
        sp,
        true,
        vec![cx.ident_of("std"), cx.ident_of("fmt"), cx.ident_of("Formatter")],
        Vec::new(),
        Vec::new(),
        Vec::new()
    ));
    let ty_mut_formatter = cx.ty_rptr(sp, ty_formatter, None, ast::MutMutable);
    let args = vec![
        cx.arg(sp, ident_template, ty_template),
        cx.arg(sp, ident_f, ty_mut_formatter)
    ];
    cx.item_fn(sp, ident_render, args, ty_result, block)
}

fn construct_content_setter<F: FnOnce(&mut ExtCtxt, ast::Ident) -> P<ast::Block>>(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    ident: ast::Ident,
    lifetime_content: ast::LifetimeDef,
    make_block: F
) -> ast::ImplItem {
    let ident_content = cx.ident_of("content");
    let ident_t = cx.ident_of("T");
    let ty_t = cx.ty_path(cx.path(sp, vec![ident_t]));

    let args = vec![cx.arg(sp, ident_content, ty_t)];

    let block = make_block(cx, ident_content);

    let bound = cx.typarambound(cx.path_all(
        sp,
        true,
        vec![cx.ident_of("fragments"), cx.ident_of("TemplateContent")],
        vec![lifetime_content.lifetime],
        Vec::new(),
        Vec::new()
    ));

    let generics = ast::Generics {
        lifetimes: Vec::new(),
        ty_params: OwnedSlice::from_vec(vec![cx.typaram(sp, ident_t, OwnedSlice::from_vec(vec![bound]), None)]),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    utils::mk_method(cx, sp, true, generics, ident, SelfType::RefMut(None), args, block, None)
}

fn construct_condition_setter<F: FnOnce(&mut ExtCtxt, ast::Ident) -> P<ast::Block>>(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    ident: ast::Ident,
    make_block: F
) -> ast::ImplItem {
    let ident_state = cx.ident_of("state");
    let ty_bool = cx.ty_path(cx.path(sp, vec![cx.ident_of("bool")]));

    let args = vec![cx.arg(sp, ident_state, ty_bool)];

    let block = make_block(cx, ident_state);

    let generics = ast::Generics {
        lifetimes: Vec::new(),
        ty_params: OwnedSlice::empty(),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    utils::mk_method(cx, sp, true, generics, ident, SelfType::RefMut(None), args, block, None)
}

fn construct_generator_setter<F: FnOnce(&mut ExtCtxt, ast::Ident, P<ast::Ty>) -> P<ast::Block>>(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    ident: ast::Ident,
    lifetime_content: ast::LifetimeDef,
    make_block: F
) -> ast::ImplItem {
    let ident_generator_var = cx.ident_of("generator");
    let ident_t = cx.ident_of("T");
    let path_generator_trait = cx.path_all(
        sp,
        true,
        vec![cx.ident_of("fragments"), cx.ident_of("Generator")],
        Vec::new(),
        Vec::new(),
        Vec::new()
    );
    let ty_t = cx.ty_path(cx.path(sp, vec![ident_t]));
    let ty_generator_lifetime = cx.ty_sum(path_generator_trait.clone(), OwnedSlice::from_vec(vec![ast::RegionTyParamBound(lifetime_content.lifetime)]));

    let args = vec![cx.arg(sp, ident_generator_var, ty_t)];

    let block = make_block(cx, ident_generator_var, ty_generator_lifetime);

    let bound = cx.typarambound(path_generator_trait);

    let generics = ast::Generics {
        lifetimes: Vec::new(),
        ty_params: OwnedSlice::from_vec(vec![cx.typaram(sp, ident_t, OwnedSlice::from_vec(vec![bound, ast::RegionTyParamBound(lifetime_content.lifetime)]), None)]),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    utils::mk_method(cx, sp, true, generics, ident, SelfType::RefMut(None), args, block, None)
}

fn construct_getter(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    ident: ast::Ident,
    lifetime_self: Option<ast::LifetimeDef>,
    ty_return: P<ast::Ty>,
    block: P<ast::Block>
) -> ast::ImplItem {
    let self_lifetime = lifetime_self.as_ref().map(|l| l.lifetime);
    let lifetimes = match lifetime_self {
        Some(lifetime) => vec![lifetime],
        None => Vec::new()
    };

    let generics = ast::Generics {
        lifetimes: lifetimes,
        ty_params: OwnedSlice::empty(),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    utils::mk_method(cx, sp, false, generics, ident, SelfType::Ref(self_lifetime), Vec::new(), block, Some(ty_return))
}

fn construct_abstract_getter(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    ident: ast::Ident,
    lifetime_self: Option<ast::LifetimeDef>,
    ty_return: P<ast::Ty>
) -> ast::TraitItem {
    let self_lifetime = lifetime_self.as_ref().map(|l| l.lifetime);
    let lifetimes = match lifetime_self {
        Some(lifetime) => vec![lifetime],
        None => Vec::new()
    };

    let generics = ast::Generics {
        lifetimes: lifetimes,
        ty_params: OwnedSlice::empty(),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    utils::mk_trait_method(cx, sp, generics, ident, SelfType::Ref(self_lifetime), Vec::new(), Some(ty_return))
}

fn construct_trait_inner(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    ident: ast::Ident,
    lifetime: ast::LifetimeDef,
    items: Vec<ast::TraitItem>
) -> P<ast::Item> {
    let generics = ast::Generics {
        lifetimes: vec![lifetime],
        ty_params: OwnedSlice::empty(),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };
    let item_trait = ast::ItemTrait(ast::Unsafety::Normal, generics, OwnedSlice::empty(), items);
    cx.item(sp, ident, Vec::new(), item_trait)
}
