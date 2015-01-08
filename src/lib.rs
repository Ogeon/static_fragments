#![feature(plugin_registrar, quote)]

extern crate syntax;
extern crate rustc;
extern crate fragments;

use std::collections::HashMap;

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
    unset_function: Option<ast::Ident>
}

#[plugin_registrar]
#[doc(hidden)]
pub fn macro_registrar(reg: &mut Registry) {
    let expander = box from_string as Box<IdentMacroExpander>;
    reg.register_syntax_extension(token::intern("template"), IdentTT(expander, None));
}

fn from_string<'cx>(cx: &'cx mut ExtCtxt, sp: codemap::Span, module_ident: ast::Ident, tts: Vec<ast::TokenTree>) -> Box<MacResult + 'cx> {
    let mut parser = cx.new_parser_from_tts(tts.as_slice());
    let (string, _) = parser.parse_str();
    let template: Template = string.get().parse().unwrap();
    build_template(cx, sp, module_ident, template.get_tokens())
}

fn build_template<'cx>(cx: &'cx mut ExtCtxt, sp: codemap::Span, module_ident: ast::Ident, tokens: &[Token]) -> Box<MacResult + 'cx> {
    let mut items = Vec::new();

    let mut placeholders = HashMap::new();
    let mut conditions = HashMap::new();
    let mut generators = HashMap::new();
    let template_show_stmts = parse_tokens(cx, sp, tokens, &mut placeholders, &mut conditions, &mut generators);
    
    let ident_template = cx.ident_of("Template");
    let ident_new = cx.ident_of("new");
    let lifetime_content = cx.lifetime_def(sp, cx.name_of("'c"), Vec::new());
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
    let ty_option_box_generator = cx.ty_option(cx.ty_path(cx.path_all(
        sp,
        true,
        vec![cx.ident_of("std"), cx.ident_of("boxed"), cx.ident_of("Box")],
        Vec::new(),
        vec![ty_generator],
        Vec::new()
    )));
    let ty_bool = cx.ty_path(cx.path(sp, vec![cx.ident_of("bool")]));
    let expr_none = cx.expr_none(sp);
    let expr_false = cx.expr_bool(sp, false);


    let mut template_fields = Vec::new();
    let mut template_constructor = Vec::new();
    let mut template_methods = Vec::new();

    for (_label, idents) in placeholders.into_iter() {
        let (field_def, field_assign) = utils::mk_field(sp, idents.field, ty_option_content.clone(), expr_none.clone());
        template_fields.push(field_def);
        template_constructor.push(field_assign);
        template_methods.push(construct_content_setter(cx, sp, idents.set_function, idents.field, lifetime_content.clone()));
    }

    for (_label, idents) in conditions.into_iter() {
        let (field_def, field_assign) = utils::mk_field(sp, idents.field, ty_bool.clone(), expr_false.clone());
        template_fields.push(field_def);
        template_constructor.push(field_assign);
        template_methods.push(construct_condition_setter(cx, sp, idents.set_function, idents.field));
    }

    for (_label, idents) in generators.into_iter() {
        let (field_def, field_assign) = utils::mk_field(sp, idents.field, ty_option_box_generator.clone(), expr_none.clone());
        template_fields.push(field_def);
        template_constructor.push(field_assign);
        template_methods.push(construct_generator_setter(cx, sp, idents.set_function, idents.field, lifetime_content.clone()));
    }

    let template_generics = ast::Generics {
        lifetimes: vec![lifetime_content],
        ty_params: OwnedSlice::empty(),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    let template_struct = cx.item_struct_poly(sp, ident_template,
        ast::StructDef {
        ctor_id: if template_fields.len() == 0 {
                Some(ast::DUMMY_NODE_ID)
            } else {
                None
            },
            fields: template_fields
        },
        template_generics.clone()
    ).map(|mut s| {
        s.vis = ast::Public;
        s
    });
    items.push(template_struct);
    
    let block_new = cx.block(
        sp, vec![],
        Some(cx.expr_struct_ident(sp, ident_template, template_constructor))
    );
    let function_new = cx.item_fn_poly(sp, ident_new, Vec::new(), ty_template.clone(), template_generics.clone(), block_new).map(|mut f| {
        f.vis = ast::Public;
        f
    });
    items.push(function_new);

    let template_impl = utils::mk_impl(cx, sp, template_generics.clone(), None, ty_template.clone(), template_methods);
    items.push(template_impl);

    let template_show_block = cx.block(sp, template_show_stmts, None);
    items.push(utils::implement_show(cx, template_generics.clone(), ty_template.clone(), template_show_block));

    let module = cx.item_mod(sp, sp, module_ident, Vec::new(), Vec::new(), items);
    MacItems::new(vec![module].into_iter())
}

fn parse_tokens<'a>(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    tokens: &'a [Token],
    placeholders: &mut HashMap<&'a str, IdentGroup>,
    conditions: &mut HashMap<&'a str, IdentGroup>,
    generators: &mut HashMap<&'a str, IdentGroup>
) -> Vec<P<ast::Stmt>> {
    tokens.iter().map(|token| {
        match token {
            &Token::String(ref string) => {
                let string = string.as_slice();
                quote_stmt!(cx, try!($string.fmt(f)))
            },
            &Token::Placeholder(ref label) => {
                let field = cx.ident_of(format!("content_{}", label).as_slice());

                placeholders.insert(label.as_slice(), IdentGroup {
                    field: field,
                    set_function: cx.ident_of(format!("insert_{}", label).as_slice()),
                    unset_function: Some(cx.ident_of(format!("unset_{}", label).as_slice()))
                });

                quote_stmt!(cx, if let Some(ref content) = self.$field {
                    try!(content.fmt(f));
                })
            },
            &Token::Conditional(ref label, expected, ref tokens) => {
                let field = cx.ident_of(format!("condition_{}", label).as_slice());
                conditions.insert(label.as_slice(), IdentGroup {
                    field: field,
                    set_function: cx.ident_of(format!("set_{}", label).as_slice()),
                    unset_function: None
                });

                let subsequence = parse_tokens(cx, sp, tokens.as_slice(), placeholders, conditions, generators);

                quote_stmt!(cx, if self.$field == $expected {
                    $subsequence
                })
            },
            &Token::ContentConditional(ref label, expected, ref tokens) => {
                let field = cx.ident_of(format!("content_{}", label).as_slice());

                placeholders.insert(label.as_slice(), IdentGroup {
                    field: field,
                    set_function: cx.ident_of(format!("insert_{}", label).as_slice()),
                    unset_function: Some(cx.ident_of(format!("unset_{}", label).as_slice()))
                });

                let subsequence = parse_tokens(cx, sp, tokens.as_slice(), placeholders, conditions, generators);

                quote_stmt!(cx, if self.$field.is_some() == $expected {
                    $subsequence
                })
            },
            &Token::Generated(ref label, ref args) => {
                let field = cx.ident_of(format!("generator_{}", label).as_slice());

                generators.insert(label.as_slice(), IdentGroup {
                    field: field,
                    set_function: cx.ident_of(format!("insert_generator_{}", label).as_slice()),
                    unset_function: Some(cx.ident_of(format!("unset_generator_{}", label).as_slice()))
                });

                let args = cx.expr_vec(sp, args.iter().map(|arg| {
                    let arg = arg.as_slice();
                    quote_expr!(cx, $arg.to_owned())
                }).collect());

                quote_stmt!(cx, if let Some(ref generator) = self.$field {
                    use ::std::borrow::ToOwned;
                    try!(generator.generate($args.as_slice(), f));
                })
            }
        }
    }).collect()
}

fn construct_content_setter(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    ident: ast::Ident,
    ident_field: ast::Ident,
    lifetime_content: ast::LifetimeDef
) -> ast::ImplItem {
    let ident_content = cx.ident_of("content");
    let ident_t = cx.ident_of("T");
    let ty_t = cx.ty_path(cx.path(sp, vec![ident_t]));

    let args = vec![cx.arg(sp, ident_content, ty_t)];

    let block = cx.block(sp, vec![cx.stmt_expr(quote_expr!(cx, self.$ident_field = Some($ident_content.into_template_content())))], None);

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

    utils::mk_method(cx, sp, generics, ident, SelfType::RefMut(None), args, block, None)
}

fn construct_condition_setter(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    ident: ast::Ident,
    ident_field: ast::Ident
) -> ast::ImplItem {
    let ident_state = cx.ident_of("state");
    let ty_bool = cx.ty_path(cx.path(sp, vec![cx.ident_of("bool")]));

    let args = vec![cx.arg(sp, ident_state, ty_bool)];

    let block = cx.block(sp, vec![cx.stmt_expr(quote_expr!(cx, self.$ident_field = $ident_state))], None);

    let generics = ast::Generics {
        lifetimes: Vec::new(),
        ty_params: OwnedSlice::empty(),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    utils::mk_method(cx, sp, generics, ident, SelfType::RefMut(None), args, block, None)
}

fn construct_generator_setter(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    ident: ast::Ident,
    ident_field: ast::Ident,
    lifetime_content: ast::LifetimeDef
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

    let block = cx.block(sp, vec![cx.stmt_expr(quote_expr!(cx, self.$ident_field = Some(box $ident_generator_var as Box<$ty_generator_lifetime>)))], None);

    let bound = cx.typarambound(path_generator_trait);

    let generics = ast::Generics {
        lifetimes: Vec::new(),
        ty_params: OwnedSlice::from_vec(vec![cx.typaram(sp, ident_t, OwnedSlice::from_vec(vec![bound, ast::RegionTyParamBound(lifetime_content.lifetime)]), None)]),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    utils::mk_method(cx, sp, generics, ident, SelfType::RefMut(None), args, block, None)
}