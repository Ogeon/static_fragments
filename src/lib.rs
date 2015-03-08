#![feature(plugin_registrar, quote, rustc_private, path, io)]

extern crate syntax;
extern crate rustc;
extern crate fragments;

use std::collections::{HashMap, HashSet};
use std::io::BufReader;
use std::fs::File;
use std::path::Path;

use syntax::{ast, codemap};
use syntax::ext::base::{
    ExtCtxt, MacResult, MacEager,
    IdentTT, IdentMacroExpander
};
use syntax::owned_slice::OwnedSlice;
use syntax::ext::build::AstBuilder;
use syntax::parse::token;
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

use rustc::plugin::Registry;

use fragments::{Template, Token};

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
    reg.register_syntax_extension(token::intern("template"), IdentTT(from_string_expander, None, true));
    let from_file_expander = Box::new(from_file) as Box<IdentMacroExpander>;
    reg.register_syntax_extension(token::intern("template_file"), IdentTT(from_file_expander, None, true));
}

fn from_string<'cx>(cx: &'cx mut ExtCtxt, _sp: codemap::Span, module_ident: ast::Ident, tts: Vec<ast::TokenTree>) -> Box<MacResult + 'cx> {
    let mut parser = cx.new_parser_from_tts(&tts);
    let sp = parser.span;
    let (string, _) = parser.parse_str();
    let template = match Template::from_chars(string.chars()) {
        Ok(template) => template,
        Err(e) => cx.span_fatal(sp, &e)
    };
    build_template(cx, sp, module_ident, template.get_tokens())
}

fn from_file<'cx>(cx: &'cx mut ExtCtxt, _sp: codemap::Span, module_ident: ast::Ident, tts: Vec<ast::TokenTree>) -> Box<MacResult + 'cx> {
    let mut parser = cx.new_parser_from_tts(&tts);
    let sp = parser.span;
    let (path, _) = parser.parse_str();
    let file = File::open(&Path::new(&*path)).map_err(|e| e.to_string());
    let template = match file.and_then(|f| Template::from_buffer(&mut BufReader::new(f))) {
        Ok(template) => template,
        Err(e) => cx.span_fatal(sp, &e)
    };
    build_template(cx, sp, module_ident, template.get_tokens())
}

fn build_template<'cx>(cx: &'cx mut ExtCtxt, sp: codemap::Span, module_ident: ast::Ident, tokens: &[Token]) -> Box<MacResult + 'cx> {
    let dict = utils::SyntaxDictionary::new(cx, sp);

    let mut items = Vec::new();

    let mut placeholders = HashMap::new();
    let mut expected_placeholders = HashSet::new();
    let mut conditions = HashMap::new();
    let mut generators = HashMap::new();
    let stmts_render = parse_tokens(cx, sp, tokens, &mut placeholders, &mut expected_placeholders, &mut conditions, &mut generators);

    for label in expected_placeholders.into_iter() {
        if !placeholders.contains_key(&label[..]) {
            cx.span_err(sp, &format!("the missing placeholder '{}' is used in a condition", label));
        }
    }


    let expr_none = cx.expr_none(sp);
    let expr_false = cx.expr_bool(sp, false);

    let mut trait_inner_methods = Vec::new();

    let mut template_fields = Vec::new();
    let mut template_constructor = Vec::new();
    let mut template_methods = Vec::new();
    let mut template_get_methods = Vec::new();

    let mut template_content_match_arms = Vec::new();
    let mut template_content_set_match_arms = Vec::new();
    let mut template_condition_match_arms = Vec::new();
    let mut template_generator_match_arms = Vec::new();

    let mut dynamic_get_methods = Vec::new();

    for (label, idents) in placeholders.into_iter() {
        let ident_field = idents.field;
        let block_template_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.$ident_field.as_ref())));
        let block_dynamic_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.get_content($label))));

        let (field_def, field_assign) = utils::mk_field(sp, idents.field, dict.ty.option_content.clone(), expr_none.clone());

        trait_inner_methods.push(construct_abstract_getter(cx, sp, idents.get_function, Some(dict.lifetime.reference.clone()), dict.ty.option_ref_content.clone()));

        template_fields.push(field_def);
        template_constructor.push(field_assign);
        template_methods.push(
            construct_content_setter(cx, sp, idents.set_function, dict.lifetime.content.clone(), |cx, ident_param| {
                cx.block(sp, vec![cx.stmt_expr(quote_expr!(cx, self.$ident_field = Some($ident_param.into_template_content())))], None)
            })
        );
        template_get_methods.push(construct_getter(cx, sp, idents.get_function, Some(dict.lifetime.reference.clone()), dict.ty.option_ref_content.clone(), block_template_get));
        template_content_match_arms.push(cx.arm(sp, vec![cx.pat_lit(sp, quote_expr!(cx, $label))], quote_expr!(cx, self.$ident_field.as_ref())));
        template_content_set_match_arms.push(cx.arm(sp, vec![cx.pat_lit(sp, quote_expr!(cx, $label))], quote_expr!(cx, self.$ident_field.is_some())));
        dynamic_get_methods.push(construct_getter(cx, sp, idents.get_function, Some(dict.lifetime.reference.clone()), dict.ty.option_ref_content.clone(), block_dynamic_get));
    }

    template_content_match_arms.push(cx.arm(sp, vec![cx.pat_wild(sp)], expr_none.clone()));
    template_content_set_match_arms.push(cx.arm(sp, vec![cx.pat_wild(sp)], expr_false.clone()));

    for (label, idents) in conditions.into_iter() {
        let ident_field = idents.field;
        let block_template_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.$ident_field)));
        let block_dynamic_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.get_condition($label))));

        let (field_def, field_assign) = utils::mk_field(sp, idents.field, dict.ty.boolean.clone(), expr_false.clone());
        
        trait_inner_methods.push(construct_abstract_getter(cx, sp, idents.get_function, None, dict.ty.boolean.clone()));

        template_fields.push(field_def);
        template_constructor.push(field_assign);
        template_methods.push(
            construct_condition_setter(cx, sp, idents.set_function, |cx, ident_param| {
                cx.block(sp, vec![cx.stmt_expr(quote_expr!(cx, self.$ident_field = $ident_param))], None)
            })
        );
        template_get_methods.push(construct_getter(cx, sp, idents.get_function, None, dict.ty.boolean.clone(), block_template_get));
        template_condition_match_arms.push(cx.arm(sp, vec![cx.pat_lit(sp, quote_expr!(cx, $label))], quote_expr!(cx, self.$ident_field)));
        dynamic_get_methods.push(construct_getter(cx, sp, idents.get_function, None, dict.ty.boolean.clone(), block_dynamic_get));
    }

    template_condition_match_arms.push(cx.arm(sp, vec![cx.pat_wild(sp)], expr_false.clone()));

    for (label, idents) in generators.into_iter() {
        let ident_field = idents.field;
        let block_template_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.$ident_field.as_ref().map(|g| &**g))));
        let block_dynamic_get = cx.block(sp, Vec::new(), Some(quote_expr!(cx, self.get_generator($label).map(|g| &*g))));

        let (field_def, field_assign) = utils::mk_field(sp, idents.field, dict.ty.option_box_generator.clone(), expr_none.clone());
        
        trait_inner_methods.push(construct_abstract_getter(cx, sp, idents.get_function, Some(dict.lifetime.reference.clone()), dict.ty.option_ref_generator.clone()));

        template_fields.push(field_def);
        template_constructor.push(field_assign);
        template_methods.push(
            construct_generator_setter(cx, sp, idents.set_function, dict.lifetime.content.clone(), |cx, ident_param, ty| {
                cx.block(sp, vec![cx.stmt_expr(quote_expr!(cx, self.$ident_field = Some(Box::new($ident_param) as Box<$ty>)))], None)
            })
        );
        template_get_methods.push(construct_getter(cx, sp, idents.get_function, Some(dict.lifetime.reference.clone()), dict.ty.option_ref_generator.clone(), block_template_get));
        template_generator_match_arms.push(cx.arm(sp, vec![cx.pat_lit(sp, quote_expr!(cx, $label))], quote_expr!(cx, self.$ident_field.as_ref().map(|g| &**g))));
        dynamic_get_methods.push(construct_getter(cx, sp, idents.get_function, Some(dict.lifetime.reference.clone()), dict.ty.option_ref_generator.clone(), block_dynamic_get));
    }

    template_generator_match_arms.push(cx.arm(sp, vec![cx.pat_wild(sp)], expr_none.clone()));

    template_methods.push(utils::implement_dynamic_wrap(cx, sp, dict.lifetime.content.clone()));

    let template_struct = cx.item_struct_poly(sp, dict.ident.template,
        ast::StructDef {
        ctor_id: if template_fields.len() == 0 {
                Some(ast::DUMMY_NODE_ID)
            } else {
                None
            },
            fields: template_fields
        },
        dict.generics.content.clone()
    ).map(|mut s| {
        s.vis = ast::Public;
        s
    });
    items.push(template_struct);
    
    let block_new = cx.block(
        sp, vec![],
        Some(cx.expr_struct_ident(sp, dict.ident.template, template_constructor))
    );
    let function_new = cx.item_fn_poly(sp, dict.ident.new, Vec::new(), dict.ty.template.clone(), dict.generics.content.clone(), block_new).map(|mut f| {
        f.vis = ast::Public;
        f
    });
    items.push(function_new);

    let block_render = cx.block(sp, stmts_render, Some(cx.expr_ok(sp, cx.expr_tuple(sp, Vec::new()))));
    items.push(construct_render_funtion(cx, sp, &dict, block_render));

    items.push(construct_trait_inner(cx, sp, dict.ident.inner_template, dict.lifetime.content.clone(), trait_inner_methods));

    let template_impl = utils::mk_impl(cx, sp, dict.generics.content.clone(), None, dict.ty.template.clone(), template_methods);
    items.push(template_impl);

    let template_impl_inner = utils::mk_impl(cx, sp, dict.generics.content.clone(), Some(dict.traits.inner.clone()), dict.ty.template.clone(), template_get_methods);
    items.push(template_impl_inner);

    let dynamic_impl_inner = utils::mk_impl(cx, sp, dict.generics.traitobj_content.clone(), Some(dict.traits.inner.clone()), dict.ty.traitobj_dynamic_inner.clone(), dynamic_get_methods);
    items.push(dynamic_impl_inner);

    items.push(utils::implement_fmt(cx, dict.generics.content.clone(), dict.ty.template.clone()));

    items.push(utils::implement_template_content(cx, dict.generics.content.clone(), dict.ty.template.clone()));

    items.push(implement_template_dynamic(
        cx, sp,
        &dict,
        template_content_match_arms,
        template_content_set_match_arms,
        template_condition_match_arms,
        template_generator_match_arms
    ));

    let module = cx.item_mod(sp, sp, module_ident, Vec::new(), items);
    MacEager::items(SmallVector::one(module))
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
                let string = &**string;
                quote_stmt!(cx, try!(::std::fmt::Display::fmt($string, f)))
            },
            &Token::Placeholder(ref label) => {
                let get = cx.ident_of(&format!("get_content_{}", label));

                if !expected_placeholders.contains(&label[..]) {
                    if !utils::is_snake_case(label) {
                        cx.span_err(sp, &format!("non snake case label: '{}'", label));
                    }

                    placeholders.insert(label, IdentGroup {
                        field: cx.ident_of(&format!("content_{}", label)),
                        set_function: cx.ident_of(&format!("insert_{}", label)),
                        get_function: get,
                        unset_function: Some(cx.ident_of(&format!("unset_{}", label)))
                    });
                    expected_placeholders.insert(label);
                }

                quote_stmt!(cx, if let Some(content) = template.$get() {
                    try!(::std::fmt::Display::fmt(content, f));
                })
            },
            &Token::Conditional(ref label, expected, ref tokens) => {
                let get = cx.ident_of(&format!("get_condition_{}", label));

                if !conditions.contains_key(&label[..]) {
                    if !utils::is_snake_case(label) {
                        cx.span_err(sp, &format!("non snake case label: '{}'", label));
                    }
                    
                    conditions.insert(label, IdentGroup {
                        field: cx.ident_of(&format!("condition_{}", label)),
                        set_function: cx.ident_of(&format!("set_{}", label)),
                        get_function: get,
                        unset_function: None
                    });
                }

                let subsequence = parse_tokens(cx, sp, tokens, placeholders, expected_placeholders, conditions, generators);

                quote_stmt!(cx, if template.$get() == $expected {
                    $subsequence
                })
            },
            &Token::ContentConditional(ref label, expected, ref tokens) => {
                let get = cx.ident_of(&format!("get_content_{}", label));

                if !expected_placeholders.contains(&label[..]) {
                    if !utils::is_snake_case(label) {
                        cx.span_err(sp, &format!("non snake case label: '{}'", label));
                    }

                    expected_placeholders.insert(label);
                }

                let subsequence = parse_tokens(cx, sp, tokens, placeholders, expected_placeholders, conditions, generators);

                quote_stmt!(cx, if template.$get().is_some() == $expected {
                    $subsequence
                })
            },
            &Token::Generated(ref label, ref args) => {
                let get = cx.ident_of(&format!("get_generator_{}", label));


                if !generators.contains_key(&label[..]) {
                    if !utils::is_snake_case(label) {
                        cx.span_err(sp, &format!("non snake case label: '{}'", label));
                    }

                    generators.insert(label, IdentGroup {
                        field: cx.ident_of(&format!("generator_{}", label)),
                        set_function: cx.ident_of(&format!("insert_generator_{}", label)),
                        get_function: get,
                        unset_function: Some(cx.ident_of(&format!("unset_generator_{}", label)))
                    });
                }

                let args = cx.expr_vec(sp, args.iter().map(|arg| {
                    let arg = &**arg;
                    quote_expr!(cx, ::std::borrow::ToOwned::to_owned($arg))
                }).collect());

                quote_stmt!(cx, if let Some(generator) = template.$get() {
                    try!(generator.generate(&$args, f));
                })
            }
        }
    }).collect()
}

fn construct_render_funtion(cx: &mut ExtCtxt, sp: codemap::Span, dict: &utils::SyntaxDictionary, block: P<ast::Block>) -> P<ast::Item> {
    let ident_render = cx.ident_of("render");
    let ident_template = cx.ident_of("template");
    let ident_f = cx.ident_of("f");
    let ty_template = cx.ty_rptr(
        sp,
        cx.ty_path(cx.path(
            sp,
            vec![dict.ident.inner_template]
        )),
        None,
        ast::MutImmutable
    );
    let args = vec![
        cx.arg(sp, ident_template, ty_template),
        cx.arg(sp, ident_f, dict.ty.mut_formatter.clone())
    ];
    cx.item_fn(sp, ident_render, args, dict.ty.fmt_result.clone(), block)
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

fn construct_dynamic_getter(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    ident: ast::Ident,
    lifetime_self: Option<ast::LifetimeDef>,
    ident_label: ast::Ident,
    ty_return: P<ast::Ty>,
    block: P<ast::Block>
) -> ast::ImplItem {
    let self_lifetime = lifetime_self.as_ref().map(|l| l.lifetime);
    let lifetimes = match lifetime_self {
        Some(lifetime) => vec![lifetime],
        None => Vec::new()
    };

    let ty_str = cx.ty_rptr(sp, cx.ty_ident(sp, cx.ident_of("str")), None, ast::MutImmutable);

    let args = vec![cx.arg(sp, ident_label, ty_str)];

    let generics = ast::Generics {
        lifetimes: lifetimes,
        ty_params: OwnedSlice::empty(),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    utils::mk_method(cx, sp, false, generics, ident, SelfType::Ref(self_lifetime), args, block, Some(ty_return))
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

fn implement_template_dynamic(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    dict: &utils::SyntaxDictionary,
    content: Vec<ast::Arm>,
    content_set: Vec<ast::Arm>,
    conditions: Vec<ast::Arm>,
    generators: Vec<ast::Arm>
) -> P<ast::Item> {
    let ident_get_content = cx.ident_of("get_content");
    let ident_is_content_defined = cx.ident_of("is_content_defined");
    let ident_get_condition = cx.ident_of("get_condition");
    let ident_get_generator = cx.ident_of("get_generator");
    let ident_render = cx.ident_of("render");
    let ident_template = cx.ident_of("top_template");
    let ident_f = cx.ident_of("f");

    let block_content = cx.block(sp, Vec::new(), Some(cx.expr_match(sp, cx.expr_ident(sp, dict.ident.label), content)));
    let block_content_set = cx.block(sp, Vec::new(), Some(cx.expr_match(sp, cx.expr_ident(sp, dict.ident.label), content_set)));
    let block_conditions = cx.block(sp, Vec::new(), Some(cx.expr_match(sp, cx.expr_ident(sp, dict.ident.label), conditions)));
    let block_generators = cx.block(sp, Vec::new(), Some(cx.expr_match(sp, cx.expr_ident(sp, dict.ident.label), generators)));
    let block_render = cx.block(sp, Vec::new(), Some(quote_expr!(cx, render(&$ident_template as &InnerTemplate, $ident_f))));

    let ty_inner_template = cx.ty_rptr(
        sp,
        cx.ty_path(cx.path_all(
            sp,
            true,
            vec![dict.ident.fragments, dict.ident.inner_template],
            Vec::new(),
            Vec::new(),
            Vec::new()
        )),
        None,
        ast::MutImmutable
    );

    let args = vec![
        cx.arg(sp, ident_template, ty_inner_template),
        cx.arg(sp, ident_f, dict.ty.mut_formatter.clone())
    ];

    let methods = vec![
        construct_dynamic_getter(cx, sp, ident_get_content, Some(dict.lifetime.reference.clone()), dict.ident.label, dict.ty.option_ref_content.clone(), block_content),
        construct_dynamic_getter(cx, sp, ident_is_content_defined, None, dict.ident.label, dict.ty.boolean.clone(), block_content_set),
        construct_dynamic_getter(cx, sp, ident_get_condition, None, dict.ident.label, dict.ty.boolean.clone(), block_conditions),
        construct_dynamic_getter(cx, sp, ident_get_generator, Some(dict.lifetime.reference.clone()), dict.ident.label, dict.ty.option_ref_generator.clone(), block_generators),
        utils::mk_method(cx, sp, false, dict.generics.empty.clone(), ident_render, SelfType::Ref(None), args, block_render, Some(dict.ty.fmt_result.clone()))
    ];

    utils::mk_impl(cx, sp, dict.generics.content.clone(), Some(dict.traits.dynamic_inner.clone()), dict.ty.template.clone(), methods)
}
