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

struct IdentGroup {
    field: ast::Ident,
    set_function: ast::Ident,
    unset_function: ast::Ident
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
    let ty_bool = cx.ty_path(cx.path(sp, vec![cx.ident_of("bool")]));
    let expr_none = cx.expr_none(sp);
    let expr_false = cx.expr_bool(sp, false);


    let mut template_fields = Vec::new();
    let mut template_constructor = Vec::new();
    let mut template_methods = Vec::new();

    for (_label, idents) in placeholders.into_iter() {
        let (field_def, field_assign) = construct_field(sp, idents.field, ty_option_content.clone(), expr_none.clone());
        template_fields.push(field_def);
        template_constructor.push(field_assign);
        template_methods.push(construct_content_setter(cx, sp, idents.set_function, idents.field, lifetime_content.clone()));
    }

    for (_label, idents) in conditions.into_iter() {
        let (field_def, field_assign) = construct_field(sp, idents.field, ty_bool.clone(), expr_false.clone());
        template_fields.push(field_def);
        template_constructor.push(field_assign);
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

    let template_impl = construct_impl(cx, sp, template_generics.clone(), None, ty_template.clone(), template_methods);
    items.push(template_impl);

    let template_show_block = cx.block(sp, template_show_stmts, None);
    items.push(implement_show(cx, template_generics.clone(), ty_template.clone(), template_show_block));

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
                    unset_function: cx.ident_of(format!("unset_{}", label).as_slice())
                });

                quote_stmt!(cx, if let Some(ref content) = self.$field {
                    try!(content.fmt(f));
                })
            },
            &Token::Conditional(ref label, expected, ref tokens) => {
                let field = cx.ident_of(format!("condition_{}", label).as_slice());

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
                    unset_function: cx.ident_of(format!("unset_{}", label).as_slice())
                });

                let subsequence = parse_tokens(cx, sp, tokens.as_slice(), placeholders, conditions, generators);

                quote_stmt!(cx, if self.$field.is_some() == $expected {
                    $subsequence
                })
            },
            &Token::Generated(ref label, ref args) => {
                let field = cx.ident_of(format!("generator_{}", label).as_slice());

                let args = cx.expr_vec(sp, args.iter().map(|arg| {
                    let arg = arg.as_slice();
                    quote_expr!(cx, $arg)
                }).collect());

                quote_stmt!(cx, try!(self.$field($args, f)))
            }
        }
    }).collect()
}

fn construct_field(sp: codemap::Span, ident: ast::Ident, ty: P<ast::Ty>, default_expr: P<ast::Expr>) -> (codemap::Spanned<ast::StructField_>, ast::Field) {
    let field_def = ast::StructField_ {
        kind: ast::NamedField(ident, ast::Public),
        id: ast::DUMMY_NODE_ID,
        ty: ty,
        attrs: Vec::new()
    };

    let field_assign = ast::Field {
        ident: codemap::Spanned {
            node: ident,
            span: sp
        },
        expr: default_expr,
        span: sp
    };

    (
        codemap::Spanned {
            node: field_def,
            span: sp
        },
        field_assign
    )
}

fn construct_impl(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    generics: ast::Generics,
    trait_ref: Option<ast::TraitRef>,
    ty: P<ast::Ty>,
    items: Vec<ast::ImplItem>
) -> P<ast::Item> {
    cx.item(
        sp, cx.ident_of(""), Vec::new(),
        ast::ItemImpl(ast::Unsafety::Normal, ast::ImplPolarity::Positive, generics, trait_ref, ty, items)
    )
}

enum SelfType {
    Ref(Option<ast::Lifetime>),
    RefMut(Option<ast::Lifetime>)
}

impl SelfType {
    fn into_explicit(self, cx: &mut ExtCtxt) -> ast::ExplicitSelf_ {
        match self {
            SelfType::Ref(lifetime) => ast::SelfRegion(lifetime, ast::MutImmutable, cx.ident_of("self")),
            SelfType::RefMut(lifetime) => ast::SelfRegion(lifetime, ast::MutMutable, cx.ident_of("self"))
        }
    }
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

    construct_method(cx, sp, generics, ident, SelfType::RefMut(None), args, block, None)
}

fn implement_show(cx: &mut ExtCtxt, generics: ast::Generics, ty: P<ast::Ty>, block: P<ast::Block>) -> P<ast::Item> {
    quote_item!(cx, impl$generics ::std::fmt::Show for $ty {
        fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            $block
            Ok(())
        }
    }).unwrap()
}

fn construct_method(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    generics: ast::Generics,
    ident: ast::Ident,
    self_type: SelfType,
    args: Vec<ast::Arg>,
    block: P<ast::Block>,
    return_type: Option<P<ast::Ty>>
) -> ast::ImplItem {
    let self_type = codemap::Spanned {
        node: self_type.into_explicit(cx),
        span: sp
    };

    let output = match return_type {
        Some(ty) => ty,
        None => cx.ty(sp, ast::TyTup(Vec::new()))
    };

    let decl = P(ast::FnDecl {
        inputs: vec![cx.arg(sp, cx.ident_of("self"), cx.ty_infer(sp))].into_iter().chain(args.into_iter()).collect(),
        output: ast::Return(output),
        variadic: false
    });

    let method = ast::Method {
        attrs: ignore_dead_code(cx, sp),
        id: ast::DUMMY_NODE_ID,
        span: sp,
        node: ast::MethDecl(ident, generics, syntax::abi::Rust, self_type, ast::Unsafety::Normal, decl, block, ast::Public)
    };

    ast::MethodImplItem(P(method))
}

fn ignore_dead_code(cx: &mut ExtCtxt, sp: codemap::Span) -> Vec<ast::Attribute> {
    vec![cx.attribute(sp, cx.meta_list(sp, token::InternedString::new("allow"), vec![cx.meta_word(sp, token::InternedString::new("dead_code"))]))]
}