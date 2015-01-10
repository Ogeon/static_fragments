use syntax::{self, ast, codemap};
use syntax::ext::base::ExtCtxt;
use syntax::ext::build::AstBuilder;
use syntax::parse::token;
use syntax::ptr::P;

pub enum SelfType {
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

pub fn implement_fmt(cx: &mut ExtCtxt, generics: ast::Generics, ty: P<ast::Ty>, block: P<ast::Block>) -> P<ast::Item> {
    quote_item!(cx, impl$generics ::std::fmt::String for $ty {
        fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            $block
            Ok(())
        }
    }).unwrap()
}

pub fn implement_template_content(cx: &mut ExtCtxt, generics: ast::Generics, ty: P<ast::Ty>) -> P<ast::Item> {
    quote_item!(cx, impl$generics ::fragments::TemplateContent$generics for $ty {
        fn into_template_content(self) -> ::fragments::ContentType$generics {
            ::fragments::ContentType::Fmt(Box::new(self) as Box<::std::fmt::String>)
        }
    }).unwrap()
}

pub fn mk_field(sp: codemap::Span, ident: ast::Ident, ty: P<ast::Ty>, default_expr: P<ast::Expr>) -> (codemap::Spanned<ast::StructField_>, ast::Field) {
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

pub fn mk_impl(
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

pub fn mk_method(
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

//Shamelessly borrowed from the lint :)
pub fn is_snake_case(name: &str) -> bool {
    let mut allow_underscore = false;
    name.chars().all(|c| {
        allow_underscore = match c {
            c if c.is_lowercase() || c.is_numeric() => true,
            '_' if allow_underscore => false,
            _ => return false,
        };
        true
    })
}
