use syntax::{self, ast, codemap};
use syntax::ext::base::ExtCtxt;
use syntax::ext::build::AstBuilder;
use syntax::parse::token;
use syntax::ptr::P;
use syntax::owned_slice::OwnedSlice;

pub struct SyntaxDictionary {
    pub ident: IdentifierDictionary,
    pub lifetime: LifetimeDictionary,
    pub ty: TypeDictionary,
    pub traits: TraitDictionary,
    pub generics: GenericsDictionary
}

impl SyntaxDictionary {
    pub fn new(cx: &mut ExtCtxt, sp: codemap::Span) -> SyntaxDictionary {
        let ident = IdentifierDictionary::new(cx);
        let lifetime = LifetimeDictionary::new(cx, sp);
        let ty = TypeDictionary::new(cx, sp, &ident, &lifetime);
        let traits = TraitDictionary::new(cx, sp, &ident, &lifetime);
        let generics = GenericsDictionary::new(&lifetime);

        SyntaxDictionary {
            ident: ident,
            lifetime: lifetime,
            ty: ty,
            traits: traits,
            generics: generics
        }
    }
}

pub struct IdentifierDictionary {
    pub template: ast::Ident,
    pub inner_template: ast::Ident,
    pub new: ast::Ident,
    pub label: ast::Ident,
    pub fragments: ast::Ident,
    pub template_content: ast::Ident,
    pub content_type: ast::Ident,
    pub generator: ast::Ident,
    pub std: ast::Ident,
    pub boxed: ast::Ident,
    pub box_: ast::Ident,
    pub fmt: ast::Ident,
    pub formatter: ast::Ident,
    pub result: ast::Ident
}

impl IdentifierDictionary {
    fn new(cx: &mut ExtCtxt) -> IdentifierDictionary {
        IdentifierDictionary {
            template: cx.ident_of("Template"),
            inner_template: cx.ident_of("InnerTemplate"),
            new: cx.ident_of("new"),
            label: cx.ident_of("label"),
            fragments: cx.ident_of("fragments"),
            template_content: cx.ident_of("TemplateContent"),
            content_type: cx.ident_of("ContentType"),
            generator: cx.ident_of("Generator"),
            std: cx.ident_of("std"),
            boxed: cx.ident_of("boxed"),
            box_: cx.ident_of("Box"),
            fmt: cx.ident_of("fmt"),
            formatter: cx.ident_of("Formatter"),
            result: cx.ident_of("Result")
        }
    }
}

pub struct LifetimeDictionary {
    pub content: ast::LifetimeDef,
    pub reference: ast::LifetimeDef,
    pub objptr: ast::LifetimeDef,
    pub traitobj: ast::LifetimeDef
}

impl LifetimeDictionary {
    fn new(cx: &mut ExtCtxt, sp: codemap::Span) -> LifetimeDictionary {
        LifetimeDictionary {
            content: cx.lifetime_def(sp, cx.name_of("'c"), Vec::new()),
            reference: cx.lifetime_def(sp, cx.name_of("'r"), Vec::new()),
            objptr: cx.lifetime_def(sp, cx.name_of("'p"), Vec::new()),
            traitobj: cx.lifetime_def(sp, cx.name_of("'t"), Vec::new())
        }
    }
}

pub struct TypeDictionary {
    pub template: P<ast::Ty>,
    pub option_content: P<ast::Ty>,
    pub option_ref_content: P<ast::Ty>,
    pub option_box_generator: P<ast::Ty>,
    pub option_ref_generator: P<ast::Ty>,
    pub boolean: P<ast::Ty>,
    pub traitobj_dynamic_inner: P<ast::Ty>,
    pub mut_formatter: P<ast::Ty>,
    pub fmt_result: P<ast::Ty>
}

impl TypeDictionary {
    fn new(cx: &mut ExtCtxt, sp: codemap::Span, ident: &IdentifierDictionary, lifetime: &LifetimeDictionary) -> TypeDictionary {
        let content = cx.ty_path(cx.path_all(
            sp,
            true,
            vec![ident.fragments, ident.content_type],
            vec![lifetime.content.lifetime],
            Vec::new(),
            Vec::new()
        ));

        let generator = cx.ty_sum(
            cx.path_all(
                sp,
                true,
                vec![ident.fragments, ident.generator],
                Vec::new(),
                Vec::new(),
                Vec::new()
            ),
            OwnedSlice::from_vec(vec![ast::RegionTyParamBound(lifetime.content.lifetime)])
        );

        let generator_ref = cx.ty_sum(
            cx.path_all(
                sp,
                true,
                vec![ident.fragments, ident.generator],
                Vec::new(),
                Vec::new(),
                Vec::new()
            ),
            OwnedSlice::from_vec(vec![ast::RegionTyParamBound(lifetime.reference.lifetime)])
        );

        let dynamic_inner = cx.ty_sum(
            cx.path_all(
                sp,
                true,
                vec![ident.fragments, ident.inner_template],
                vec![lifetime.content.lifetime],
                Vec::new(),
                Vec::new()
            ),
            OwnedSlice::from_vec(vec![ast::RegionTyParamBound(lifetime.traitobj.lifetime)])
        );

        let formatter = cx.ty_path(cx.path_all(
            sp,
            true,
            vec![ident.std, ident.fmt, ident.formatter],
            Vec::new(),
            Vec::new(),
            Vec::new()
        ));

        TypeDictionary {
            template: cx.ty_path(cx.path_all(
                sp,
                false,
                vec![ident.template],
                vec![lifetime.content.lifetime],
                Vec::new(),
                Vec::new()
            )),
            option_content: cx.ty_option(content.clone()),
            option_ref_content: cx.ty_option(cx.ty_rptr(sp, content.clone(), Some(lifetime.reference.lifetime), ast::MutImmutable)),
            option_box_generator: cx.ty_option(cx.ty_path(cx.path_all(
                sp,
                true,
                vec![ident.std, ident.boxed, ident.box_],
                Vec::new(),
                vec![generator.clone()],
                Vec::new()
            ))),
            option_ref_generator: cx.ty_option(cx.ty_rptr(sp, generator_ref.clone(), Some(lifetime.reference.lifetime), ast::MutImmutable)),
            boolean: cx.ty_path(cx.path(sp, vec![cx.ident_of("bool")])),
            traitobj_dynamic_inner: cx.ty_rptr(sp, dynamic_inner.clone(), Some(lifetime.objptr.lifetime), ast::MutImmutable),
            mut_formatter: cx.ty_rptr(sp, formatter, None, ast::MutMutable),
            fmt_result: cx.ty_path(cx.path_all(
                sp,
                true,
                vec![ident.std, ident.fmt, ident.result],
                Vec::new(),
                Vec::new(),
                Vec::new()
            ))
        }
    }
}

pub struct TraitDictionary {
    pub inner: ast::TraitRef,
    pub dynamic_inner: ast::TraitRef
}

impl TraitDictionary {
    fn new(cx: &mut ExtCtxt, sp: codemap::Span, ident: &IdentifierDictionary, lifetime: &LifetimeDictionary) -> TraitDictionary {
        TraitDictionary {
            inner: cx.trait_ref(cx.path_all(
                sp,
                false,
                vec![ident.inner_template],
                vec![lifetime.content.lifetime],
                Vec::new(),
                Vec::new()
            )),
            dynamic_inner: cx.trait_ref(cx.path_all(
                sp,
                true,
                vec![ident.fragments, ident.inner_template],
                vec![lifetime.content.lifetime],
                Vec::new(),
                Vec::new()
            ))
        }
    }
}

pub struct GenericsDictionary {
    pub content: ast::Generics,
    pub traitobj_content: ast::Generics,
    pub empty: ast::Generics
}

impl GenericsDictionary {
    fn new(lifetime: &LifetimeDictionary) -> GenericsDictionary {
        GenericsDictionary {
            content: ast::Generics {
                lifetimes: vec![lifetime.content.clone()],
                ty_params: OwnedSlice::empty(),
                where_clause: ast::WhereClause {
                    id: ast::DUMMY_NODE_ID,
                    predicates: Vec::new()
                }
            },
            traitobj_content: ast::Generics {
                lifetimes: vec![lifetime.objptr.clone(), lifetime.content.clone(), lifetime.traitobj.clone()],
                ty_params: OwnedSlice::empty(),
                where_clause: ast::WhereClause {
                    id: ast::DUMMY_NODE_ID,
                    predicates: Vec::new()
                }
            },
            empty: ast::Generics {
                lifetimes: Vec::new(),
                ty_params: OwnedSlice::empty(),
                where_clause: ast::WhereClause {
                    id: ast::DUMMY_NODE_ID,
                    predicates: Vec::new()
                }
            }
        }
    }
}





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

pub fn implement_fmt(cx: &mut ExtCtxt, generics: ast::Generics, ty: P<ast::Ty>) -> P<ast::Item> {
    quote_item!(cx, impl$generics ::std::fmt::String for $ty {
        fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            render(self as &InnerTemplate, f)
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

pub fn implement_dynamic_wrap(cx: &mut ExtCtxt, sp: codemap::Span, lifetime_content: ast::LifetimeDef) -> ast::ImplItem {
    let block = cx.block(sp, Vec::new(), Some(quote_expr!(cx, ::fragments::Shell::new(self))));

    let lifetime_shell = cx.lifetime_def(sp, cx.name_of("'s"), vec![lifetime_content.lifetime]);

    let self_type = SelfType::Ref(Some(lifetime_shell.lifetime));

    let ty_shell = cx.ty_path(cx.path_all(
        sp,
        true,
        vec![cx.ident_of("fragments"), cx.ident_of("Shell")],
        vec![lifetime_shell.lifetime, lifetime_content.lifetime],
        Vec::new(),
        Vec::new()
    ));

    let generics = ast::Generics {
        lifetimes: vec![lifetime_shell],
        ty_params: OwnedSlice::empty(),
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: Vec::new()
        }
    };

    let ident = cx.ident_of("dynamic_wrap");

    mk_method(cx, sp, true, generics, ident, self_type, Vec::new(), block, Some(ty_shell))
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
    public: bool,
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
        node: ast::MethDecl(ident, generics, syntax::abi::Rust, self_type, ast::Unsafety::Normal, decl, block, if public {ast::Public} else {ast::Inherited})
    };

    ast::MethodImplItem(P(method))
}

pub fn mk_trait_method(
    cx: &mut ExtCtxt,
    sp: codemap::Span,
    generics: ast::Generics,
    ident: ast::Ident,
    self_type: SelfType,
    args: Vec<ast::Arg>,
    return_type: Option<P<ast::Ty>>
) -> ast::TraitItem {
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

    let method = ast::TypeMethod {
        ident: ident,
        attrs: Vec::new(),
        unsafety: ast::Unsafety::Normal,
        abi: syntax::abi::Rust,
        decl: decl,
        generics: generics,
        explicit_self: self_type,
        id: ast::DUMMY_NODE_ID,
        span: sp,
        vis: ast::Inherited
    };

    ast::RequiredMethod(method)
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
