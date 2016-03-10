#![feature(plugin_registrar)]
#![feature(box_syntax)]
#![feature(quote)]
#![crate_type="dylib"]
//#![crate_name="rewrite"]

extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;

use syntax::{ast, parse};
use syntax::codemap::{Span,BytePos,ExpnId,Spanned};
use syntax::ext::base::{Annotatable, ExtCtxt, MultiItemModifier, Modifier};
use syntax::ptr::P;
use syntax::ast::{Block, Expr, ExprKind, FnDecl, FunctionRetTy, ItemKind, LitKind, LitIntType, Mod, Stmt, StmtKind, UintTy};
use syntax::print::pprust;
use rustc_plugin::registry::Registry;
use syntax::parse::{ParseSess, new_parser_from_source_str};
use syntax::parse::parser::Parser;

struct Mutant;

impl Mutant {
  fn mutate(&self, ecx: &mut ExtCtxt, item: P<ast::Item>) -> P<ast::Item> {
    //for attr in item.attrs.iter() {
    //  println!("attr: {:?}", attr);
    //}
    item.map(|i| self.mutate_item(ecx, i))
  }

  fn mutate_item(&self, ecx: &mut ExtCtxt, ast::Item {id, ident, attrs, node, vis, span}: ast::Item) -> ast::Item {
    println!("ident: {}", ident.name.as_str());

    ast::Item {
      id: id,
      ident: ident,
      attrs: attrs,
      //node: self.mutate_item_underscore(node, span),
      node: self.mutate_item_underscore(ecx, node, span, ident .name.as_str() == "forty_two"),
      vis:   vis,
      span: span
    }
  }

  fn mutate_item_underscore(&self, ecx: &mut ExtCtxt, i: ast::ItemKind, span: Span, doit: bool) -> ast::ItemKind {
    match i {
      ItemKind::Mod(m) => {
        println!("is a mod");
        ItemKind::Mod(self.mutate_mod(ecx, m))
      },
      ItemKind::Fn(decl, unsafety, abi, generics, body) => {
        if !doit {
          return ItemKind::Fn(decl, unsafety, abi, generics, body)
        }
        println!("got a function");
        println!("decl1: {:?}", decl.output);
        println!("body1: {:?}", body);
        let new_decl_output = FunctionRetTy::Default(span);

        let new_decl = P(FnDecl{ inputs: decl.inputs.clone(), output: new_decl_output, variadic: decl.variadic });

        let b = body.map(|b| self.mutate_block(ecx, b));
        println!("decl2: {:?}", new_decl.output);
        println!("body2: {:?}", b);
        ItemKind::Fn(
          decl, //new_decl,
          unsafety,
          abi,
          generics,
          b          //body.map(|b| self.mutate_block(b))
        )
      },
      a => {
        println!("is not a mod");
        a
      }
    }
  }

  fn mutate_mod(&self, ecx: &mut ExtCtxt, Mod{inner, items} : Mod) -> Mod {
    Mod {
        inner: inner,
        items: items.into_iter().map(|i| self.mutate(ecx, i)).collect()
    }
  }

  fn mutate_block(&self, ecx: &mut ExtCtxt, Block {stmts, expr, id, rules,span}: Block) -> Block {
    let new_span = Span {
      lo:      span.lo,
      hi:      span.hi,
      expn_id: ExpnId::from_llvm_cookie(id)
    };

    let mut new_stmts: Vec<P<Stmt>> = Vec::new();
    let new_expr = quote_expr!(ecx, return 42);
    //new_expr.map(|e| { println!("replacing expression with {}", pprust::expr_to_string(&e.clone())); e });
    println!("replacing expression with '{}'", pprust::expr_to_string(&new_expr));
    /*let new_expr = P(Expr {
              id: id,
              node: ExprRet(Some(P(Expr {
                id: id,
                node: ExprLit(P(Spanned {
                  node: LitInt(42, UnsignedIntLit(TyU8)),
                  span: span
                })),
                span: span
              }))),
              span: span,
            });*/
    new_stmts.push(P(Spanned {
      node: StmtKind::Semi(
              new_expr,
              id),
            span: span}
            ));
    Block {
      stmts: new_stmts,
      expr:  expr,
      id:    id,
      rules: rules,
      span:  span
    }
  }
}

impl MultiItemModifier for Mutant {
  fn expand(&self, ecx: &mut ExtCtxt, span: Span, meta: &ast::MetaItem, item: Annotatable) -> Annotatable {
    let a: P<Expr> = quote_expr!(ecx, return 42);
    println!("token: {:?}", a);

    let ps = ParseSess::new();
    let mut parser = new_parser_from_source_str(&ps,
                                  Vec::new(),
                                  "MUTE".to_string(),
                                  "return 1234".to_string());
    let ex = parser.parse_expr();
    println!("EX: {:?}", ex);

    self.mutate(ecx, item)
  }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
  reg.register_syntax_extension(
    parse::token::intern("mutant"),
    Modifier(box Mutant as Box<ItemModifier>)
  );
}

