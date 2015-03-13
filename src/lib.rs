#![feature(plugin_registrar)]
#![feature(box_syntax)]
#![feature(quote)]
#![crate_type="dylib"]
//#![crate_name="rewrite"]

extern crate syntax;
extern crate rustc;

use syntax::{ast, parse};
use syntax::codemap::{Span,BytePos,ExpnId,Spanned};
use syntax::ext::base::{ExtCtxt, ItemModifier, Modifier};
use syntax::ptr::P;
use syntax::ast::Item_::*;
use syntax::ast::FunctionRetTy::*;
use syntax::ast::Expr_::*;
use syntax::ast::Stmt_::*;
use syntax::ast::Lit_::*;
use syntax::ast::LitIntType::*;
use syntax::ast::UintTy::*;
use syntax::ast::{Mod,FnDecl,Block,Stmt,Expr};
use rustc::plugin::Registry;

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

  fn mutate_item_underscore(&self, ecx: &mut ExtCtxt, i: ast::Item_, span: Span, doit: bool) -> ast::Item_ {
    match i {
      ItemMod(m) => {
        println!("is a mod");
        ItemMod(self.mutate_mod(ecx, m))
      },
      ItemFn(decl, unsafety, abi, generics, body) => {
        if !doit {
          return ItemFn(decl, unsafety, abi, generics, body)
        }
        println!("got a function");
        println!("decl1: {:?}", decl.output);
        println!("body1: {:?}", body);
        let new_decl_output = DefaultReturn(span);

        let new_decl = P(FnDecl{ inputs: decl.inputs.clone(), output: new_decl_output, variadic: decl.variadic });

        let b = body.map(|b| self.mutate_block(ecx, b));
        println!("decl2: {:?}", new_decl.output);
        println!("body2: {:?}", b);
        ItemFn(
          decl, //new_decl,
          unsafety,
          abi,
          generics,
          b          //body.map(|b| self.mutate_block(b))
        )
      },
      a          => {
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
      node: StmtSemi(
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

impl ItemModifier for Mutant {
    fn expand(&self, ecx: &mut ExtCtxt, span: Span, meta: &ast::MetaItem, item: P<ast::Item>) -> P<ast::Item> {
      let a: P<syntax::ast::Expr> = quote_expr!(ecx, return 42);
      println!("token: {:?}", a);
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

