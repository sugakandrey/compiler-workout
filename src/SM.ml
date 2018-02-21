open GT
open Syntax

module L = List

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
*)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
*)
let eval' (stack, state, is, os) = function
  | CONST v  -> (v :: stack, state, is, os)
  | READ     -> (L.hd is :: stack, state, L.tl is, os)
  | WRITE    -> (L.tl stack, state, is, L.hd stack :: os)
  | LD name  -> (state name :: stack, state, is, os)
  | ST name  -> (L.tl stack, Expr.update name (L.hd stack) state, is, os)
  | BINOP op ->
    let f = Expr.getBinOpFun op in
    let y :: x :: rem = stack in
    (f x y :: rem, state, is, os)


let eval = L.fold_left eval'

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)

let rec compile' = function
| Expr.Const v         -> [CONST v]
| Expr.Var name        -> [LD name]
| Expr.Binop (f, x, y) -> compile' x @ compile' y @ [BINOP f]

let rec compile = function
  | Stmt.Read s           -> [READ; ST s]
  | Stmt.Write e          -> compile' e @ [WRITE]
  | Stmt.Assign (name, e) -> compile' e @ [ST name]
  | Stmt.Seq (fst, snd)   -> compile fst @ compile snd

