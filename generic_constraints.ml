(****************************************************************************)
(*                                                                          *)
(*    This file is part of Zephyrus.                                        *)
(*                                                                          *)
(*    Zephyrus is free software: you can redistribute it and/or modify      *)
(*    it under the terms of the GNU General Public License as published by  *)
(*    the Free Software Foundation, either version 3 of the License, or     *)
(*    (at your option) any later version.                                   *)
(*                                                                          *)
(*    Zephyrus is distributed in the hope that it will be useful,           *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*    GNU General Public License for more details.                          *)
(*                                                                          *)
(*    You should have received a copy of the GNU General Public License     *)
(*    along with Zephyrus.  If not, see <http://www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)


open Helpers
open Aeolus_types_output.Plain


(* Operator types definitions *)

type unary_arith_op =
  | Abs

type binary_arith_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

and nary_arith_op =
  | Sum

and binary_arith_cmp_op =
  | Lt
  | LEq
  | Eq
  | GEq
  | Gt
  | NEq

and binary_cstr_op =
  | And
  | Or
  | Impl
  | IfAndOnlyIf

and unary_cstr_op =
  | Not



(* Type definitions *)

type const =
  | Int of int
  | Inf of bool (* Plus and minus infinity *)

type var = 
  Variables.variable

and expr =
  | Const               of const                              (* expr = integer constant *)
  | Var                 of var                                (* expr = value of a variable *)
  | Reified             of cstr                               (* expr = if the constraint is satisfied then 1 else 0 *)
  | UnaryArithExpr      of unary_arith_op      * expr         (* expr = OP expr *)
  | BinaryArithExpr     of binary_arith_op     * expr * expr  (* expr = lexpr OP rexpr *)
  | NaryArithExpr       of nary_arith_op       * expr list    (* expr = OP (expr1, expr2, ... , expr) *)

and cstr =
  | TrueCstr
  | FalseCstr
  | BinaryArithCmpCstr  of binary_arith_cmp_op * expr * expr  (* cstr : lexpr OP rexpr *)
  | UnaryCstrOpCstr     of unary_cstr_op       * cstr         (* cstr : OP cstr' *)
  | BinaryCstrOpCstr    of binary_cstr_op      * cstr * cstr  (* cstr : lcstr OP rcstr *)



(* Printing *)

let rec string_of_unary_arith_op op =
  match op with
  | Abs -> "abs"

and string_of_binary_arith_op op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"

and string_of_nary_arith_op op =
  match op with
  | Sum -> "+"

and unit_of_nary_arith_op op =
  match op with
  | Sum -> "0"

and string_of_binary_arith_cmp_op op =
  match op with
  | Lt  -> "<"
  | LEq -> "<="
  | Eq  -> "="
  | GEq -> ">="
  | Gt  -> ">"
  | NEq -> "<>"

and string_of_binary_cstr_op op =
  match op with
  | And         -> "and"
  | Or          -> "or"
  | Impl        -> "=>"
  | IfAndOnlyIf -> "<=>"

and string_of_unary_cstr_op op =
  match op with
  | Not -> "not"

and string_of_const const =
  match const with
  | Int (const) -> Printf.sprintf "%d" const
  | Inf (plus)  -> Printf.sprintf "%sINFINITY" (if plus then "" else "NEG_")

and string_of_var var =
  Variables.string_of_variable var
  
and string_of_expr expr = 
  match expr with
  | Const (const) -> 
      string_of_const const

  | Var (var) ->
      Printf.sprintf "%s" (string_of_var var)

  | Reified (cstr) ->
      Printf.sprintf "||%s||" (string_of_cstr cstr)
  
  | UnaryArithExpr (op, expr) ->
      Printf.sprintf "%s (%s)"
      (string_of_unary_arith_op op)
      (string_of_expr expr)

  | BinaryArithExpr (op, lexpr, rexpr) ->
      Printf.sprintf "(%s %s %s)" 
      (string_of_expr lexpr)
      (string_of_binary_arith_op op)
      (string_of_expr rexpr)

  | NaryArithExpr (op, exprs) ->
      Printf.sprintf "(%s)"
      (if exprs = [] 
       then unit_of_nary_arith_op op
       else
         (String.concat
           (Printf.sprintf " %s " (string_of_nary_arith_op op))
           (List.map string_of_expr exprs) ) )


and string_of_cstr cstr = 
  match cstr with
  | TrueCstr  -> "true"
  | FalseCstr -> "false"

  | BinaryArithCmpCstr (op, lexpr, rexpr) ->
      Printf.sprintf "(%s %s %s)" 
      (string_of_expr lexpr)
      (string_of_binary_arith_cmp_op op)
      (string_of_expr rexpr)

  | BinaryCstrOpCstr (op, lcstr, rcstr) ->
      Printf.sprintf "(%s %s %s)" 
      (string_of_cstr lcstr)
      (string_of_binary_cstr_op op)
      (string_of_cstr rcstr)

  | UnaryCstrOpCstr (op, cstr) ->
      Printf.sprintf "(%s %s)"
      (string_of_unary_cstr_op op)
      (string_of_cstr cstr)



(* Building expressions *)

let var (variable : Variables.variable) = variable

let var2expr   (var   : var)   : expr = Var var
let const2expr (const : const) : expr = Const const
let int2expr   (const : int)   : expr = Const (Int const)

(* Building constraints *)

let truecstr  = TrueCstr
let falsecstr = FalseCstr

(* Reification *)

let reify cstr = Reified cstr


(* Arithmetic operators *)

let ( +~ )    x y  = BinaryArithExpr (Add, x, y)
let ( -~ )    x y  = BinaryArithExpr (Sub, x, y)
let ( *~ )    x y  = BinaryArithExpr (Mul, x, y)
let ( /~ )    x y  = BinaryArithExpr (Div, x, y)
let ( %~ )    x y  = BinaryArithExpr (Mod, x, y)
let abs       x    = UnaryArithExpr  (Abs, x)


let sum exprs_to_sum = NaryArithExpr (Sum, exprs_to_sum)


(* Arithmetic comparisons *)

let (  <~ )   x y  = BinaryArithCmpCstr (Lt,  x, y)
let ( <=~ )   x y  = BinaryArithCmpCstr (LEq, x, y)
let (  =~ )   x y  = BinaryArithCmpCstr (Eq,  x, y)
let ( >=~ )   x y  = BinaryArithCmpCstr (GEq, x, y)
let (  >~ )   x y  = BinaryArithCmpCstr (Gt,  x, y)
let ( <>~ )   x y  = BinaryArithCmpCstr (NEq, x, y)


(* Constraint operators *)

let (  &&~~ ) x y  = BinaryCstrOpCstr (And,         x, y)
let (  ||~~ ) x y  = BinaryCstrOpCstr (Or,          x, y)
let (  =>~~ ) x y  = BinaryCstrOpCstr (Impl,        x, y)
let ( <=>~~ ) x y  = BinaryCstrOpCstr (IfAndOnlyIf, x, y)
let not       x    = UnaryCstrOpCstr  (Not,         x)


(* Extract all variable keys that appear in a constraint / expression. *)

let rec extract_variables_of_cstr cstr =
  match cstr with
  | TrueCstr                              -> []
  | FalseCstr                             -> []
  | BinaryArithCmpCstr (op, lexpr, rexpr) -> (extract_variables_of_expr lexpr) @ (extract_variables_of_expr rexpr)
  | BinaryCstrOpCstr   (op, lcstr, rcstr) -> (extract_variables_of_cstr lcstr) @ (extract_variables_of_cstr rcstr)
  | UnaryCstrOpCstr    (op, cstr)         -> extract_variables_of_cstr cstr

and extract_variables_of_expr expr =
  match expr with
  | Const              (const)            -> []
  | Var                (variable)     -> [variable]
  | Reified            (cstr)             -> extract_variables_of_cstr cstr
  | UnaryArithExpr     (op, expr)         -> (extract_variables_of_expr expr)
  | BinaryArithExpr    (op, lexpr, rexpr) -> (extract_variables_of_expr lexpr) @ (extract_variables_of_expr rexpr)
  | NaryArithExpr      (op, exprs)        -> List.flatten (List.map (fun expr -> (extract_variables_of_expr expr)) exprs)