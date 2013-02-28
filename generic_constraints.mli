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


open Aeolus_types_t

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
  | Xor

and unary_cstr_op =
  | Not



(* Type definitions *)

type const =
  | Int of int
  | Inf of bool (* Plus and minus infinity *)

type var = 
  | NamedVar  of Variable_keys.variable_key

and expr =
  | Const               of const                              (* expr = integer constant *)
  | Var                 of var                                (* expr = value of a variable *)
  | Reified             of cstr                               (* expr = if the constraint is satisfied then 1 else 0 *)
  | UnaryArithExpr      of unary_arith_op      * expr         (* expr = OP expr *)
  | BinaryArithExpr     of binary_arith_op     * expr * expr  (* expr = lexpr OP rexpr *)
  | NaryArithExpr       of nary_arith_op       * expr list    (* expr = OP (expr1, expr2, ... , expr) *)
  | BinaryArithCmpExpr  of binary_arith_cmp_op * expr * expr  (* expr = if (lexpr OP rexpr) then 1 else 0 *)

and cstr =
  | TrueCstr
  | FalseCstr
  | BinaryArithCmpCstr  of binary_arith_cmp_op * expr * expr  (* cstr : lexpr OP rexpr *)
  | BinaryCstrOpCstr    of binary_cstr_op      * cstr * cstr  (* cstr : lcstr OP rcstr *)
  | UnaryCstrOpCstr     of unary_cstr_op       * cstr         (* cstr : OP cstr' *)



(* Printing *)

val string_of_var  : var  -> string
val string_of_expr : expr -> string
val string_of_cstr : cstr -> string



(* Building expressions *)

val var        : Variable_keys.variable_key -> var

val var2expr   : var   -> expr
val const2expr : const -> expr
val int2expr   : int   -> expr

(* Building constraints *)

val truecstr  : cstr
val falsecstr : cstr


(* Reification *)

val reify : cstr -> expr

(* Arithmetic operators *)

val ( +~ ) : expr -> expr -> expr
val ( -~ ) : expr -> expr -> expr
val ( *~ ) : expr -> expr -> expr
val ( /~ ) : expr -> expr -> expr
val ( %~ ) : expr -> expr -> expr
val abs    : expr -> expr

val sum : expr list -> expr


(* Reified arithmetic comparisons *)

val (  <~~ ) : expr -> expr -> expr
val ( <=~~ ) : expr -> expr -> expr
val (  =~~ ) : expr -> expr -> expr
val ( >=~~ ) : expr -> expr -> expr
val (  >~~ ) : expr -> expr -> expr
val ( <>~~ ) : expr -> expr -> expr


(* Arithmetic comparisons *)

val (  <~ ) : expr -> expr -> cstr
val ( <=~ ) : expr -> expr -> cstr
val (  =~ ) : expr -> expr -> cstr
val ( >=~ ) : expr -> expr -> cstr
val (  >~ ) : expr -> expr -> cstr
val ( <>~ ) : expr -> expr -> cstr


(* Constraint operators *)

val (  &&~~ ) : cstr -> cstr -> cstr    
val (  ||~~ ) : cstr -> cstr -> cstr    
val (  =>~~ ) : cstr -> cstr -> cstr    
val ( <=>~~ ) : cstr -> cstr -> cstr    
val xor       : cstr -> cstr -> cstr    
val not       : cstr -> cstr


(* Extract all variable keys that appear in a constraint / expression. *)

val extract_variable_keys_of_cstr : cstr -> Variable_keys.variable_key list