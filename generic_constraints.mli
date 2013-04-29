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


(** Operator definitions *)

(** Unary arithmetic operators: to perform operations on an arithmetic expression. *)
type unary_arith_op =
  | Abs (** Absolute value *)

(** Binary arithmetic operators: to perform operations on two arithmetic expressions. *)
type binary_arith_op =
  | Add (** Addition operator *)
  | Sub (** Substraction operator *)
  | Mul (** Multiplication operator *)
  | Div (** Integer division operator *)
  | Mod (** Modulo operator *)

(** N-ary arithmetic operators: to perform operations on multiple arithmetic expressions. *)
type nary_arith_op =
  | Sum (** Sum operator *)

(** Binary arithmetic comparison operators: to compare two arithmetic expressions. *)
type binary_arith_cmp_op =
  | Lt  (** Less-than operator *)
  | LEq (** Less-than-or-equal-to operator *)
  | Eq  (** Equal-to operator *)
  | GEq (** Grearter-than-or-equal-to operator *)
  | Gt  (** Greater-than operator *)
  | NEq (** Not-equal-to operator *)

(** Unary constraint operators: to perform operations on a constraint. *)
type unary_cstr_op =
  | Not (** Not operator *)

(** Binary constraint operators: to perform operations on two constraints. *)
type binary_cstr_op =
  | And         (** And operator *)
  | Or          (** Or operator *)
  | Impl        (** Implies operator *)
  | IfAndOnlyIf (** If-and-only-if operator *)



(** Type definitions *)

(** Constants *)
type const =
  | Int of int  (** An integer constant *)
  | Inf of bool (** A positive and negative infinity constant *)

(** Variables *)
type var = 
  Variables.variable

(** Arithmetic expressions *)
type expr =
  | Const               of const                              (** Constant expression. *)
  | Var                 of var                                (** Expression representing the value of a variable. *)
  | Reified             of cstr                               (** Reified constraint: if the constraint is satisfied then this expression will evaluate to 1, if not to 0. *)
  | UnaryArithExpr      of unary_arith_op      * expr         (** Unary arithmetic operator applied to an expression: OP (expr1). *)
  | BinaryArithExpr     of binary_arith_op     * expr * expr  (** Binary arithmetic operator applied to a pair of expressions: expr1 OP expr2.*)
  | NaryArithExpr       of nary_arith_op       * expr list    (** N-ary arithmetic operator applied to a list of expressions: OP (expr1, expr2, ... , expr). *)

(** Constraints *)
and cstr =
  | TrueCstr  (** Always satisfied constraint. *)
  | FalseCstr (** Never satisfied constraint. *)
  | BinaryArithCmpCstr  of binary_arith_cmp_op * expr * expr  (** Binary arithmetic comparison operator applied to two expressions. If the comparison is true, then the constraint is satisfied, if not, then not. *)
  | UnaryCstrOpCstr     of unary_cstr_op       * cstr         (** Unary constraint operator applied to a constraint: OP (cstr1). *)
  | BinaryCstrOpCstr    of binary_cstr_op      * cstr * cstr  (** Binary constraint operator applied to a pair of constraints : cstr1 OP rcstr2. *)



(** Printing *)

val string_of_var  : var  -> string
val string_of_expr : expr -> string
val string_of_cstr : cstr -> string



(** Building expressions *)

val var        : Variables.variable -> var

val var2expr   : var   -> expr
val const2expr : const -> expr
val int2expr   : int   -> expr

(** Building constraints *)

val truecstr  : cstr
val falsecstr : cstr


(** Reification *)

val reify : cstr -> expr

(** Arithmetic operators *)

val ( +~ ) : expr -> expr -> expr
val ( -~ ) : expr -> expr -> expr
val ( *~ ) : expr -> expr -> expr
val ( /~ ) : expr -> expr -> expr
val ( %~ ) : expr -> expr -> expr
val abs    : expr -> expr

val sum : expr list -> expr


(** Arithmetic comparisons *)

val (  <~ ) : expr -> expr -> cstr
val ( <=~ ) : expr -> expr -> cstr
val (  =~ ) : expr -> expr -> cstr
val ( >=~ ) : expr -> expr -> cstr
val (  >~ ) : expr -> expr -> cstr
val ( <>~ ) : expr -> expr -> cstr


(** Constraint operators *)

val (  &&~~ ) : cstr -> cstr -> cstr
val (  ||~~ ) : cstr -> cstr -> cstr
val (  =>~~ ) : cstr -> cstr -> cstr
val ( <=>~~ ) : cstr -> cstr -> cstr
val not       : cstr -> cstr


(** Extract all variable keys that appear in a constraint / expression. *)
val extract_variables_of_cstr : cstr -> Variables.variable list