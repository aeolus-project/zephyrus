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



(************************************)
(** 1/4 Types                       *)
(************************************)

type variable =
  (* Number of instances of a given component_type / port / package installed globally in the configuration. *)
  | GlobalElementVariable    of DataInput.element
  (* Number of instances of a given component_type / port / package installed on a given location. *)
  | LocalElementVariable     of DataInput.location_name * DataInput.element
  (* Number of bindings on the given port between the instances of the given requiring type and given providing type. *)
  | BindingVariable          of DataInput.port_name * DataInput.component_type_name * DataInput.component_type_name
  (* Is the given repository installed on the given location? (boolean variable) *)
  | LocalRepositoryVariable  of DataInput.location_name * DataInput.repository_name
  (* How many resources of the given type are provided by the given location. *)
  | LocalResourceVariable    of DataInput.location_name * DataInput.resource_name
  (* Specifiaction variable *)
  | SpecificationVariable    of DataInput.spec_variable_name

type unary_arith_op = | Abs
type binary_arith_op = | Add | Sub | Mul | Div | Mod
type nary_arith_op = | Sum
type binary_arith_cmp_op = | Lt | LEq | Eq | GEq | Gt | NEq
type binary_cstr_op = | And | Or | Impl | IfAndOnlyIf | Xor
type unary_cstr_op = | Not

type const =
  | Int of int
  | Inf of bool (* Plus and minus infinity *)

type expr =
  | Const               of const                              (* expr = integer constant *)
  | Var                 of variable                           (* expr = value of a variable *)
  | Reified             of cstr                               (* expr = if the constraint is satisfied then 1 else 0 *)
  | UnaryArithExpr      of unary_arith_op      * expr         (* expr = OP expr *)
  | BinaryArithExpr     of binary_arith_op     * expr * expr  (* expr = lexpr OP rexpr *)
  | NaryArithExpr       of nary_arith_op       * expr list    (* expr = OP (expr1, expr2, ... , expr) *)
  | BinaryArithCmpExpr  of binary_arith_cmp_op * expr * expr  (* expr = if (lexpr OP rexpr) then 1 else 0 *)

type cstr =
  | TrueCstr
  | FalseCstr
  | BinaryArithCmpCstr  of binary_arith_cmp_op * expr * expr  (* cstr : lexpr OP rexpr *)
  | BinaryCstrOpCstr    of binary_cstr_op      * cstr * cstr  (* cstr : lcstr OP rcstr *)
  | UnaryCstrOpCstr     of unary_cstr_op       * cstr         (* cstr : OP cstr' *)

type constraints = (string * (cstr list)) list
(* type constraints = { universe : cstr; specification : cstr; configuration : cstr; optimization : optimization_function } *)


(************************************)
(** 2/4 Basic Constructors          *)
(************************************)

val var2expr   : variable   -> expr
val const2expr : const -> expr
val int2expr   : int   -> expr
val reify : cstr -> expr

val ( +~ ) : expr -> expr -> expr
val ( -~ ) : expr -> expr -> expr
val ( *~ ) : expr -> expr -> expr
val ( /~ ) : expr -> expr -> expr
val ( %~ ) : expr -> expr -> expr
val abs    : expr -> expr

val sum : expr list -> expr


val truecstr  : cstr
val falsecstr : cstr

val (  <~ ) : expr -> expr -> cstr
val ( <=~ ) : expr -> expr -> cstr
val (  =~ ) : expr -> expr -> cstr
val ( >=~ ) : expr -> expr -> cstr
val (  >~ ) : expr -> expr -> cstr
val ( <>~ ) : expr -> expr -> cstr

val (  &&~~ ) : cstr -> cstr -> cstr    
val (  ||~~ ) : cstr -> cstr -> cstr    
val (  =>~~ ) : cstr -> cstr -> cstr    
val ( <=>~~ ) : cstr -> cstr -> cstr    
val xor       : cstr -> cstr -> cstr    
val not       : cstr -> cstr


(************************************)
(** 3/4 Basic Functions             *)
(************************************)

val variables_of_cstr : cstr -> variable list
val is_special_variable : variable -> bool

(************************************)
(** 4/4 string_of                   *)
(************************************)

val string_of_variable  : variable  -> string
val string_of_expr : expr -> string
val string_of_cstr : cstr -> string
val string_of_constraints : constraints -> string


