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
(** 1/6 Variable                    *)
(************************************)

type variable =
  | GlobalElementVariable    of element
  (** Number of instances of a given component_type / port / package installed globally in the configuration. *)

  | LocalElementVariable     of location_name * element
  (** Number of instances of a given component_type / port / package installed on a given location. *)

  | BindingVariable          of port_name * component_type_name * component_type_name
  (** Number of bindings on the given port between the instances of the given requiring type and given providing type. *)

  | LocalRepositoryVariable  of location_name * repository_name
  (** Is the given repository installed on the given location? (boolean variable) *)

  | LocalResourceVariable    of location_name * resource_name
  (** How many resources of the given type are provided by the given location. *)

  | SpecificationVariable    of spec_variable_name
  (** Specifiaction variable *)


let string_of_variable variable =
  match variable with
  | GlobalElementVariable   (element) ->
      Printf.sprintf
        "N(%s)"
        (string_of_element element)

  | LocalElementVariable    (location_name, element) ->
      Printf.sprintf 
        "N(%s,%s)"
        (string_of_location_name location_name)
        (string_of_element       element)

  | BindingVariable         (port_name, providing_component_type_name, requiring_component_type_name) ->
      Printf.sprintf 
        "B(%s,%s,%s)"
        (string_of_port_name           port_name)
        (string_of_component_type_name providing_component_type_name)
        (string_of_component_type_name requiring_component_type_name)

  | LocalRepositoryVariable (location_name, repository_name) ->
      Printf.sprintf 
        "R(%s,%s)"
        (string_of_location_name   location_name)
        (string_of_repository_name repository_name)

  | LocalResourceVariable   (location_name, resource_name) ->
      Printf.sprintf 
        "O(%s,%s)"
        (string_of_location_name location_name)
        (string_of_resource_name resource_name)

  | SpecificationVariable   (spec_variable_name) ->
      Printf.sprintf
        "S(%s)"
        (string_of_spec_variable_name spec_variable_name)



(************************************)
(** 2/6 Operators                   *)
(************************************)

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


let string_of_unary_arith_op op =
  match op with
  | Abs -> "abs"

let string_of_binary_arith_op op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"

let string_of_nary_arith_op op =
  match op with
  | Sum -> "+"

let unit_of_nary_arith_op op =
  match op with
  | Sum -> "0"

let string_of_binary_arith_cmp_op op =
  match op with
  | Lt  -> "<"
  | LEq -> "<="
  | Eq  -> "="
  | GEq -> ">="
  | Gt  -> ">"
  | NEq -> "<>"

let string_of_binary_cstr_op op =
  match op with
  | And         -> "and"
  | Or          -> "or"
  | Impl        -> "=>"
  | IfAndOnlyIf -> "<=>"

let string_of_unary_cstr_op op =
  match op with
  | Not -> "not"



(************************************)
(** 2/6 Expressions and Constraints *)
(************************************)

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


let string_of_const const =
  match const with
  | Int (const) -> Printf.sprintf "%d" const
  | Inf (plus)  -> Printf.sprintf "%sINFINITY" (if plus then "" else "NEG_")

let string_of_var var =
  Variables.string_of_variable var
  
let rec string_of_expr expr = 
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


type constraints = (string * (cstr list)) list





(* Building expressions *)

let var (variable : Variables.variable) = variable

let var2expr   (var   : var)   : expr = Var var
let const2expr (const : const) : expr = Const const
let int2expr   (const : int)   : expr = Const (Int const)

(** Building constraints *)

let truecstr  = TrueCstr
let falsecstr = FalseCstr

(** Reification *)

let reify cstr = Reified cstr


(** Arithmetic operators *)

let ( +~ )    x y  = BinaryArithExpr (Add, x, y)
let ( -~ )    x y  = BinaryArithExpr (Sub, x, y)
let ( *~ )    x y  = BinaryArithExpr (Mul, x, y)
let ( /~ )    x y  = BinaryArithExpr (Div, x, y)
let ( %~ )    x y  = BinaryArithExpr (Mod, x, y)
let abs       x    = UnaryArithExpr  (Abs, x)


let sum exprs_to_sum = NaryArithExpr (Sum, exprs_to_sum)


(** Arithmetic comparisons *)

let (  <~ )   x y  = BinaryArithCmpCstr (Lt,  x, y)
let ( <=~ )   x y  = BinaryArithCmpCstr (LEq, x, y)
let (  =~ )   x y  = BinaryArithCmpCstr (Eq,  x, y)
let ( >=~ )   x y  = BinaryArithCmpCstr (GEq, x, y)
let (  >~ )   x y  = BinaryArithCmpCstr (Gt,  x, y)
let ( <>~ )   x y  = BinaryArithCmpCstr (NEq, x, y)


(** Constraint operators *)

let (  &&~~ ) x y  = BinaryCstrOpCstr (And,         x, y)
let (  ||~~ ) x y  = BinaryCstrOpCstr (Or,          x, y)
let (  =>~~ ) x y  = BinaryCstrOpCstr (Impl,        x, y)
let ( <=>~~ ) x y  = BinaryCstrOpCstr (IfAndOnlyIf, x, y)
let not       x    = UnaryCstrOpCstr  (Not,         x)


(** Extract all variable keys that appear in a constraint / expression. *)

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
  | Var                (variable)         -> [variable]
  | Reified            (cstr)             -> extract_variables_of_cstr cstr
  | UnaryArithExpr     (op, expr)         -> (extract_variables_of_expr expr)
  | BinaryArithExpr    (op, lexpr, rexpr) -> (extract_variables_of_expr lexpr) @ (extract_variables_of_expr rexpr)
  | NaryArithExpr      (op, exprs)        -> List.flatten (List.map (fun expr -> (extract_variables_of_expr expr)) exprs)