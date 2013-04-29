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
  | GlobalElementVariable    of Data_input.element
  | LocalElementVariable     of Data_input.location_name * Data_input.element
  | BindingVariable          of Data_input.port_name * Data_input.component_type_name * Data_input.component_type_name
  | LocalRepositoryVariable  of Data_input.location_name * Data_input.repository_name
  | LocalResourceVariable    of Data_input.location_name * Data_input.resource_name
  | SpecificationVariable    of Data_input.spec_variable_name

let string_of_variable variable =
  match variable with
  | GlobalElementVariable(element) -> Printf.sprintf "N(%s)" (Data_input.string_of_element element)
  | LocalElementVariable(location_name, element) -> Printf.sprintf "N(%s,%s)" location_name (Data_input.string_of_element element)
  | BindingVariable(port_name, providing_type, requiring_type) -> Printf.sprintf "B(%s,%s,%s)" port_name providing_type requiring_type
  | LocalRepositoryVariable(location_name, repository_name) -> Printf.sprintf "R(%s,%s)" location_name repository_name
  | LocalResourceVariable(location_name, resource_name) -> Printf.sprintf "O(%s,%s)" location_name resource_name
  | SpecificationVariable(spec_variable_name) -> spec_variable_name

let is_special_variable variable =
  match variable with
  | SpecificationVariable _   -> false
  | _                         -> true


(************************************)
(** 2/6 Operators                   *)
(************************************)

type unary_arith_op = Abs
type binary_arith_op = Add | Sub | Mul | Div | Mod
type nary_arith_op = Sum
type binary_arith_cmp_op = Lt | LEq | Eq | GEq | Gt | NEq
type binary_cstr_op = And | Or | Impl | IfAndOnlyIf | Xor
type unary_cstr_op = Not

let string_of_unary_arith_op op = "abs"
let string_of_binary_arith_op op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
let string_of_nary_arith_op op = "+"
let unit_of_nary_arith_op op = "0"
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
  | Xor         -> "xor"
let string_of_unary_cstr_op op = "not"



(************************************)
(** 2/6 Expressions and Constraints *)
(************************************)

type const = Int of int | Inf of bool
type expr =
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

let string_of_const const = match const with
  | Int (const) -> string_of_int const
  | Inf (plus)  -> (if plus then "" else "-") ^ "infty"
let rec string_of_expr expr = 
  match expr with
  | Const (const) -> string_of_const const
  | Var (var) -> string_of_variable var
  | Reified (cstr) -> "||" ^ (string_of_cstr cstr) ^ "||"
  | UnaryArithExpr (op, expr) -> Printf.sprintf "%s (%s)" (string_of_unary_arith_op op) (string_of_expr expr)
  | BinaryArithExpr (op, lexpr, rexpr) -> Printf.sprintf "(%s %s %s)" (string_of_expr lexpr) (string_of_binary_arith_op op) (string_of_expr rexpr)
  | NaryArithExpr (op, exprs) ->"(" ^
      (if exprs = [] then unit_of_nary_arith_op op
       else (String.concat (string_of_nary_arith_op op) (List.map string_of_expr exprs)) ) ^ ")"
and string_of_cstr cstr = 
  match cstr with
  | TrueCstr  -> "true"
  | FalseCstr -> "false"
  | BinaryArithCmpCstr (op, lexpr, rexpr) -> Printf.sprintf "(%s %s %s)" (string_of_expr lexpr) (string_of_binary_arith_cmp_op op) (string_of_expr rexpr)
  | BinaryCstrOpCstr (op, lcstr, rcstr) -> Printf.sprintf "(%s %s %s)" (string_of_cstr lcstr) (string_of_binary_cstr_op op) (string_of_cstr rcstr)
  | UnaryCstrOpCstr (op, cstr) -> Printf.sprintf "(%s %s)" (string_of_unary_cstr_op op) (string_of_cstr cstr)



type constraints = (string * (cstr list)) list





(* Building expressions *)

let var (variable_key : Variable_keys.variable_key) = NamedVar variable_key

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


(* Reified arithmetic comparisons *)

let (  <~~ )  x y  = BinaryArithCmpExpr (Lt,  x, y)
let ( <=~~ )  x y  = BinaryArithCmpExpr (LEq, x, y)
let (  =~~ )  x y  = BinaryArithCmpExpr (Eq,  x, y)
let ( >=~~ )  x y  = BinaryArithCmpExpr (GEq, x, y)
let (  >~~ )  x y  = BinaryArithCmpExpr (Gt,  x, y)
let ( <>~~ )  x y  = BinaryArithCmpExpr (NEq, x, y)


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
let xor       x y  = BinaryCstrOpCstr (Xor,         x, y)
let not       x    = UnaryCstrOpCstr  (Not,         x)


(* Extract all variable keys that appear in a constraint / expression. *)

let rec extract_variable_keys_of_cstr cstr =
  match cstr with
  | TrueCstr                              -> []
  | FalseCstr                             -> []
  | BinaryArithCmpCstr (op, lexpr, rexpr) -> (extract_variable_keys_of_expr lexpr) @ (extract_variable_keys_of_expr rexpr)
  | BinaryCstrOpCstr   (op, lcstr, rcstr) -> (extract_variable_keys_of_cstr lcstr) @ (extract_variable_keys_of_cstr rcstr)
  | UnaryCstrOpCstr    (op, cstr)         -> extract_variable_keys_of_cstr cstr

and extract_variable_keys_of_expr expr =
  match expr with
  | Const              (const)                 -> []
  | Var                (NamedVar variable_key) -> [variable_key]
  | Reified            (cstr)                  -> extract_variable_keys_of_cstr cstr
  | UnaryArithExpr     (op, expr)              -> (extract_variable_keys_of_expr expr)
  | BinaryArithExpr    (op, lexpr, rexpr)      -> (extract_variable_keys_of_expr lexpr) @ (extract_variable_keys_of_expr rexpr)
  | NaryArithExpr      (op, exprs)             -> List.flatten (List.map (fun expr -> (extract_variable_keys_of_expr expr)) exprs)
  | BinaryArithCmpExpr (op, lexpr, rexpr)      -> (extract_variable_keys_of_expr lexpr) @ (extract_variable_keys_of_expr rexpr)
