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

(* Depends on
    - datatypes/Data_model (for names)
    - datatypes/Data_common (for sets and maps of variables)
*)

open Data_model

(* 1. Variables *)

type element = 
  | Component_type of component_type_id
  | Port           of port_id
  | Package        of package_id

type variable = 
  | Simple_variable            of spec_variable_name           (** Specifiaction variable *)
  | Global_variable            of element                      (** Number of instances of a given component_type / port / package installed globally in the configuration. *)
  | Local_variable             of location_id * element        (** Number of instances of a given component_type / port / package installed on a given location. *)
  | Binding_variable           of port_id * component_type_id * component_type_id
    (** Number of bindings on the given port between the instances of the given requiring type and given providing type. *)
  | Local_repository_variable  of location_id * repository_id  (** Is the given repository installed on the given location? (boolean variable) *)
  | Local_resource_variable    of location_id * resource_id    (** How many resources of the given type are provided by the given location. *)

module Variable = struct type t = variable let compare = Pervasives.compare end
module Variable_set = Data_common.Set.Make(Variable)
module Variable_map = Data_common.Map.Make(Variable)

(* 2. Constraints *)

(* Remark: all associative and commutative operators have lists in arguement, for more efficient encoding *)


type value = Finite_value of int | Infinite_value

type arith_cmp_op = 
  | Lt  (** Less-than                 operator *)
  | LEq (** Less-than-or-equal-to     operator *)
  | Eq  (** Equal-to                  operator *)
  | GEq (** Grearter-than-or-equal-to operator *)
  | Gt  (** Greater-than              operator *)
  | NEq (** Not-equal-to              operator *)

type unary_arith_op =
  | Abs 

type binary_arith_op =
  | Add 
  | Sub 
  | Mul 
  | Div 
  | Mod 

type nary_arith_op =
  | Sum 
  | Product

type unary_konstraint_op =
  | Not 

type binary_konstraint_op =          
  | Implies        

type nary_konstraint_op =
  | And
  | Or

type expression = 
  | Constant              of value
  | Variable              of variable
  | Reified               of konstraint
  | UnaryArithExpression  of unary_arith_op  * expression
  | BinaryArithExpression of binary_arith_op * expression * expression
  | NaryArithExpression   of nary_arith_op   * expression list
and konstraint = 
  | True                                  (** Always satisfied constraint. *)
  | False
  | ArithKonstraint       of arith_cmp_op         * expression * expression
  | UnaryKonstraint       of unary_konstraint_op  * konstraint         
  | BinaryKonstraint      of binary_konstraint_op * konstraint * konstraint
  | NaryKonstraint        of nary_konstraint_op   * konstraint list

(* 3. Optimization Function *)

type optimization_function = 
  | Minimize of expression
  | Maximize of expression
  | Lexicographic of optimization_function list

(* 4. Bounds *)

type bound = { min : value; max : value }    (** Each variable has a given search space in between [min] and [max]  *)
type variable_bounds = variable -> bound  (** Function that gives for each variable its bound. Can be implemented with a map. *)


(* 5. Solutions *)

type solution = {
  domain          : Variable_set.t;
  variable_values : variable -> int; (** solution of a constraint, can be implemented with a [Variable_map.t] *)
}



let value_of_provide_arity a = match a with Data_model.Infinite_provide -> Infinite_value | Data_model.Finite_provide(i) -> Finite_value(i)
let value_of_require_arity a = Finite_value(a)
let value i = Finite_value(i)
let infinite_value = Infinite_value

let constant i = Constant(Finite_value(i))
let constant_of_provide_arity a = Constant(value_of_provide_arity a)
let constant_of_require_arity a = Constant(value_of_require_arity a)

let var2expr   v = Variable v
let const2expr c = Constant c
let int2expr   i = Constant (Finite_value i)

let ( +~ )    x y  = NaryArithExpression   ( Sum,     [x; y] )
let ( -~ )    x y  = BinaryArithExpression ( Sub,      x, y  )
let ( *~ )    x y  = NaryArithExpression   ( Product, [x; y] )
let ( /~ )    x y  = BinaryArithExpression ( Div,      x, y  )
let ( %~ )    x y  = BinaryArithExpression ( Mod,      x, y  )
let abs       x    = UnaryArithExpression  ( Abs,      x     )
let sum       l    = NaryArithExpression   ( Sum,      l     )
let prod      l    = NaryArithExpression   ( Product,  l     )
let conj      l    = NaryKonstraint        ( And,      l     )
let disj      l    = NaryKonstraint        ( Or,       l     )
let reify     cstr = Reified(cstr)

let (  <~ )   x y  = ArithKonstraint (Lt,  x, y)
let ( <=~ )   x y  = ArithKonstraint (LEq, x, y)
let (  =~ )   x y  = ArithKonstraint (Eq,  x, y)
let ( >=~ )   x y  = ArithKonstraint (GEq, x, y)
let (  >~ )   x y  = ArithKonstraint (Gt,  x, y)
let ( <>~ )   x y  = ArithKonstraint (NEq, x, y)

let true_konstraint  = True
let false_konstraint = False

let (  &&~~ ) x y  = NaryKonstraint   ( And, ([x; y]) )
let (  ||~~ ) x y  = NaryKonstraint   ( Or,  ([x; y]) )
let (  =>~~ ) x y  = BinaryKonstraint ( Implies, x, y )
let (  !~   ) x    = UnaryKonstraint  ( Not, x        )