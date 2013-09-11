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

(*/************************************************************************\*)
(*| 1. Variables                                                           |*)
(*\************************************************************************/*)

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
  | Location_used_variable     of location_id

module Variable = struct type t = variable let compare = Pervasives.compare end
module Variable_set = Data_common.Set.Make(Variable)
module Variable_map = Data_common.Map.Make(Variable)


(*/************************************************************************\*)
(*| 2. Values                                                              |*)
(*\************************************************************************/*)

type value = Finite_value of int | Infinite_value

module Value = struct
  type t = value
  
  let zero     = Finite_value 0
  let one      = Finite_value 1
  let infty    = Infinite_value

  let of_int n = Finite_value n
  let of_provide_arity p = match p with | Finite_provide(n) -> of_int n | Infinite_provide -> infty
  let of_require_arity = of_int
  let of_resource_provide_arity = of_int

  let min v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (min n1 n2)
    | (Finite_value _ , _              ) -> v1
    | (_              , Finite_value _ ) -> v2
    | _  -> infty
  let mins l = Data_common.List.fold_combine (fun x -> x) min l infty

  let max v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (max n1 n2)
    | _ -> Infinite_value
  let maxs l = Data_common.List.fold_combine (fun x -> x) max l zero

  let sum v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (n1 + n2)
    | _ -> Infinite_value
  let sums l = Data_common.List.fold_combine (fun x -> x) sum l zero

  let prod v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (n1 * n2)
    | _ -> Infinite_value
  let prods l = Data_common.List.fold_combine (fun x -> x) prod l one
  
  let sub v1 v2 = match (v1, v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (Pervasives.max 0 (n1 - n2)) (* we want to stay positive :p *)
    | (Finite_value _ , _              ) -> zero
    | (_              , Finite_value _ ) -> infty
    | _  -> zero

  let div v1 v2 = match (v1, v2) with
    | (Finite_value n1, Finite_value n2) -> of_int ((n1 / n2) + (if (n1 mod n2) > 0 then 1 else 0)) (* rounded up, to match bound computation *)
    | (Finite_value _ , _              ) -> zero
    | (_              , Finite_value _ ) -> infty
    | _  -> of_int 1
  
  let is_finite v = (v != Infinite_value)
  let is_infty  v = (v  = Infinite_value)
  
  let is_sup v1 v2 = match (v1, v2) with
    | (Finite_value n1, Finite_value n2) -> v1 > v2
    | (Finite_value _ , _              ) -> false
    | (_              , Finite_value _ ) -> true
    | _  -> false
    
  let is_inf_eq v1 v2 = match (v1, v2) with
    | (Finite_value n1, Finite_value n2) -> v1 <= v2
    | (Finite_value _ , _              ) -> true
    | (_              , Finite_value _ ) -> false
    | _  -> true
  
  let int_of v default = match v with | Finite_value v -> v | Infinite_value -> default
  let int_of_unsafe v  = match v with | Finite_value v -> v | Infinite_value -> raise (Failure "cannot give a normal value for an infinite one")
  
end


(*/************************************************************************\*)
(*| 3. Constraints                                                         |*)
(*\************************************************************************/*)

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

let constant i = Constant(Value.of_int i)
let constant_of_provide_arity a = Constant(Value.of_provide_arity a)
let constant_of_require_arity a = Constant(Value.of_require_arity a)

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


let rec variables_of_expression e = match e with
  | Constant(v)                     -> Variable_set.empty
  | Variable(v)                     -> Variable_set.singleton v
  | Reified(c)                      -> variables_of_konstraint c
  | UnaryArithExpression  (_,e')    -> variables_of_expression e'
  | BinaryArithExpression (_,e1,e2) -> Variable_set.union (variables_of_expression e1) (variables_of_expression e2)
  | NaryArithExpression   (_,l)     -> List.fold_left (fun vars e -> Variable_set.union (variables_of_expression e) vars) Variable_set.empty l
and variables_of_konstraint c = match c with
  | True            -> Variable_set.empty
  | False           -> Variable_set.empty
  | ArithKonstraint  (_,e1,e2) -> Variable_set.union (variables_of_expression e1) (variables_of_expression e2)
  | UnaryKonstraint  (_,c')    -> variables_of_konstraint c'
  | BinaryKonstraint (_,c1,c2) -> Variable_set.union (variables_of_konstraint c1) (variables_of_konstraint c2)
  | NaryKonstraint   (_,l)     -> List.fold_left (fun vars e -> Variable_set.union (variables_of_konstraint e) vars) Variable_set.empty l



(*/************************************************************************\*)
(*| 4. Optimization Functions                                              |*)
(*\************************************************************************/*)

type optimization_function = 
  | Minimize of expression
  | Maximize of expression
  | Lexicographic of optimization_function list

let rec variables_of_optimization_function f = match f with
  | Minimize (e) -> variables_of_expression e
  | Maximize (e) -> variables_of_expression e
  | Lexicographic (l) -> List.fold_left (fun res f' -> Variable_set.union (variables_of_optimization_function f') res) Variable_set.empty l

(*/************************************************************************\*)
(*| 5. Bounds for Variables                                                |*)
(*\************************************************************************/*)

module Bound = struct
  type t = { min : value; max : value }   (** Each variable has a given search space in between [min] and [max]  *)

  let create v1 v2 = { min = v1; max = v2 }
  let singleton v = { min = v; max = v }
  let combine b1 b2 = { min = Value.max b1.min b2.min; max = Value.min b1.max b2.max }
  let add_min b v = { min = Value.max b.min v; max = b.max }
  let add_max b v = { min = b.min; max = Value.min b.max v }
  let ext_max b v = { min = b.min; max = if b.max = Value.infty then v else Value.max b.max v }

  let min b = b.min
  let max b = b.max

  let is_empty b = Value.is_sup b.min b.max
 
  let null  = create Value.zero Value.zero 
  let small = create Value.zero Value.one
  let big   = create Value.zero Value.infty
end

type variable_bounds = variable -> Bound.t  (** Function that gives for each variable its bound. Can be implemented with a map. *)


(*/************************************************************************\*)
(*| 6. Solution of Constraints                                             |*)
(*\************************************************************************/*)

type solution = {
  domain          : Variable_set.t;
  variable_values : variable -> int; (** solution of a constraint, can be implemented with a [Variable_map.t] *)
}

let is_empty sol = 
  Variable_set.fold (fun v res -> if not res then (if (sol.variable_values v) != 0 then false else res) else res) sol.domain true

(*
let value_of_provide_arity a = match a with Data_model.Infinite_provide -> Infinite_value | Data_model.Finite_provide(i) -> Finite_value(i)
let value_of_require_arity a = Finite_value(a)
let value i = Finite_value(i)
let infinite_value = Infinite_value
*)


