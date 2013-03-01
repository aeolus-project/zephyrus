
open Helpers

open Aeolus_types_j

open Typing_context
open Variable_keys

open Aeolus_types_output_facade.Aeolus_types_plain_output

let string_of_element element =
  match element with
  | ComponentType (component_type_name) -> Printf.sprintf "component_type_%s" (string_of_component_type_name component_type_name)
  | Port          (port_name)           -> Printf.sprintf "port_%s"           (string_of_port_name           port_name)
  | Package       (package_name)        -> Printf.sprintf "package_%s"        (string_of_package_name        package_name)

let string_of_variable_key variable_key =
  match variable_key with
  | GlobalElementVariable   (element) ->
      Printf.sprintf 
        "global_element_%s" 
        (string_of_element element)

  | LocalElementVariable    (location_name, element) ->
      Printf.sprintf
        "local_element_%s_%s"
        (string_of_location_name location_name)
        (string_of_element element)

  | BindingVariable         (port_name, providing_component_type_name, requiring_component_type_name) ->
      Printf.sprintf
        "binding_%s_%s_%s"      
        (string_of_port_name           port_name)
        (string_of_component_type_name providing_component_type_name)
        (string_of_component_type_name requiring_component_type_name)

  | LocalRepositoryVariable (location_name, repository_name) ->
      Printf.sprintf
        "local_repository_%s_%s"
        (string_of_location_name   location_name)
        (string_of_repository_name repository_name)

  | LocalResourceVariable   (location_name, resource_name) ->
      Printf.sprintf
        "local_resource_%s_%s"
        (string_of_location_name location_name)
        (string_of_resource_name resource_name)

  | SpecificationVariable   (spec_variable_name) ->
      Printf.sprintf 
        "spec_var_%s"
        (string_of_spec_variable_name spec_variable_name)


module C = Generic_constraints

let rec string_of_unary_arith_op op =
  match op with
  | C.Abs -> "abs"

and string_of_binary_arith_op op =
  match op with
  | C.Add -> "+"
  | C.Sub -> "-"
  | C.Mul -> "*"
  | C.Div -> "div"
  | C.Mod -> "mod"

and string_of_nary_arith_op op =
  match op with
  | C.Sum -> "+"

and unit_of_nary_arith_op op =
  match op with
  | C.Sum -> "0"

and string_of_binary_arith_cmp_op op =
  match op with
  | C.Lt  -> "<"
  | C.LEq -> "<="
  | C.Eq  -> "="
  | C.GEq -> ">="
  | C.Gt  -> ">"
  | C.NEq -> "<>"

and string_of_binary_cstr_op op =
  match op with
  | C.And         -> "/\\"
  | C.Or          -> "\\/"
  | C.Impl        -> "->"
  | C.IfAndOnlyIf -> "<->"
  | C.Xor         -> "xor" (* TODO *)

and string_of_unary_cstr_op op =
  match op with
  | C.Not -> "not"

and string_of_var var =
  match var with
  | C.NamedVar  (var_key) -> string_of_variable_key var_key
  
and string_of_expr expr = 
  match expr with
  | C.Const (const) -> 
    (
      match const with
      | C.Int (const) -> Printf.sprintf "%d" const
      | C.Inf (plus)  -> Printf.sprintf "%s1000" (if plus then "" else "-") 
    )

  | C.Var (var) ->
      Printf.sprintf "%s" (string_of_var var)

  | C.Reified (cstr) ->
      Printf.sprintf "(bool2int %s)" (string_of_cstr cstr)
  
  | C.UnaryArithExpr (op, expr) ->
      Printf.sprintf "(%s (%s))"
      (string_of_unary_arith_op op)
      (string_of_expr expr)

  | C.BinaryArithExpr (op, lexpr, rexpr) ->
      Printf.sprintf "(%s %s %s)" 
      (string_of_expr lexpr)
      (string_of_binary_arith_op op)
      (string_of_expr rexpr)

  | C.NaryArithExpr (op, exprs) ->
      Printf.sprintf "(%s)"
      (if exprs = [] 
       then unit_of_nary_arith_op op
       else
         (String.concat
           (Printf.sprintf " %s " (string_of_nary_arith_op op))
           (List.map string_of_expr exprs) ) )

  | C.BinaryArithCmpExpr (op, lexpr, rexpr) ->
      Printf.sprintf "(bool2int %s)"
      (string_of_cstr (C.BinaryArithCmpCstr (op, lexpr, rexpr)))


and string_of_cstr cstr = 
  match cstr with
  | C.TrueCstr  -> "true"  (* TODO *)
  | C.FalseCstr -> "false" (* TODO *)

  | C.BinaryArithCmpCstr (op, lexpr, rexpr) ->
      Printf.sprintf "(%s %s %s)" 
      (string_of_expr lexpr)
      (string_of_binary_arith_cmp_op op)
      (string_of_expr rexpr)

  | C.BinaryCstrOpCstr (op, lcstr, rcstr) ->
      Printf.sprintf "(%s %s %s)" 
      (string_of_cstr lcstr)
      (string_of_binary_cstr_op op)
      (string_of_cstr rcstr)

  | C.UnaryCstrOpCstr (op, cstr) ->
      Printf.sprintf "(%s (%s))"
      (string_of_unary_cstr_op op)
      (string_of_cstr cstr)