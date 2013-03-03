
open Helpers

open Aeolus_types_j

open Typing_context
open Variable_keys

open Aeolus_types_output_facade.Aeolus_types_plain_output


let sanitize_name name =
  let lowercased_name = String.lowercase name
  in
  BatString.filter_map (fun c ->
    match c with 
    | 'a'..'z' -> Some c 
    | '0'..'9' -> Some c
    | '@'
    | ' '      -> None
    | _        -> Some '_'
  ) lowercased_name

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
        (sanitize_name (string_of_element element))

  | LocalElementVariable    (location_name, element) ->
      Printf.sprintf
        "local_element_%s_%s"
        (sanitize_name (string_of_location_name location_name))
        (sanitize_name (string_of_element element))

  | BindingVariable         (port_name, providing_component_type_name, requiring_component_type_name) ->
      Printf.sprintf
        "binding_%s_%s_%s"      
        (sanitize_name (string_of_port_name           port_name))
        (sanitize_name (string_of_component_type_name providing_component_type_name))
        (sanitize_name (string_of_component_type_name requiring_component_type_name))

  | LocalRepositoryVariable (location_name, repository_name) ->
      Printf.sprintf
        "local_repository_%s_%s"
        (sanitize_name (string_of_location_name   location_name))
        (sanitize_name (string_of_repository_name repository_name))

  | LocalResourceVariable   (location_name, resource_name) ->
      Printf.sprintf
        "local_resource_%s_%s"
        (sanitize_name (string_of_location_name location_name))
        (sanitize_name (string_of_resource_name resource_name))

  | SpecificationVariable   (spec_variable_name) ->
      Printf.sprintf 
        "spec_var_%s"
        (sanitize_name (string_of_spec_variable_name spec_variable_name))

let create_minizinc_variable variable_key =
  (variable_key, string_of_variable_key variable_key)

let create_minizinc_variables variable_keys =
  List.map create_minizinc_variable variable_keys

let get_minizinc_variable minizinc_variables variable_key =
  List.assoc variable_key minizinc_variables

let get_minizinc_variable_reverse minizinc_variables minizinc_variable =
  BatList.assoc_inv minizinc_variable minizinc_variables

module C = Generic_constraints

let minizinc_max_int = 10000

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
      | C.Inf (plus)  -> Printf.sprintf "%s%d" (if plus then "" else "-") minizinc_max_int
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
  | C.TrueCstr  -> "true" 
  | C.FalseCstr -> "false"

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


let translate_constraints minizinc_variables cstrs minimize_expr =
  let strings_of_variables =
    List.map (fun (key, var) -> 
      match Variables.variable_kind_of_variable_key key with 
      | Variables.BooleanVariable -> Printf.sprintf "var 0..1: %s;\n" var
      | Variables.NaturalVariable -> Printf.sprintf "var 0..10000: %s;\n" var
    ) minizinc_variables
  in
  let strings_of_constraints =
    List.map (fun cstr ->
      Printf.sprintf "constraint %s;\n" (string_of_cstr cstr)
    ) cstrs
  in
  let solve_string =
    Printf.sprintf "solve minimize %s;" (string_of_expr minimize_expr)
  in
  let output_variable_strings =
    List.map (fun (key, var) ->
      Printf.sprintf "\"%s = \" , show(%s), \";\\n\"" (string_of_variable_key key) (get_minizinc_variable minizinc_variables key)
    ) minizinc_variables
  in
  let output_string =
    Printf.sprintf
      "output [ \n %s ];"
      (String.concat ",\n" output_variable_strings)
  in
  Printf.sprintf
    "\n%s\n\n%s\n\n%s\n\n%s\n" 
    (String.concat "" strings_of_variables) 
    (String.concat "" strings_of_constraints) 
    solve_string
    output_string

let solution_of_bound_minizinc_variables minizinc_variables (bound_variables : (string * int) list) : Solution.solution =
  List.map ( fun (minizinc_variable, value) ->
    (get_minizinc_variable_reverse minizinc_variables minizinc_variable, value)
  ) bound_variables
