
open Resource_types_t

open Typing_context
open Variable_keys
open Generic_constraints


let create_specification_constraints specification : cstr list =
  try
  List.map ( fun (specification_element, specification_constraint) ->
    let variable =
      match specification_element with
        `Component domain_element ->
          var (DomainVariable domain_element)
      | `Port port ->
          var (PortVariable port)
    in
    match specification_constraint with
      `Lt  n -> (var2expr variable) <~  (const2expr n)
    | `LEq n -> (var2expr variable) <=~ (const2expr n)
    | `Eq  n -> (var2expr variable) =~  (const2expr n)
    | `GEq n -> (var2expr variable) >=~ (const2expr n)
    | `Gt  n -> (var2expr variable) >~  (const2expr n)
  ) specification
  with
  | Not_found -> 
      failwith "specification concerns an element which does not exist it the typing context!"
