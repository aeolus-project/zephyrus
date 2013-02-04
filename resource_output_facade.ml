
open Helpers

(*
module type RESOURCE_TYPES =
  sig
    type resource_name
    type resource_type
    type resource
    type typing_entry
    type binding
    type typed_system
  end

module My_resource_types : RESOURCE_TYPES = Resource_generation
*)

module T = Resource_generation

module type RESOURCE_OUTPUT =
  (* functor (T : RESOURCE_TYPES) -> *)
  sig
    val string_of_resource_name : T.resource_name -> string
    val string_of_resource_type : T.resource_type -> string
    val string_of_resource      : T.resource      -> string
    val string_of_binding       : T.binding       -> string
    val string_of_typing_entry  : T.typing_entry  -> string
    val string_of_typed_system  : T.typed_system  -> string
  end

module Simple_resource_output : RESOURCE_OUTPUT =
  struct

    open Resource_generation

    let string_of_resource_name (resource_name : resource_name) : string =
      resource_name

    let string_of_resource_type (resource_type : resource_type) : string =
      Typing_context.string_of_domain_element resource_type

    let string_of_resource (resource : resource) : string =
      let string_of_port_with_arity =
        fun ( (port_name, arity) : (string * int) ) ->
        Printf.sprintf "(%s:%d)" port_name arity
      in
      Printf.sprintf "%s" resource.resource_name
      
      (* (* More data: *)
      Printf.sprintf
      "%s | Requires: %s | Provides: %s" 
      resource.resource_name
      (String.concat " " (List.map string_of_port_with_arity resource.resource_requires))
      (String.concat " " (List.map string_of_port_with_arity resource.resource_provides))
      *)
    
    let string_of_binding (binding : binding) : string =
      Printf.sprintf
        "(%s) %s -> %s"
        binding.binding_port
        binding.binding_from
        binding.binding_to
    
    let string_of_typing_entry : typing_entry -> string =
      fun (resource_name, resource_type) -> 
      Printf.sprintf
        "(%s : %s)"
        (string_of_resource_name resource_name)
        (string_of_resource_type resource_type)

    let string_of_typed_system (system : typed_system) : string =
      Printf.sprintf 
        "\nTyped System:\n\nThe resources:\n%s\n\nThe bindings:\n%s\n"
        (lines_of_strings (List.map string_of_resource (system.resources)))
        (lines_of_strings (List.map string_of_binding  (system.bindings)))
  
  end


module JSON_resource_output : RESOURCE_OUTPUT =
  struct

    module O = Resource_types_j

    open Resource_generation

    let string_of_resource_name (resource_name : resource_name) : string =
      O.string_of_resource_name resource_name

    let string_of_resource_type (resource_type : resource_type) : string =
      O.string_of_resource_type_and_state resource_type

    let convert_resource (resource : resource) : O.resource = 
      {
        O.resource_name          = resource.resource_name;
        O.resource_require_ports = resource.resource_requires;
        O.resource_provide_ports = resource.resource_provides;
      }

    let string_of_resource (resource : resource) : string = 
      O.string_of_resource (convert_resource resource)

    let convert_binding (binding : binding) : O.binding =
      {
        O.binding_port = binding.binding_port;
        O.binding_from = binding.binding_from;
        O.binding_to   = binding.binding_to;
      }

    let string_of_binding (binding : binding) : string =
      O.string_of_binding (convert_binding binding)

    let convert_typing_entry : typing_entry -> O.typing_entry =
      fun (resource_name, resource_type) -> 
      {
        O.typing_entry_resource_name = resource_name;
        O.typing_entry_resource_type = resource_type;
      }

    let string_of_typing_entry (typing_entry : typing_entry) : string =
      O.string_of_typing_entry (convert_typing_entry typing_entry)

    let convert_typed_system (typed_system : typed_system) : O.typed_system =
      {  
        O.typed_system_typing_environment = List.map convert_typing_entry typed_system.typing_environment;
        O.typed_system_resources          = List.map convert_resource     typed_system.resources;
        O.typed_system_bindings           = List.map convert_binding      typed_system.bindings;
      }

    let string_of_typed_system (typed_system : typed_system) : string =
      O.string_of_typed_system (convert_typed_system typed_system)

  end
