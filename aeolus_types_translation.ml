
open Helpers

module X = Aeolus_types
module Y = Aeolus_types_t

let component_type_name_translate component_type_name = component_type_name
let port_name_translate port_name = port_name
let package_name_translate package_name = package_name
let repository_name_translate repository_name = repository_name
let resource_name_translate resource_name = resource_name

let provide_arity_translate provide_arity =
  match provide_arity with
  | `InfiniteProvide -> X.InfiniteProvide
  | `FiniteProvide i -> X.FiniteProvide i

let require_arity_translate require_arity = require_arity
let resource_consumption_translate resource_consumption = resource_consumption
let resource_provide_arity_translate resource_provide_arity = resource_provide_arity

exception Double_provide  of Y.component_type_name * Y.port_name
exception Double_require  of Y.component_type_name * Y.port_name
exception Double_conflict of Y.component_type_name * Y.port_name
exception Double_consume  of Y.component_type_name * Y.resource_name

let component_type_translate component_type = {
  X.component_type_name = component_type_name_translate component_type.Y.component_type_name;

  X.component_type_provide = (
    let module T = Map_of_assoc_list(X.Port_name_map) in
    try
      T.translate port_name_translate provide_arity_translate component_type.Y.component_type_provide
    with
      T.Double_key port_name -> raise (Double_provide (component_type.Y.component_type_name, port_name)) );
  
  X.component_type_require = (
    let module T = Map_of_assoc_list(X.Port_name_map) in
    try
      T.translate port_name_translate require_arity_translate component_type.Y.component_type_require
    with
      T.Double_key port_name -> raise (Double_require (component_type.Y.component_type_name, port_name)) );
  
  X.component_type_conflict = (
    let module T = Set_of_list(X.Port_name_set) in
    try
      T.translate port_name_translate component_type.Y.component_type_conflict
    with 
      T.Double_element port_name -> raise (Double_conflict (component_type.Y.component_type_name, port_name)) );
  
  X.component_type_consume = (
    let module T = Map_of_assoc_list(X.Resource_name_map) in
    try
      T.translate resource_name_translate resource_consumption_translate component_type.Y.component_type_consume
    with
      T.Double_key resource_name -> raise (Double_consume (component_type.Y.component_type_name, resource_name)) );
}

exception Double_package_name of X.package_name
exception Double_package_consume of X.package_name * X.resource_name
exception Double_package_disjunction of X.Package_name_set.t
exception Double_package_conflict of X.package_name * X.package_name

let package_translate package = {
  X.package_name     = package_name_translate package.Y.package_name;

  X.package_depend   = (
    let package_names_translate package_names =
      let module T = Set_of_list(X.Package_name_set) in
      try
        T.translate package_name_translate package_names
      with
        T.Double_element package_name -> raise (Double_package_name package_name)
    in
      let module T = Set_of_list(X.Package_name_set_set) in
      try
        T.translate package_names_translate package.Y.package_depend
      with
        T.Double_element package_names -> raise (Double_package_disjunction package_names) );
      
  X.package_conflict = (
    let module T = Set_of_list(X.Package_name_set) in
    try
      T.translate package_name_translate package.Y.package_conflict
    with 
      T.Double_element package_name -> raise (Double_package_conflict (package.Y.package_name, package_name)) );

  X.package_consume  = (
    let module T = Map_of_assoc_list(X.Resource_name_map) in
    try
      T.translate resource_name_translate resource_consumption_translate package.Y.package_consume
    with
      T.Double_key resource_name -> raise (Double_package_consume (package.Y.package_name, resource_name)) );
}

exception Double_package_nameInRepository of Y.repository_name * Y.package_name

let repository_translate repository = {
  X.repository_name     = repository_name_translate repository.Y.repository_name;

  X.repository_packages = (
    let module T = Map_of_list(X.Package_name_map) in
    try
      T.translate (fun package -> package.Y.package_name) package_translate repository.Y.repository_packages
    with
      T.Double_key package_name -> raise (Double_package_nameInRepository (repository.Y.repository_name, package_name)) );
}

exception Double_component_type_name_in_universe of Y.component_type_name
exception Double_implementation_of_a_component_in_universe of Y.component_type_name
exception Double_package_nameInImplementation of Y.component_type_name * Y.package_name (* TODO: There is no way to produce this exception, because when we want to throw it we have no information about the component type name... *)
exception Double_repository_name_in_universe of Y.repository_name

let universe_translate universe = {
  X.universe_component_types = (
    let module T = Map_of_list(X.Component_type_name_map) in
    try
      T.translate (fun component_type -> component_type.Y.component_type_name) component_type_translate universe.Y.universe_component_types
    with
      T.Double_key component_type_name -> raise (Double_component_type_name_in_universe component_type_name) );

  X.universe_implementation = (
    let package_names_translate package_names =
      let module T = Set_of_list(X.Package_name_set) in
      try
        T.translate package_name_translate package_names
      with
        T.Double_element package_name -> raise (Double_package_name package_name)
    in
      let module T = Map_of_assoc_list(X.Component_type_name_map) in
      try
        T.translate component_type_name_translate package_names_translate universe.Y.universe_implementation
      with
        T.Double_key component_type_name -> raise (Double_implementation_of_a_component_in_universe (component_type_name)) );

  X.universe_repositories = (
    let module T = Map_of_list(X.Repository_name_map) in
    try
      T.translate (fun repository -> repository.Y.repository_name) repository_translate universe.Y.universe_repositories
    with
      T.Double_key repository_name -> raise (Double_repository_name_in_universe repository_name) );
}

let location_name_translate location_name = location_name
let component_name_translate component_name = component_name

exception Double_location_resource_provide of Y.location_name * Y.resource_name
exception Double_location_package_installed of Y.location_name * Y.package_name

let location_translate location = {
  X.location_name = location_name_translate location.Y.location_name;

  X.location_provide_resources  = (
    let module T = Map_of_assoc_list(X.Resource_name_map) in
    try
      T.translate resource_name_translate resource_provide_arity_translate location.Y.location_provide_resources
    with
      T.Double_key resource_name -> raise (Double_location_resource_provide (location.Y.location_name, resource_name)) );

  X.location_repository = repository_name_translate location.Y.location_repository;

  X.location_packages_installed = (
    let module T = Set_of_list(X.Package_name_set) in
    try
      T.translate package_name_translate location.Y.location_packages_installed
    with
      T.Double_element package_name -> raise (Double_location_package_installed (location.Y.location_name, package_name)) );
}

let component_translate component = {
  X.component_name     = component_name_translate      component.Y.component_name;
  X.component_type     = component_type_name_translate component.Y.component_type;
  X.component_location = location_name_translate       component.Y.component_location;
}

let binding_translate binding = {
  X.binding_port     = port_name_translate      binding.Y.binding_port;
  X.binding_requirer = component_name_translate binding.Y.binding_requirer;
  X.binding_provider = component_name_translate binding.Y.binding_provider;
}

exception Double_location_name_in_configuration of X.location_name
exception Double_component_name_in_configuration of X.component_name
exception Double_binding_in_configuration of X.binding

let configuration_translate configuration = {
  X.configuration_locations  = (
    let module T = Map_of_list(X.Location_name_map) in
    try
      T.translate (fun location -> location.Y.location_name) location_translate configuration.Y.configuration_locations
    with
      T.Double_key location_name -> raise (Double_location_name_in_configuration location_name) );

  X.configuration_components = (
    let module T = Map_of_list(X.Component_name_map) in
    try
      T.translate (fun component -> component.Y.component_name) component_translate configuration.Y.configuration_components
    with
      T.Double_key component_name -> raise (Double_component_name_in_configuration component_name) );

  X.configuration_bindings = (
    let module T = Set_of_list(X.Binding_set) in
    try
      T.translate binding_translate configuration.Y.configuration_bindings
    with
      T.Double_element binding -> raise (Double_binding_in_configuration binding));
}





(** ==== SPECIFICATION ==== *)

let spec_variable_name_translate spec_variable_name = spec_variable_name

let spec_const_translate spec_const = spec_const

let spec_local_element_translate spec_local_element =
  match spec_local_element with 
  | `SpecLocalElementPackage (package_name) -> X.SpecLocalElementPackage (package_name_translate package_name)
  | `SpecLocalElementComponentType (component_type_name) -> X.SpecLocalElementComponentType (component_type_name_translate component_type_name)
  | `SpecLocalElementPort (port_name) -> X.SpecLocalElementPort (port_name_translate port_name)

let rec spec_local_expr_translate spec_local_expr =
  match spec_local_expr with 
  | `SpecLocalExprVar (spec_variable_name) -> X.SpecLocalExprVar (spec_variable_name_translate spec_variable_name)
  | `SpecLocalExprConst (spec_const) -> X.SpecLocalExprConst (spec_const_translate spec_const)
  | `SpecLocalExprArity (spec_local_element) -> X.SpecLocalExprArity (spec_local_element_translate spec_local_element)
  | `SpecLocalExprAdd (l_spec_local_expr, r_spec_local_expr) -> X.SpecLocalExprAdd (spec_local_expr_translate l_spec_local_expr, spec_local_expr_translate r_spec_local_expr)
  | `SpecLocalExprSub (l_spec_local_expr, r_spec_local_expr) -> X.SpecLocalExprSub (spec_local_expr_translate l_spec_local_expr, spec_local_expr_translate r_spec_local_expr)
  | `SpecLocalExprMul (spec_const, spec_local_expr) -> X.SpecLocalExprMul (spec_const_translate spec_const, spec_local_expr_translate spec_local_expr)

let spec_op_translate spec_op =
  match spec_op with 
  | `Lt   -> X.Lt 
  | `LEq  -> X.LEq 
  | `Eq   -> X.Eq 
  | `GEq  -> X.GEq 
  | `Gt   -> X.Gt 
  | `NEq  -> X.NEq

let rec local_specification_translate local_specification =
  match local_specification with 
  | `SpecLocalTrue -> X.SpecLocalTrue
  | `SpecLocalOp (l_spec_local_expr, spec_op, r_spec_local_expr) -> X.SpecLocalOp (spec_local_expr_translate l_spec_local_expr, spec_op_translate spec_op, spec_local_expr_translate r_spec_local_expr)
  | `SpecLocalAnd (l_local_specification, r_local_specification) -> X.SpecLocalAnd (local_specification_translate l_local_specification, local_specification_translate r_local_specification)
  | `SpecLocalOr (l_local_specification, r_local_specification) -> X.SpecLocalOr (local_specification_translate l_local_specification, local_specification_translate r_local_specification)
  | `SpecLocalImpl (l_local_specification, r_local_specification) -> X.SpecLocalImpl (local_specification_translate l_local_specification, local_specification_translate r_local_specification)
  | `SpecLocalNot (local_specification) -> X.SpecLocalNot (local_specification_translate local_specification)

let spec_repository_constraint_translate spec_repository_constraint = 
  repository_name_translate spec_repository_constraint

let spec_repository_constraints_translate spec_repository_constraints =
  List.map spec_repository_constraint_translate spec_repository_constraints

let spec_resource_constraint_translate spec_resource_constraint =
  let (resource_name, spec_op, spec_const) = spec_resource_constraint in
  (resource_name_translate resource_name, spec_op_translate spec_op, spec_const_translate spec_const)

let spec_resource_constraints_translate spec_resource_constraints =
  List.map spec_resource_constraint_translate spec_resource_constraints

let spec_element_translate spec_element =
  match spec_element with
  | `SpecElementPackage (package_name) -> X.SpecElementPackage (package_name_translate package_name)
  | `SpecElementComponentType (component_type_name) -> X.SpecElementComponentType (component_type_name_translate component_type_name)
  | `SpecElementPort (port_name) -> X.SpecElementPort (port_name_translate port_name)
  | `SpecElementLocalisation (spec_resource_constraints, spec_repository_constraints, local_specification) -> 
      X.SpecElementLocalisation (((spec_resource_constraints_translate spec_resource_constraints), (spec_repository_constraints_translate spec_repository_constraints), (local_specification_translate local_specification)))

let rec spec_expr_translate spec_expr =
  match spec_expr with 
  | `SpecExprVar (spec_variable_name) -> X.SpecExprVar (spec_variable_name_translate spec_variable_name)
  | `SpecExprConst (spec_const) -> X.SpecExprConst (spec_const_translate spec_const)
  | `SpecExprArity (spec_element) -> X.SpecExprArity (spec_element_translate spec_element)
  | `SpecExprAdd (l_spec_expr, r_spec_expr) -> X.SpecExprAdd (spec_expr_translate l_spec_expr, spec_expr_translate r_spec_expr)
  | `SpecExprSub (l_spec_expr, r_spec_expr) -> X.SpecExprSub (spec_expr_translate l_spec_expr, spec_expr_translate r_spec_expr)
  | `SpecExprMul (spec_const, spec_expr) -> X.SpecExprMul (spec_const_translate spec_const, spec_expr_translate spec_expr)

let rec specification_translate specification =
  match specification with 
  | `SpecTrue -> X.SpecTrue
  | `SpecOp (l_spec_expr, spec_op, r_spec_expr) -> X.SpecOp (spec_expr_translate l_spec_expr, spec_op_translate spec_op, spec_expr_translate r_spec_expr)
  | `SpecAnd (l_specification, r_specification) -> X.SpecAnd (specification_translate l_specification, specification_translate r_specification)
  | `SpecOr (l_specification, r_specification) -> X.SpecOr (specification_translate l_specification, specification_translate r_specification)
  | `SpecImpl (l_specification, r_specification) -> X.SpecImpl (specification_translate l_specification, specification_translate r_specification)
  | `SpecNot (specification) -> X.SpecNot (specification_translate specification)
