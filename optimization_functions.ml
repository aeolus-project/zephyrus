
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

let cost_expr_number_of_all_components universe = 
  let component_type_names = get_component_type_names universe
  in
  sum 
    (List.map (fun component_type_name -> 
      var2expr (var (GlobalElementVariable (ComponentType component_type_name))) 
    ) component_type_names)


let cost_expr_number_of_used_locations initial_configuration universe =
  let component_type_names = get_component_type_names universe
  and package_names = get_package_names universe
  and location_names = get_location_names initial_configuration
  in

  let used_locations =
    List.map (fun location_name ->
      
      let local_components_and_packages =
        sum (

          (* Components *)
          (List.map (fun component_type_name ->
            var2expr (var (LocalElementVariable (location_name, (ComponentType component_type_name)))) 
          ) component_type_names)
          @
          
          (* Packages *)
          (List.map (fun package_name ->
            var2expr (var (LocalElementVariable (location_name, (Package package_name)))) 
          ) package_names)

        )
      
      in 
      reify ( local_components_and_packages >=~ (int2expr 1) )

    ) location_names

    in
    let total_number_of_used_locations = (sum used_locations)
    in
    total_number_of_used_locations


let cost_expr_compact initial_configuration universe =
  (
    (cost_expr_number_of_all_components universe) 
    +~
    (cost_expr_number_of_used_locations initial_configuration universe)
  )

let cost_expr_difference_of_components initial_configuration universe =
  let component_type_names = get_component_type_names universe
  and location_names = get_location_names initial_configuration
  in

  let local_differences_of_number_of_components = 
    
    List.flatten (
      List.map (fun location_name ->
        List.map (fun component_type_name ->
          
          let number_of_components_in_initial_configuration =
            List.length (
              List.filter (fun component -> 
                (component.component_type = component_type_name) && (component.component_location = location_name)
              ) initial_configuration.configuration_components
            )

          and number_of_components_in_final_configuration =
            var (LocalElementVariable (location_name, (ComponentType component_type_name)))

          in

          let local_difference_of_number_of_components = 
            abs ( 
              (var2expr number_of_components_in_final_configuration) 
              -~
              (int2expr number_of_components_in_initial_configuration) 
            )
          
          in
          local_difference_of_number_of_components

        ) component_type_names
      ) location_names
    )
    
  in
  let total_difference_of_number_of_components = (sum local_differences_of_number_of_components)
  in
  total_difference_of_number_of_components