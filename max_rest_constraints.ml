
open Typing_context
open Variable_keys
open Generic_constraints



let create_max_rest_constraints typing_context : cstr list =

  List.flatten (
  
    List.map ( fun port_name ->
  
      let r =
        match (r typing_context port_name) with
          None -> failwith ""
        | Some r -> r
      in

      if r <= 1 
      then []
      else

      let max_provide = max_provide typing_context port_name
      in

      let list_of_ints_from_one_to_max_provide =
        BatList.init max_provide (fun i -> i + 1)
      in
  
      let all_redundancy_constraints_for_the_port =

        [ 
          [ 
            (var2expr (var (MaxRestVariable (T,    port_name, 0))) ) =~ (const2expr 0);
            (var2expr (var (MaxRestVariable (MAX,  port_name, 0))) ) =~ (const2expr 0);
            (var2expr (var (MaxRestVariable (REST, port_name, 0))) ) =~ (const2expr 0);
          ]
        ]
  
        @
  
        List.map ( fun i ->
      
          let all_resource_types_providing_at_least_i =
            List.filter ( fun domain_element ->
              delta typing_context domain_element port_name Provides >= i
            ) (domain typing_context)
    
          in
    
          let exprs_to_sum = 
              List.map (fun domain_element ->
                var2expr (var (DomainVariable domain_element))
              ) all_resource_types_providing_at_least_i
          in
    
          let number_of_resources_providing_at_least_i = sum exprs_to_sum
          in
    
          let the_T_variable         = var (MaxRestVariable (T,    port_name, i))
          and the_MAX_variable       = var (MaxRestVariable (MAX,  port_name, i))
          and the_REST_variable      = var (MaxRestVariable (REST, port_name, i))
          and previous_REST_variable = var (MaxRestVariable (REST, port_name, (i - 1)))

          in
          
          let the_T_constraint =
            (var2expr the_T_variable) =~ number_of_resources_providing_at_least_i
  
          and the_MAX_constraint = 
            (var2expr the_MAX_variable) =~ 
              ( ( (var2expr the_T_variable) +~ (var2expr previous_REST_variable) ) /~ (const2expr r) )
  
          and the_REST_constraint =
            (var2expr the_REST_variable) =~ 
              ( ( ( (var2expr the_T_variable) +~ (var2expr previous_REST_variable) ) %~ (const2expr r) )
              *~
              ( (var2expr the_T_variable) >=~~ (const2expr r) ) )
  
          in
  
          [the_T_constraint; the_MAX_constraint; the_REST_constraint]

        ) list_of_ints_from_one_to_max_provide

        in
        List.flatten all_redundancy_constraints_for_the_port
  
    ) (ports typing_context) )
