
open Typing_context
open Variable_keys
open Generic_constraints


let create_provide_constraints typing_context : cstr list =
  List.map ( fun port ->

    let exprs_to_sum =
      List.map ( fun domain_element ->
        let n = (delta typing_context) domain_element port Provides
        and resource_var = var (DomainVariable domain_element)
        in
        
        (* Part of the sum:  N(t,s)  x  delta((t,s),a,Provides)  *)
        ( (var2expr resource_var) *~ (const2expr n) )

      ) (domain typing_context)
    in
    let sum_of_provided_ports = (sum exprs_to_sum)
    in
    let port_var = var (PortVariable port)
    in

    (* The constraint : [for each port p] P(p) = sum (over all (t,s) from the domain) of N(t,s) x delta((t,s),a,Provides) *)
    (var2expr port_var) =~ sum_of_provided_ports

    (* Name           : Provide a given port. *)
    (* Description    : Total number of provided ports p in the configuration is a sum of the quantity of port p provided by every resource present. *)
    (* Explanation    : P(p) is equal to the total number of provided ports p. *)
    
  ) (ports typing_context)



let create_require_constraints typing_context : cstr list =
  List.map ( fun port ->

    let exprs_to_sum =
      List.map ( fun domain_element ->
        let n = (delta typing_context) domain_element port Requires
        and resource_var = var (DomainVariable domain_element)
        in
    
        (* Part of the sum:  N(t,s)  x  delta((t,s),a,Requires)  *)
        ( (var2expr resource_var) *~ (const2expr n) )
    
      ) (domain typing_context) 
    in
    let sum_of_required_ports = (sum exprs_to_sum)
    in
    let port_var = var (PortVariable port)
    in

    (** The constraint : [for each port p] P(p) >= sum (over all (t,s) from the domain) of N(t,s) x delta((t,s),a,Requires) *)
    (var2expr port_var) >=~ sum_of_required_ports

    (* This constraint is obsolete! *)

  ) (ports typing_context)



let create_conflict_constraints typing_context : cstr list  =
  List.flatten (
    List.map ( fun domain_element ->
      
      let ports_in_conflict = 
        (gamma typing_context) domain_element
      in
      List.map ( fun port ->
        let n = (delta typing_context) domain_element port Provides
        in
        let resource_var = var (DomainVariable domain_element)
        and port_var     = var (PortVariable   port)
        in

        (* The constraint : [for each resource type (t,s)] [for each port p which conflicts with resource type (t,s)] ( N(t,s) >= 1 )  implies  P(p) = delta((t,s),a,Provides) *)
        ( (var2expr resource_var) >=~ (const2expr 1) ) =>~~ ( (var2expr port_var) =~ (const2expr n) )

        (* Name           : Conflict of (t,s) on port p. *)
        (* Description    : If a resource of type (t,s) is present in the configuration and it is in conflict with port p, then he can be the only resource providing port p present in the configuration. *)
        (* Explanation    : If N(t,s) is greater than zero, then total number of ports p provided must be equal exactly the number that (t,s) provides. *)

      ) ports_in_conflict
  
    ) (domain typing_context)
  )

