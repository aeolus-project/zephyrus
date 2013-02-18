
open Variable_keys
open Helpers

type solution = (variable_key * int) list 

let string_of_solution solution =
  let strings_of_solution_assoc_list solution_assoc_list string_of_key =
    List.map (fun ( key, i ) -> 
          
          Printf.sprintf 
            "%s = %d" 
            (string_of_key key)
            i
  
        ) solution_assoc_list
  in
  let strings = 
    strings_of_solution_assoc_list solution string_of_variable_key
  in
  Printf.sprintf
    "\n%s\n"
    (lines_of_strings strings)