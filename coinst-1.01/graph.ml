
let colored = true

module F (R : Repository.S) = struct

open R

module Quotient = Quotient.F(R)

module Conflicts = Conflicts.F (R)



(* Transform an iterator over a collection into a map function returning a list. *)
let map_of_iter (collection_iter : ('a -> unit) -> 'c -> unit) (f : 'a -> 'b) (collection : 'c) : 'b list =
  let (result_list : (('b list) ref)) = ref []
  in
  collection_iter (fun collection_element ->
    let result = f collection_element
    in
    result_list := result :: !result_list;
  ) collection;
  result_list := List.rev !result_list;
  !result_list

(* The simplies map_of_iter which returns a list of the elements of the collection. *)
let list_of_iter (collection_iter : ('a -> unit) -> 'c -> unit) (collection : 'c) : 'a list = 
  map_of_iter collection_iter (fun x -> x) collection

(* Collection iterators with usual sequence of arguments (i.e. first the function, then the collection). *)
let quotient_iter (f : R.Package.t -> unit) (quotient : Quotient.t)  : unit = Quotient.iter f quotient
and formula_iter  (f : R.Disj.t -> unit)    (formula  : R.Formula.t) : unit = Formula.iter  formula f
and disj_iter     (f : R.Package.t -> unit) (disj     : R.Disj.t)    : unit = R.Disj.iter   disj    f



let output
      ?package_weight
      ?(edge_color = fun _ _ _ -> Some "blue")
      file ?(mark_all = false) ?(roots = [])
      quotient deps confl =

  (* Mark the packages to be included in the graph *)
  let marks = Hashtbl.create 101 in
  let marked i = Hashtbl.mem marks i in
  let has_dependencies p =
    let dep = PTbl.get deps p in
    not (Formula.implies Formula._true dep ||
         Formula.implies (Formula.lit p) dep)
  in
  let rec mark p =
    if not (marked p) then begin
      Hashtbl.add marks p ();
      PSet.iter mark (Conflict.of_package confl p)
    end
  in
  if mark_all then
    Quotient.iter (fun p -> Hashtbl.add marks p ()) quotient
  else if roots = [] then
    Quotient.iter
      (fun p ->
         if has_dependencies p then begin
           mark p;
           Formula.iter (PTbl.get deps p) (fun d -> Disj.iter d mark)
         end)
      quotient
  else (*XXX Find the right algorithm...
         Work on transitive closure of dependencies
         Mark all conflicts; marks all packages at the other side of
         these conflicts and all the alternative in the dependency.
         Proceed recursively...

         Backward mode:
         mark source package and all edges but the one considered

         A package is not relevant if installing it or not has no
         impact on the considered package
       *)
    List.iter mark roots;

  let ch = open_out file in
  let f = Format.formatter_of_out_channel ch in

  let string_of_eq_class (package : R.Package.t) : string =
    Format.fprintf Format.str_formatter "\"%a\"" (Quotient.print_class quotient) package;
    Format.flush_str_formatter ()

  and string_of_package (package : R.Package.t) : string =
    (* Format.fprintf Format.str_formatter "\"%a\"" (R.Package.print_name (Quotient.pool quotient)) package; *)
    Format.fprintf Format.str_formatter "\"%a\"" (R.Package.print (Quotient.pool quotient)) package;
    Format.flush_str_formatter ()

  in

  (* List of all the marked packages. *)
  let (quotient_packages : R.Package.t list) = 
    let all_packages = list_of_iter quotient_iter quotient
    in
    List.filter (fun (package : R.Package.t) -> 
      marked package
    ) all_packages

  in

  (* List of strings with descriptions of each package. *)
  let eq_class_strings =
  List.map
    (fun (eq_class : R.Package.t) ->
      
      let (coinst_dependencies : Formula.t) = PTbl.get deps eq_class 
      and (coinst_conflicts    : PSet.t)    = Conflict.of_package confl eq_class
      in

      let buf = Buffer.create 16;
      in

      (* Package description beginning *)

      Buffer.add_string buf "{\n";


      (* Name *)

      Buffer.add_string buf (Printf.sprintf "  \"name\"     : %s,\n" (string_of_eq_class eq_class));


      (* Dependencies *)

      Buffer.add_string buf (Printf.sprintf "  \"depend\"   : ");

      (* A list of lists of R.Package.t representing conjunction of disjunctions of dependencies. *)
      let conjunction_of_disjunctions =
        map_of_iter formula_iter (fun (coinst_disjunction : R.Disj.t) ->
          list_of_iter disj_iter coinst_disjunction
      ) coinst_dependencies

      in

      let string_of_conjunction_of_dicjunctions conjunction_of_disjunctions =
        Printf.sprintf 
          "[%s]" 
          (String.concat 
            ", " 
            (List.map 
              (fun disjunction ->
                Printf.sprintf 
                  "[%s]" 
                  (String.concat 
                    ", " 
                    (List.map string_of_eq_class disjunction)
                  )
              ) conjunction_of_disjunctions
            ) 
          )

      in

      Buffer.add_string buf (Printf.sprintf "%s,\n" (string_of_conjunction_of_dicjunctions conjunction_of_disjunctions));


      (* Conflicts *)

      Buffer.add_string buf (Printf.sprintf "  \"conflict\" : ");

      (* A list of lists of R.Package.t representing conflicts. *)
      let conflicts = list_of_iter PSet.iter coinst_conflicts
      in

      let string_of_conflicts conflicts =
        Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_eq_class conflicts))
      in

      Buffer.add_string buf (Printf.sprintf "%s\n" (string_of_conflicts conflicts));


      (* Package description end *)
      Buffer.add_string buf (Printf.sprintf "}");


      (* The result: *)
      Buffer.contents buf
    
  ) quotient_packages

  in
  
  let eq_classes_strings = String.concat ",\n" eq_class_strings 
  in

  (* Quotient.print quotient deps; *)

  (* List of strings with descriptions of each package. *)
  let packages_in_eq_classes_strings =
    List.flatten (
      List.map
        (fun (eq_class : R.Package.t) ->
          
          try
            let (representants : R.PSet.t) = Quotient.clss quotient eq_class
            in

            if R.PSet.cardinal representants = 1
            then [] (* Class of equivalence is one package. *)
            else

              let representant_strings = 
                map_of_iter PSet.iter (fun package -> 

                  let representant_buf = Buffer.create 16
                  in

                  (* Name *)
                  Buffer.add_string representant_buf (Printf.sprintf "{ \"name\" : %s, \"depend\" : [[%s]] }" (string_of_package package) (string_of_eq_class eq_class));                

                  (* The result: *)
                  Buffer.contents representant_buf

                ) representants

              in

              representant_strings

          with Not_found -> failwith "Problem with eq_class!";

      ) quotient_packages
    )

  in

  let packages_in_eq_classes_string = String.concat ",\n" packages_in_eq_classes_strings
  in

  (* Print the whole list of package descriptions. *)
  (* Print the whole list of package implementations. *)
  Format.fprintf f "[\n%s,\n%s\n]" eq_classes_strings packages_in_eq_classes_string;

  close_out ch

end
