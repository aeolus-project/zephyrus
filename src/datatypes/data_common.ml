(****************************************************************************)
(*                                                                          *)
(*    This file is part of Zephyrus.                                        *)
(*                                                                          *)
(*    Zephyrus is free software: you can redistribute it and/or modify      *)
(*    it under the terms of the GNU General Public License as published by  *)
(*    the Free Software Foundation, either version 3 of the License, or     *)
(*    (at your option) any later version.                                   *)
(*                                                                          *)
(*    Zephyrus is distributed in the hope that it will be useful,           *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*    GNU General Public License for more details.                          *)
(*                                                                          *)
(*    You should have received a copy of the GNU General Public License     *)
(*    along with Zephyrus.  If not, see <http://www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)

(* Depends on
    - Map
    - Set
    - String
*)

(*/************************************************************************\*)
(*| 0. Model helpers                                                       |*)
(*\************************************************************************/*)

let get_name model_object = model_object#name

(*/************************************************************************\*)
(*| 1. Custom sets and maps                                                |*)
(*\************************************************************************/*)

module Map_global_from_stdlib = Map
module Set_global_from_stdlib = Set

module type OrderedType = Map_global_from_stdlib.OrderedType
module type Map_from_stblib = Map_global_from_stdlib.S
module type Set_from_stblib = Set_global_from_stdlib.S
module type String_from_stdlib = sig
  type t = string
  val compare: t -> t -> int

  val length : string -> int
  val get : string -> int -> char
  val set : string -> int -> char -> unit
  val create : int -> string
  val make : int -> char -> string
  val copy : string -> string
  val sub : string -> int -> int -> string
  val fill : string -> int -> int -> char -> unit
  val blit : string -> int -> string -> int -> int -> unit
  val concat : string -> string list -> string
  val iter : (char -> unit) -> string -> unit
  val escaped : string -> string
  val index : string -> char -> int
  val rindex : string -> char -> int
  val index_from : string -> int -> char -> int
  val rindex_from : string -> int -> char -> int
  val contains : string -> char -> bool
  val contains_from : string -> int -> char -> bool
  val rcontains_from : string -> int -> char -> bool
  val uppercase : string -> string
  val lowercase : string -> string
  val capitalize : string -> string
  val uncapitalize : string -> string
end

module type List_from_stdlib = sig
  val length : 'a list -> int
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val nth : 'a list -> int -> 'a
  val rev : 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val flatten : 'a list list -> 'a list

  val iter : ('a -> unit) -> 'a list -> unit
  val map : ('a -> 'b) -> 'a list -> 'b list
  val rev_map : ('a -> 'b) -> 'a list -> 'b list
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
 
  val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c

  val for_all : ('a -> bool) -> 'a list -> bool
  val exists : ('a -> bool) -> 'a list -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val mem : 'a -> 'a list -> bool
  val memq : 'a -> 'a list -> bool

  val find : ('a -> bool) -> 'a list -> 'a
  val filter : ('a -> bool) -> 'a list -> 'a list
  val find_all : ('a -> bool) -> 'a list -> 'a list
  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list

  val assoc : 'a -> ('a * 'b) list -> 'b
  val assq : 'a -> ('a * 'b) list -> 'b
  val mem_assoc : 'a -> ('a * 'b) list -> bool
  val mem_assq : 'a -> ('a * 'b) list -> bool
  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list

  val split : ('a * 'b) list -> 'a list * 'b list
  val combine : 'a list -> 'b list -> ('a * 'b) list

  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
end

module Int    = struct type t = int let compare = (-) end
module String = String
module List_from_stdlib_implem = List
module List = struct
  include List_from_stdlib_implem
  let is_empty l = (l = [])
  let rec fold_combine conv combine l init = match l with
    | [] -> init
    | [el] -> conv el
    | el::l' -> combine (conv el) (fold_combine conv combine l' init)
end

module Linked_list : sig
  type 'a t                                               (* type of a generic double linked list *)
  
  val create : unit -> 'a t                               (* create a new empty generic list *)
  val add_first : 'a -> 'a t -> unit                      (* add a new element at the beginning of the list *)
  val add_last  : 'a -> 'a t -> unit                      (* add a new element at the end of the list *)
  val add       : 'a -> 'a t -> unit                      (* synonym of add_last *)

  val first : 'a t -> 'a                                  (* returns the first element of the list, or raise Not_found *)
  val last  : 'a t -> 'a                                  (* returns the last  element of the list, or raise Not_found *)
  
  val rm_first : 'a t -> unit                             (* removes the first element of the list, or raise Not_found *)
  val rm_last  : 'a t -> unit                             (* removes the last  element of the list, or raise Not_found *)

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b   (* fold over the list, starting with its first element *)
  val fold_left  : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b   (* fold over the list, starting with its last  element *)
  val fold       : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b   (* synonym of fold_right *)

  val iter_right : ('a -> unit) -> 'a t -> unit           (* iter over the list, starting with its first element *)
  val iter_left  : ('a -> unit) -> 'a t -> unit           (* iter over the list, starting with its last  element *)
  val iter       : ('a -> unit) -> 'a t -> unit           (* synonym of iter_right *)

  val filter : ('a -> bool) -> 'a t -> 'a t               (* returns a new list keeping in the right order all the elements validating the input predicat *)

(*
  val prefix : 'a -> 'a t -> 'a t                         (* returns the smallest sub list that finishes with v in parameter, or raise Not_found *)
  val suffix : 'a -> 'a t -> 'a t                         (* returns the biggest sub list that starts with v in parameter, or raise Not_found *)
  val sub    : 'a -> 'a -> 'a t -> 'a t                   (* equivalent to prefix v2 (suffix v1 l) *)
*)
  val append : 'a t -> 'a t -> 'a t                       (* concat the two lists together *)
  val concat : 'a t list -> 'a t                          (* concat all the lists together *)

  val to_list : 'a t -> 'a list
  
  
  type 'a iterator
  
  val forward_iterator : 'a t -> 'a iterator
  val backward_iterator : 'a t -> 'a iterator
  
  val has_next : 'a iterator -> bool
  val next     : 'a iterator -> 'a
  
end = struct

  type 'a elt = { mutable pred : 'a elt option; value : 'a; mutable succ : 'a elt option}
  type 'a t = { mutable first : 'a elt option; mutable last : 'a elt option}

  let create () = { first = None; last = None }

  let add_first v l = let el = { pred = None; value = v; succ = l.first } in let sel = Some(el) in (match l.first with
    | None -> l.last <- sel
    | Some(el') -> el'.pred <- sel); l.first <- Some(el)

  let add_last v l =  let el = { pred = l.last; value = v; succ = None } in let sel = Some(el) in (match l.last with
    | None -> l.first <- sel
    | Some(el') -> el'.succ <- sel); l.last <- Some(el)

  let add = add_last

  let first_el l = match l.first with | None -> raise Not_found | Some(el) -> el
  let last_el  l = match l.last  with | None -> raise Not_found | Some(el) -> el
  let pred el = match el.pred  with | None -> raise Not_found | Some(el') -> el'
  let succ el = match el.succ  with | None -> raise Not_found | Some(el') -> el'

  let first l = (first_el l).value
  let last  l = (last_el  l).value

  let rm_first l = match l.first with
    | None -> raise Not_found
    | Some(el) -> l.first <- el.succ; match l.first with | None -> () | Some(el') -> el.pred <- None
  let rm_last  l = match l.last with
    | None -> raise Not_found
    | Some(el) -> l.last  <- el.pred; match l.last  with | None -> () | Some(el') -> el.succ <- None

  let fold_right f l accu = 
    let rec step el accu = let accu' = f el.value accu in match el.succ with None -> accu' | Some(el') -> step el' accu' in match l.first with
    | None -> accu | Some(el) -> step el accu
  let fold_left f accu l = 
    let rec step el accu = let accu' = f accu el.value in match el.pred with None -> accu' | Some(el') -> step el' accu' in match l.last with
    | None -> accu | Some(el) -> step el accu
  let fold = fold_right

  let iter_right f l =
    let rec step el = f el.value; match el.succ with None -> () | Some(el') -> step el' in match l.first with
    | None -> () | Some(el) -> step el
  let iter_left f l = 
    let rec step el = f el.value; match el.pred with None -> () | Some(el') -> step el' in match l.last with
    | None -> () | Some(el) -> step el
  let iter = iter_right

  let filter f l = fold_right (fun v res -> if f v then (add_last v res; res) else res) l (create ())

(*
  let rec find_el_el v el  = Printf.printf "node "; flush stdout; if el.value = v then (Printf.printf "found\n "; flush stdout; el) else match el.succ with | None -> raise Not_found | Some(el') -> Printf.printf "next "; flush stdout; find_el_el v el'
  let find_el v l = find_el_el v (first_el l)

  let extract first last =
    let first' = {pred = None; value = first.value; succ = None } in
    let rec step pred el = Printf.printf "toto "; flush stdout; let el' = { pred = Some(pred); value = el.value; succ = None } in
      pred.succ <- Some(el'); if el = last then el' else step el' (succ el) in
    let last' = if first = last then first' else step first' (succ first) in { first = Some(first'); last = Some(last') }

  let prefix v l = let el = find_el v l in extract (first_el l) el
  let suffix v l = let el = find_el v l in extract el (last_el l)
  let sub v1 v2 l = let el1 = find_el v1 l in let el2 = find_el_el v2 el1 in extract el1 el2
*)
  
  let append l1 l2 = let res = create () in
    iter_right (fun v -> add_last v res) l1;
    iter_right (fun v -> add_last v res) l2; res
    
  let rec concat l = match l with | [] -> create () | e::l' -> append e (concat l')
  
  
  let to_list l = fold_left (fun res v -> v::res) [] l
  
  type 'a iterator = { mutable element : 'a elt option; step : 'a elt -> 'a elt option }
  
  let forward_iterator  l = { element = l.first; step = fun el -> el.succ } 
  let backward_iterator l = { element = l.last; step = fun el -> el.pred } 

  let has_next i = i.element != None
  let next i     = match i.element with | None -> raise Not_found | Some(el) -> i.element <- i.step el; el.value

end


(** Extension of the Set module from the standard library with Construction and Conversion **)
module Set = struct

  module type S = sig
    include Set_from_stblib
    
    val keep_elements : int -> t -> t
    val set_of_direct_list: elt list -> t
    val set_of_list: ('a -> elt) -> 'a list -> t
  end
  
  module Make(Ord : OrderedType) : S with type elt = Ord.t = struct
    module Set_tmp = Set_global_from_stdlib.Make(Ord)
    include Set_tmp


    let keep_elements n s = if n <= 0 then empty else (let rec f n s = if n <= 0 then s else f (n - 1) (remove (choose s) s) in f ((cardinal s) - n) s)
    let set_of_direct_list l = List.fold_left (fun res v -> add v res) empty l
    let set_of_list f l = List.fold_left (fun res v -> add (f v) res) empty l
  end

  module Convert(Set_origin : S) (Set_target : S) = struct
    let convert f s = Set_origin.fold (fun v res -> Set_target.add (f v) res) s Set_target.empty
    let filter_convert f s = 
      Set_origin.fold (fun el s -> 
        match f el with
        | None   -> s 
        | Some x -> Set_target.add x s
      ) s Set_target.empty

    let set_convert f s = Set_origin.fold (fun v res -> Set_target.union (f v) res) s Set_target.empty
  end

  module EquivalenceClass(Set_origin : S)(Set_target : S with type elt = Set_origin.t) = struct
    let mem f t ss = Set_target.fold (fun s res -> match res with | Some _ -> res | None -> if f  t (Set_origin.choose s) then Some(s) else None) ss None
    let compute_step f t ss = match mem f t ss with
      | None -> Set_target.add (Set_origin.singleton t) ss
      | Some(s) -> Set_target.add (Set_origin.add t s) (Set_target.remove s ss)

    let compute f s = Set_origin.fold (compute_step f) s Set_target.empty
  end
end

module Int_set        = Set.Make(Int)
module Int_set_set    = Set.Make(Int_set)
module String_set     = Set.Make(String)
module String_set_set = Set.Make(String_set)

module Int_set_to_String_set = Set.Convert(Int_set)(String_set)
let setstring_of_setint    s = Int_set_to_String_set.convert string_of_int s

module Map = struct

  module type S = sig
    include Map_from_stblib
    
    val of_direct_list: (key * 'a) list -> 'a t
    val map_of_list: ('a -> key * 'b) -> 'a list -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t

    val keys : 'a t -> key list
    val values : 'a t -> 'a list

    module Set_of_keys(Set_target : Set.S with type elt = key) : sig
      val set_of_keys : 'a t -> Set_target.t
    end

    module Set_of_values(Set_target : Set.S) : sig
      val set_of_values : Set_target.elt t -> Set_target.t
    end
  end
  
  module Make(Ord : OrderedType) : S with type key = Ord.t = struct
    module Map_tmp = Map_global_from_stdlib.Make(Ord)
    include Map_tmp
  
    let map_of_list f l = List.fold_left (fun res el -> let (k,v) = f el in add k v res) empty l
    let of_direct_list l = List.fold_left (fun res (k,v) -> add k v res) empty l
    let map f m = fold (fun k v res -> add k (f v) res) m empty
    
    let values m = fold (fun _ v res -> v::res) m []
    let keys   m = fold (fun k _ res -> k::res) m []

    module Set_of_keys(Set_target : Set.S with type elt = key) = struct
      let set_of_keys map = fold (fun k _ res -> Set_target.add k res) map Set_target.empty 
    end

    module Set_of_values(Set_target : Set.S) = struct
      let set_of_values map = fold (fun _ v res -> Set_target.add v res) map Set_target.empty
    end
  end
  
  module Convert(Map_origin : S) (Map_target : S) = struct
    let convert f m = Map_origin.fold (fun k v res -> let (k',v') = f (k,v) in Map_target.add k' v' res) m Map_target.empty
  end


end

module Int_map    = Map.Make(Int)
module String_map = Map.Make(String)

module Keys_of_Int_map    = Int_map.Set_of_keys(Int_set)
module Keys_of_String_map = String_map.Set_of_keys(String_set)



(*/************************************************************************\*)
(*| 2. Unique identifier management                                        |*)
(*\************************************************************************/*)

(* Modules for used tokens (names, ids, etc.) management. *)
module type Used_tokens_type =
sig
  type t
  type token
  val empty : unit -> t
  val mem   : token -> t -> bool
  val add   : token -> t -> unit
end

module Used_tokens_set =
functor (Token_set : Set.S) ->
struct
  type t     = Token_set.t ref
  type token = Token_set.elt
  let empty ()              = ref Token_set.empty
  let mem token used_tokens = Token_set.mem token !used_tokens
  let add token used_tokens = used_tokens := Token_set.add token !used_tokens
end

module Used_tokens_string : Used_tokens_type with type token = string = Used_tokens_set(String_set)
module Used_tokens_int    : Used_tokens_type with type token = int    = Used_tokens_set(Int_set)

(* Modules for unique identifier creation *)
module type Fresh =
sig
  type t
  type id
  val create    : unit -> t
  val current   : t -> id
  val next      : t -> id
  val is_used   : t -> id -> bool
  val mark_used : t -> id -> unit
end

module Fresh_integer : Fresh with type id = int =
struct

  type t_record = { 
    current : int;
    used    : Used_tokens_int.t;
  }

  type t = t_record ref

  type id = int

  let create () = ref {
    current = 0;
    used    = Used_tokens_int.empty ()
  }

  let is_used t id =
    Used_tokens_int.mem id (!t).used

  let mark_used t id =
    Used_tokens_int.add id (!t).used

  let current t = (!t).current

  let rec next (t : t) : id =
    let value = current t + 1 in
    t := {
      current = value;
      used    = (!t).used;
    };
    if   is_used t value
    then next t
    else (mark_used t value; value)

end

(* Unique identifiers plus special identifier requests *)
module type Fresh_with_special =
sig
  include Fresh
  type special_request (* type used to make requests for special identifiers *)
  val special : special_request -> id (* return an id corresponding to a special request *)
end

(* Only one special request: Deprecated *)
type special_request_deprecated = Deprecated

module Fresh_integer_with_deprecated =
struct
  include Fresh_integer
  type special_request = special_request_deprecated
  let special = function Deprecated -> -1
end


(*/************************************************************************\*)
(*| 3. Catalog                                                             |*)
(*\************************************************************************/*)


(* Module providing a class for storing id and names and creating mappings between names and ids. *)
(* In fact it's general enough to use it for mappings between ids and anything 
   (not only names but also any objects) as long as we can order them. *)
(* A five-functor module is quite heavy, but these functors here are redundant, so it is not as
   complicated as it looks. It could be probably possible to reduce it to three functors, however
   I have no idea if that would type correctly, so I prefer to leave it like this. *)
module Catalog =
  functor (Fresh_id : Fresh)                             -> (* For generating fresh ids. *)
  functor (Id_set   : Set.S with type elt = Fresh_id.id) -> (* Set of ids. Must match with the type of ids generated by Fresh_id. *)
  functor (Obj_set  : Set.S)                             -> (* Set of names. *)
  functor (Id_map   : Map.S with type key = Id_set.elt)  -> (* Map with ids as keys.   Must match with the Id_set  element type. *)
  functor (Obj_map  : Map.S with type key = Obj_set.elt) -> (* Map with names as keys. Must match with the Id_name element type. *)
  struct

    (* Five functors is quite heavy indeed, let's extract the two basic types. *)
    type id  = Id_set.elt
    type obj = Obj_set.elt

    (* A modifiable catalog with id <-> object mapping. *)
    class type obj_catalog_iface = object
      (* Access *)
      method ids             : Id_set.t           (* All the ids. *)
      method objs            : Obj_set.t          (* All the objects. *)
      method obj_of_id       : id  -> obj         (* Get the object corresponding to the given id. May throw Not_found exception. *)
      method id_of_obj       : obj -> id          (* Get the id corresponding to the given object. May throw Not_found exception. *)
      (* Modify *)
      method get_else_add    : obj -> id          (* Get the id corresponding to the given object. If the object does not exist, create a new fresh id for this object, update the data structures and the return the id. *)
      method add             : obj -> unit        (* If the object does not exist, create a new fresh id for this object and update the data structures. *)
      method add_id_obj_pair : id  -> obj -> unit (* Update the data structures with the given (id, object) pair. *)
      (* Lower level manipulation *)
      method set_id_of_obj   : obj -> id  -> unit (* Adds the object to objs and makes it correspond to the given id     (only one way, we have obj -> id, but not id -> obj!). *)
      method set_obj_of_id   : id  -> obj -> unit (* Adds the id     to ids  and makes it correspond to the given object (only one way, we have id -> obj, but not obj -> id!). *)
      method id_to_obj_map   : obj Id_map.t       (* Retrieve directly the id -> object map. *)
      method obj_to_id_map   : id Obj_map.t       (* Retrieve directly the object -> id map. *)
    end

    (* Implementation of the catalog. *)
    class obj_catalog : obj_catalog_iface = 

      (* Data structures. *)
      let objs          : Obj_set.t ref      = ref Obj_set.empty in  (* All the ids. *)
      let ids           : Id_set.t  ref      = ref Id_set.empty  in  (* All the objs. *)
      let obj_to_id_map : (id Obj_map.t) ref = ref Obj_map.empty in  (* Mapping obj -> id *)
      let id_to_obj_map : (obj Id_map.t) ref = ref Id_map.empty  in  (* Mapping id -> obj *)
    
      (* For unique identifier creation. *)
      let current_id = Fresh_id.create () in
      
      (* Mapping functions. May throw Not_found exception. *)
      let id_of_obj (obj : obj) : id  = Obj_map.find obj !obj_to_id_map in
      let obj_of_id (id  : id)  : obj = Id_map .find id  !id_to_obj_map in
      
      (* Add the obj to objs and make it correspond to a given id (obj -> id). *)
      let set_id_of_obj obj id =
        objs          := Obj_set.add obj    (!objs);
        obj_to_id_map := Obj_map.add obj id (!obj_to_id_map) in

      (* Add the id to ids and make it correspond to a given obj (id -> obj). *)
      let set_obj_of_id id obj =
        ids           := Id_set.add id     (!ids);
        id_to_obj_map := Id_map.add id obj (!id_to_obj_map);
        Fresh_id.mark_used current_id id in
          
      (* Adds new obj and id to appropriate sets 
         and add the relation id <-> obj to both maps. *)
      let add_new_id_obj_pair id obj =
        set_id_of_obj obj id;
        set_obj_of_id id obj in


      (* Look for the obj in the maps, if it does not exist create it and give it a fresh id. *)
      let get_else_add obj = 
        try id_of_obj obj
        with Not_found -> let id = Fresh_id.next current_id in add_new_id_obj_pair id obj; id in

      (* As above, but don't return anything. *)
      let add obj =
        let _ = get_else_add obj in () in

      (* The object catalog: *)
      object
        method ids             = !ids                
        method objs            = !objs               
        method id_of_obj       = id_of_obj           
        method obj_of_id       = obj_of_id           
        method get_else_add    = get_else_add        
        method add             = add                 
        method add_id_obj_pair = add_new_id_obj_pair 
        method set_id_of_obj   = set_id_of_obj       
        method set_obj_of_id   = set_obj_of_id       
        method id_to_obj_map   = !id_to_obj_map      
        method obj_to_id_map   = !obj_to_id_map      
      end

    (* Create a new catalog by taking a set of objects and adding them all. *)
    let of_set_of_objs (objs : Obj_set.t) : obj_catalog_iface = 
      let catalog = new obj_catalog in
      Obj_set.iter catalog#add objs;
      catalog

    (* Create a new catalog corresponding to a given id -> object map. *)
    let of_id_to_obj_map (id_to_obj_map : obj Id_map.t) : obj_catalog_iface =
      let catalog = new obj_catalog in
      Id_map.iter catalog#add_id_obj_pair id_to_obj_map;
      catalog



    (* A catalog variation with just one purely syntactic difference - we have names not objects. *)
    type name = obj

    (* A modifiable catalog with name <-> id mapping. *)
    class type catalog_iface = object
      (* Access *)
      method ids              : Id_set.t           (* All the ids. *)
      method names            : Obj_set.t          (* All the names. *)
      method name_of_id       : id   -> name       (* Get the name corresponding to the given id. May throw Not_found exception. *)
      method id_of_name       : name -> id         (* Get the id corresponding to the given name. May throw Not_found exception. *)
      (* Modify *)
      method get_else_add     : name -> id         (* Get the id corresponding to the given name. If the name does not exist, create a new fresh id for this name, update the data structures and the return the id. *)
      method add              : name -> unit       (* If the name does not exist, create a new fresh id for this name and update the data structures. *)
      method add_id_name_pair : id -> name -> unit (* Update the data structures with the given (id, name) pair. *)
      (* Lower level manipulation *)
      method set_id_of_name   : name -> id -> unit (* Adds the name to names and makes it correspond to the given id     (only one way, we have name -> id,  but not id  -> name!). *)
      method set_name_of_id   : id -> name -> unit (* Adds the id   to ids  and makes it correspond to the given object  (only one way, we have id   -> obj, but not obj -> id!  ). *)
      method id_to_name_map   : name Id_map.t      (* Retrieve directly the id -> object map. *)
      method name_to_id_map   : id Obj_map.t       (* Retrieve directly the object -> id map. *)
    end

    let catalog_of_obj_catalog (obj_catalog : obj_catalog) : catalog_iface =
      (* The name catalog: *)
      object
        method ids              = obj_catalog#ids
        method names            = obj_catalog#objs
        method id_of_name       = obj_catalog#id_of_obj
        method name_of_id       = obj_catalog#obj_of_id
        method get_else_add     = obj_catalog#get_else_add
        method add_id_name_pair = obj_catalog#add_id_obj_pair
        method add              = obj_catalog#add
        method set_id_of_name   = obj_catalog#set_id_of_obj
        method set_name_of_id   = obj_catalog#set_obj_of_id
        method id_to_name_map   = obj_catalog#id_to_obj_map
        method name_to_id_map   = obj_catalog#obj_to_id_map
      end

    class catalog : catalog_iface = 
      let obj_catalog = new obj_catalog in 
      (* TODO: Why cannot we just write "catalog_of_obj_catalog obj_catalog" here?! *)
      object
        method ids              = obj_catalog#ids
        method names            = obj_catalog#objs
        method id_of_name       = obj_catalog#id_of_obj
        method name_of_id       = obj_catalog#obj_of_id
        method get_else_add     = obj_catalog#get_else_add
        method add_id_name_pair = obj_catalog#add_id_obj_pair
        method add              = obj_catalog#add
        method set_id_of_name   = obj_catalog#set_id_of_obj
        method set_name_of_id   = obj_catalog#set_obj_of_id
        method id_to_name_map   = obj_catalog#id_to_obj_map
        method name_to_id_map   = obj_catalog#obj_to_id_map
      end

    (* A closed catalog (closed means that it cannot be modified. *)
    class type closed_catalog_iface = object
      method ids            : Id_set.t
      method names          : Obj_set.t
      method name_of_id     : id   -> name
      method id_of_name     : name -> id
      method id_to_name_map : name Id_map.t
      method name_to_id_map : id Obj_map.t
    end

    (* Implementation of a closed catalog which throws appropriate exceptions. *)
    class closed_catalog_with_exceptions (catalog : catalog) (catalog_name : string) (string_of_id : id -> string) (string_of_name : name -> string) : closed_catalog_iface = object
      method ids             = catalog#ids
      method names           = catalog#names
      method name_of_id id   = try catalog#name_of_id id   with Not_found -> failwith (Printf.sprintf "%s#name_of_id %s" catalog_name (string_of_id id))
      method id_of_name name = try catalog#id_of_name name with Not_found -> failwith (Printf.sprintf "%s#id_of_name %s" catalog_name (string_of_name name))
      method id_to_name_map  = catalog#id_to_name_map
      method name_to_id_map  = catalog#name_to_id_map
    end
    
    (* Create a new catalog by taking a set of names and adding them all. *)
    let of_set_of_names (names : Obj_set.t) : catalog_iface = 
      catalog_of_obj_catalog (of_set_of_objs names)

    (* Create a new catalog corresponding to a given id -> name map. *)
    let of_id_to_name_map (id_to_name_map : name Id_map.t) : catalog_iface =
      catalog_of_obj_catalog (of_id_to_obj_map id_to_name_map)

  end

(*/************************************************************************\*)
(*| 4. Basic Data Base                                                     |*)
(*\************************************************************************/*)

module Database = struct
  exception Table_not_found  (* exception raised when trying to access a table that is not present in the database *)
  exception Column_not_found (* exception raised when trying to access a column that is not present in the table *)


  module Table = struct
    module type S = sig      (* Interface of the table module *)
      type t                 (* Type of a table *)
      type key               (* Type of the key used in the table *)
      type 'a column         (* Generic type for all columns used in the table *)

      val create : int -> t  (* Create a new table with the initial capacity in parameter (automatically grow if necessary) *)
      
      val mem : t -> key -> bool                          (* Tests if the key was inserted in the table *)
      val mem_in_column : t -> 'a column -> key -> bool   (* Tests if the key was inserted in the column in parameter (useful, as columns can be optional) *)
      
      type add_type                                           (*  *)
      val add : t -> key -> add_type
      val add_to_column : t -> 'a column ->  key ->'a -> unit (*  *)
    
      val find : t -> 'a column -> key -> 'a
      val find_key : t -> 'a column -> 'a -> key
    end

    module Empty(K : sig type key type 'a column val compare : key -> key -> int end) = struct
      module Key_set = Set.Make(struct type t = K.key let compare = K.compare end)
      type t = { mutable mem : Key_set.t }
      type key = K.key
      type 'a column = 'a K.column
      
      let create _ = { mem = Key_set.empty }
      
      let mem m k = Key_set.mem k m.mem
      let mem_in_column m c k = raise Column_not_found
      
      type add_type = unit
      let add m k = m.mem <- Key_set.add k m.mem
      let add_to_column m c k v = raise Column_not_found
    
      let find m c k      = raise Column_not_found
      let find_key m c v  = raise Column_not_found
    end



    module type Input = sig type t type key type 'a column val name : t  column end
    module type First_intermediate = sig include Input val check : (key, t) Hashtbl.t -> key -> t -> unit end
    module type Second_intermediate  = sig include First_intermediate val find : (key, t) Hashtbl.t -> key -> t end
    module type Third_intermediate = sig include Second_intermediate val aggregate : ((key, t) Hashtbl.t) -> ((t, key) Hashtbl.t) -> key -> t -> t end
    
    module WithChecking(C : Input)(P : sig val check : C.key -> C.t -> C.t option -> unit end) = struct
      include C let check m k v = P.check k v (try Some (Hashtbl.find m k) with Not_found -> None) end
    module WithoutChecking(C : Input) = struct
      include C let check m k v = () end

    module WithDefaultValue(C : First_intermediate)(P : sig val default : C.t end) = struct
      include C let find m k = try Hashtbl.find m k with Not_found -> P.default
    end module WithoutDefaultValue(C : First_intermediate) = struct
      include C let find m k = Hashtbl.find m k
    end
    
    module WithAggregate(C : Second_intermediate)(P : sig val aggregate : C.t -> C.t -> C.t end) = struct
      include C let aggregate m1 m2 k v = try let v' = Hashtbl.find m1 k in Hashtbl.remove m2 v'; P.aggregate v' v with Not_found -> v end
    module WithoutAggregate(C : Second_intermediate) = struct
      include C let aggregate m1 m2 k v = v end


    module Structure(C : Third_intermediate)(T : S with type key = C.key and type 'a column = 'a C.column) = struct
      type t = (((C.key, C.t) Hashtbl.t) * ((C.t, C.key) Hashtbl.t)) * T.t
      type key = T.key
      type 'a column = 'a T.column

      let create n = ((Hashtbl.create n, Hashtbl.create n), T.create n)

      let mem (_,p) = T.mem p
      let mem_in_column ((m1,m2),p) c k = if (Obj.magic c) == C.name then Hashtbl.mem m1 k else T.mem_in_column p c k
    
      let local_add (m1,m2) k v' =
        let v_tmp: C.t = Obj.magic v' in C.check m1 k v_tmp;
        let v = C.aggregate m1 m2 k v_tmp in Hashtbl.replace m1 k v; Hashtbl.replace m2 v k

      let add_to_column (m,p) c k v = if (Obj.magic c) != C.name then T.add_to_column p c k v
        else (if not (T.mem p k) then raise Not_found else local_add m k v)

      let find ((m1,m2),p) c k = if (Obj.magic c) == C.name then Obj.magic (C.find m1 k) else T.find p c k
      let find_key ((m1,m2),p) c v = if (Obj.magic c) == C.name then Hashtbl.find m2 (Obj.magic v) else T.find_key p c v
    end


    module AddMandatory(C : Third_intermediate)(T : S with type key = C.key and type 'a column = 'a C.column) = struct
      include Structure(C)(T)

      type add_type = C.t -> T.add_type
      let add (m,p) k v = local_add m k v; T.add p k
    end

    module AddOptional(C : Third_intermediate)(T : S with type key = C.key and type 'a column = 'a C.column) = struct
      include Structure(C)(T)

      type add_type = T.add_type
      let add (_,p) = T.add p
    end
    
  end

  module type S = sig
    type t
    type 'a key
    type ('a, 'b) table
    type 'a column

    val create : int -> t
      
    val mem : t -> ('a, 'b) table -> 'a key -> bool
    val mem_in_column : t -> ('a, 'b) table -> 'c column -> 'a key -> bool
      
    val add : t -> ('a, 'b) table -> 'a key -> 'b
    val add_to_column : t -> ('a, 'b) table -> 'c column -> 'a key -> 'c -> unit
    
    val find : t -> ('a, 'b) table -> 'c column -> 'a key -> 'd
    val find_key : t -> ('a, 'b) table -> 'c column -> 'c -> 'a key
  end

  module Empty(K : sig type 'a key type ('a, 'b) table type 'a column end) = struct
    type t = unit
    type 'a key = 'a K.key
    type ('a, 'b) table = ('a, 'b) K.table
    type 'a column = 'a K.column
      
    let create _ = ()
      
    let mem m t k             = raise Table_not_found
    let mem_in_column m t c k = raise Table_not_found
      
    let add m t k               = raise Table_not_found
    let add_to_column m t c k v = raise Table_not_found
    
    let find m t c k      = raise Table_not_found
    let find_list m t c k = raise Table_not_found
    let find_key m t c v  = raise Table_not_found
  end

  module AddTable(T : Table.S)(Id : sig type ('a, 'b) table val name : (T.key, T.add_type) table end)
      (DB : S with type ('a, 'b) table = ('a, 'b) Id.table and type 'a column = 'a T.column) = struct
    type t = T.t * DB.t
    type 'a key = 'a DB.key
    type ('a, 'b) table = ('a, 'b) DB.table
    type 'a column = 'a DB.column
    
    let create n = (T.create n, DB.create n)
    
    let mem (m,p) t k = if (Obj.magic t) == Id.name then T.mem m (Obj.magic k) else DB.mem p t k
    let mem_in_column (m,p) t c k = if (Obj.magic t) == Id.name then T.mem_in_column m c (Obj.magic k) else DB.mem_in_column p t c k
    
    let add (m,p) t k = if (Obj.magic t) == Id.name then Obj.magic (T.add m (Obj.magic k)) else DB.add p t k
    let add_to_column (m,p) t c k v = if (Obj.magic t) == Id.name then T.add_to_column m c (Obj.magic k) v else DB.add_to_column p t c k v
    
    let find (m,p) t c k      = if (Obj.magic t) == Id.name then Obj.magic (T.find m c (Obj.magic k)) else DB.find p t c k
    let find_key (m,p) t c v  = if (Obj.magic t) == Id.name then Obj.magic (T.find_key m c v) else DB.find_key p t c v
  end

end


(*/************************************************************************\*)
(*| 5. Generic Graph                                                       |*)
(*\************************************************************************/*)

module Graph = struct
  module type Vertice_data = sig type t end
  module type Edge_data   = sig type t end

  module type S = sig
    type vertice_data
    type edge_data
    
    module rec Vertice : sig
      type t                           (* type of vertices *)
      val data   : t -> vertice_data   (* returns the data contained in that vertice *)
      val succs_e: t -> Edge_set.t     (* returns the set of edges exiting from that vertice *)
      val preds_e: t -> Edge_set.t     (* returns the set of edges targeting that vertice *)
      val succs_v: t -> Vertice_set.t  (* returns the set of vertices that have an edge from that vertice *)
      val preds_v: t -> Vertice_set.t  (* returns the set of vertices that have an edge to that vertice *)

      val compare : t -> t -> int      (* classic comparison function *)
      val equal   : t -> t -> bool     (* comparison that returns a bool instead of an int *)
    end and Vertice_set : (Set.S with type elt = Vertice.t) and Vertice_map : (Map.S with type key = Vertice.t)
    
    and Edge : sig
      type t                           (* type of edges *)
      val data   : t -> edge_data      (* returns the data contained in that edge *)
      val origin : t -> Vertice.t      (* returns the origin vertice of that edge *)
      val target : t -> Vertice.t      (* returns the target vertice of that edge *)

      val is_loop      : t -> bool     (* returns if that edge is part of a loop *)
      val is_loop_in   : t -> bool     (* returns if that edge targets a vertice in a loop *)
      val is_loop_out  : t -> bool     (* returns if that edge originates from a vertice in a loop *)
      
      val compare : t -> t -> int      (* classic comparison function *)
      val equal   : t -> t -> bool     (* comparison that returns a bool instead of an int *)
    end and Edge_set : (Set.S with type elt = Edge.t) and Edge_map : (Map.S with type key = Edge.t)

    and Path : sig
      type t                                      (* type of path *)
      exception Invalid_path_extension            (* exception raised when erroneous operation is done on a path *)

      val create      : Vertice.t -> t            (* create an empty path starting on that vertice *)
      val is_empty    : t -> bool                 (* returns if the path is empty *)
      val add         : Edge.t -> t -> t          (* returns that path extended with that edge, or raise Invalid_path_extension *)
      val mem         : Vertice.t -> t -> bool    (* returns if that vertice is in that path *)
      val vertices    : t -> Vertice_set.t        (* returns the set of vertices in that path *)
      val edges       : t -> Edge_set.t           (* returns the set of edges in that path *)
      val split       : Vertice.t -> t -> t list  (* [split v p] returns the list [p1;...;pn] with [p = pn -> v -> ... -> v -> p1. Does not create empty path in case origin or target is [v] *)
      val concat      : t -> t -> t               (* concat the two paths together, or raise Invalid_path_extension  *)
      val concat_list : t list -> t               (* concat a non-empty list of paths *)
      val is_loop     : t -> bool                 (* tests if that path is a loop *)
      
      val compare : t -> t -> int                 (* classic comparison function *)
      val equal   : t -> t -> bool                (* comparison that returns a bool instead of an int *)
    end and Path_set : (Set.S with type elt = Path.t) and Path_map : (Map.S with type key = Path.t)

    and Loop : sig
      type t                                     (* type of loop *)
      
      val vertices : t -> Vertice_set.t          (* returns the set of vertices that are part of that loop *)
      val edges    : t -> Edge_set.t             (* returns the set of edges that are part of that loop *)
      val succs_e  : t -> Edge_set.t             (* returns the set of edges whose origin is part of the loop (and which are not part of the loop) *)
      val preds_e  : t -> Edge_set.t             (* returns the set of edges whose target is part of the loop (and which are not part of the loop) *)
      val succs_v  : t -> Vertice_set.t          (* returns the targets of the edges in [succs_e] of that loop *)
      val preds_v  : t -> Vertice_set.t          (* returns the origins of the edges in [preds_e] of that loop *)
      
      val mem    : Vertice.t -> t -> bool        (* tests if that vertice is part of that loop *)
      val center : Vertice.t -> t -> unit        (* if that vertice is part of that loop, move the starting point of the main path of the loop on that vertice *)
      val path   : t -> Edge.t list              (* returns the main path of the loop, once it has been centered *)
      
      val compare : t -> t -> int                (* classic comparison function *)
      val equal   : t -> t -> bool               (* comparison that returns a bool instead of an int *)
    end and Loop_set : (Set.S with type elt = Loop.t) and Loop_map : (Map.S with type key = Loop.t)

    
    type t

    val create      : unit -> t                                           (* create a new empty graph *)
    val add_vertice : vertice_data -> t -> Vertice.t                      (* add a new vertice to the graph *)
    val add_edge    : Vertice.t -> edge_data -> Vertice.t -> t -> Edge.t  (* add a new edge to the graph *)
    
    val vertices : t -> Vertice_set.t                                     (* returns the set of vertices in the graph *)
    val edges    : t -> Edge_set.t                                        (* returns the set of edges in the graph *)
    val loops    : t -> Loop_set.t                                        (* returns the set of loops in the graph *)
    
    val vertice_roots : t -> Vertice_set.t
    val loop_roots    : t -> Loop_set.t 
    val vertice_leafs : t -> Vertice_set.t
    val loop_leafs    : t -> Loop_set.t 
    
(*    val to_dot : (vertice_data -> string * string) -> (edge_data -> string * string) -> t -> string*)
    
    module Traverse_depth : sig
      val iter : (Path.t -> Vertice.t -> bool -> (unit -> unit) -> unit) -> t -> unit
    end
    
    module Traverse_topology : sig
      val iter_downward : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit
      val iter_upward   : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit
    end    
  end


  module Make(V: Vertice_data)(E: Edge_data) = struct (* Functor for graphs *)
    type vertice_data = V.t  (* data contained in vertices *)
    type edge_data    = E.t  (* data contained in edges *)

    type loop_id    = int    (* unique identifiers for loops *)
    type vertice_id = int    (* unique identifiers for vertices *)
    type edge_id    = int    (* unique identifiers for edgess *)
  
    module rec Data_types : sig
      type vertice = {                                                            (* a vertice *)
        v_id : vertice_id;                                                        (* has a unique id *)
        v_data : vertice_data;                                                    (* contains some data *)
        mutable v_successors : Edge_set.t; mutable v_predecessors : Edge_set.t;   (* links to its input and output edges (these lists can grow) *)
        mutable v_parse_tag : bool;                                               (* has a tag for basic parsing *)
        mutable v_abst : to_component Lazy.t }                                    (* is part of the abstract graph (its representation can change in case loops are created) *)
        
      and edge = {                                                                (* an edge *)
        e_id : edge_id;                                                           (* has a unique id *)
        e_data : edge_data;                                                       (* contains some data *)
        e_origin : Vertice.t; e_target : Vertice.t;                               (* has an origin and a target vertice *)
        mutable e_loop_tag : edge_loop_tag }                                      (* and some annex informations concerning loops (I don't know if still useful) *)
      and edge_loop_tag = Edge_no_loop | Edge_loop of Loop.t | Edge_loop_in of Loop.t | Edge_loop_out of Loop.t

      and path = {                                                                (* a path *)
        p_origin : Vertice.t; p_end : Vertice.t;                                  (* has an origin and a target vertice (like edges) *)
        p_vertices : Vertice_set.t;                                               (* involves a set of vertices *)
        p_path : edge list }                                                      (* and is an ordered list of edges *)
        
      and loop = {                                                                (* a loop *)
        mutable l_path : Edge.t list;                                             (* is defined by a path going through all the edges of the loop *)
        l_edges : Edge_set.t;                                                     (* the set of all the edges in the loop *)
        l_edges_in : Edge_set.t;                                                  (* contains in annex the set of edges that enter the loop *)
        l_edges_out : Edge_set.t;                                                 (* contains in annex the set of edges that exit the loop *)
        l_abst : to_component Lazy.t }                                            (* is part of the abstract graph (its representation can change in case loops are merged) *)
             
      and to_component = {                                                        (* An element of the abstract graph (called topological order) *)
        mutable to_data     : to_data;                                            (* is either a vertice or a loop *)
        mutable to_position : int;                                                (* has an position in the order *)
        mutable to_status   : to_status }                                         (* is either a root, a leaf or a inner element *)
      and to_data = Loop of Loop.t | Vertice of Vertice.t  
      and to_status
      
      val to_status_inner : to_status                                             (* status of an element with nothing in particular *)
      val to_status_root  : to_status                                             (* status of a root element *)
      val to_status_leaf  : to_status                                             (* status of a leaf element *)
      val to_status_combine : to_status ->  to_status ->  to_status               (* function that combine status *)
      val to_status_is_root : to_status -> bool                                   (* tests if the element is a root *)
      val to_status_is_leaf : to_status -> bool                                   (* tests if the element is a leaf *)
      val to_status_remove : to_status -> to_status -> to_status                  (* remove a status from a status *)

    end = struct   (* Once again, every data types with the corresponding comments *)
      type vertice = {                                                            (* a vertice *)
        v_id : vertice_id;                                                        (* has a unique id *)
        v_data : vertice_data;                                                    (* contains some data *)
        mutable v_successors : Edge_set.t; mutable v_predecessors : Edge_set.t;   (* links to its input and output edges (these lists can grow) *)
        mutable v_parse_tag : bool;                                               (* has a tag for basic parsing *)
        mutable v_abst : to_component Lazy.t }                                    (* is part of the abstract graph (its representation can change in case loops are created) *)
        
      and edge = {                                                                (* an edge *)
        e_id : edge_id;                                                           (* has a unique id *)
        e_data : edge_data;                                                       (* contains some data *)
        e_origin : Vertice.t; e_target : Vertice.t;                               (* has an origin and a target vertice *)
        mutable e_loop_tag : edge_loop_tag }                                      (* and some annex informations concerning loops (I don't know if still useful) *)
      and edge_loop_tag = Edge_no_loop | Edge_loop of Loop.t | Edge_loop_in of Loop.t | Edge_loop_out of Loop.t

      and path = {                                                                (* a path *)
        p_origin : Vertice.t; p_end : Vertice.t;                                  (* has an origin and a target vertice (like edges) *)
        p_vertices : Vertice_set.t;                                               (* involves a set of vertices *)
        p_path : edge list }                                                      (* and is an ordered list of edges *)

      and loop = {                                                                (* a loop *)
        mutable l_path : Edge.t list;                                             (* is defined by a path going through all the edges of the loop *)
        l_edges : Edge_set.t;                                                     (* the set of all the edges in the loop *)
        l_edges_in : Edge_set.t;                                                  (* contains in annex the set of edges that enter the loop *)
        l_edges_out : Edge_set.t;                                                 (* contains in annex the set of edges that exit the loop *)
        l_abst : to_component Lazy.t }                                            (* is part of the abstract graph (its representation can change in case loops are merged) *)

      and to_component = {                                                        (* An element of the abstract graph (called topological order) *)
        mutable to_data     : to_data;                                            (* is either a vertice or a loop *)
        mutable to_position : int;                                                (* has an position in the order *)
        mutable to_status   : to_status }                                         (* is either a root, a leaf or a inner element *)
      and to_data = Loop of Loop.t | Vertice of Vertice.t
      and to_status = int

      let to_status_inner = 0
      let to_status_root = 1
      let to_status_leaf = 2
      let to_status_combine = (lor)

      let to_status_is_root s = (s land to_status_root) != 0
      let to_status_is_leaf s = (s land to_status_leaf) != 0
      let to_status_remove s1 s2 = s1 land (lnot s2)

    end and Vertice : sig
      type t = Data_types.vertice                                  (* type of a vertice *)
      val create : vertice_data -> t                               (* create a vertice with it's abstract representation *)
      
      val data    : t -> V.t                                       (* access the data of a vertice *)
      val succs_e : t -> Edge_set.t                                (* access the edges exiting the vertice *)
      val preds_e : t -> Edge_set.t                                (* access the edges entering the vertice *)
      val succs_v : t -> Vertice_set.t                             (* access the son vertices of that vertice *)
      val preds_v : t -> Vertice_set.t                             (* access the parent vertices of that vertice *)
      
      val parse_tag   : t -> unit                                  (* set the vertice as parsed *)
      val is_parsed   : t -> bool                                  (* returns if the vertice was parsed *)
      val parse_untag : t -> unit                                  (* set the vertice as not parsed *)
      val mod_abst    : Data_types.to_component -> t -> unit       (* change the abstract representation of the vertice *)
      val get_abst    : t -> Data_types.to_component               (* returns the abstract representation of the vertice *)
      
      val compare : t -> t -> int                                  (* compare two vertices *)
      val equal   : t -> t -> bool                                 (* wrapper over compare for simple equality test between two vertices *)
    end = struct
      open Data_types
      type t = vertice

      let new_abst v =                  (* the initial abstract representation of a vertice *)
        let status = Data_types.to_status_combine Data_types.to_status_root Data_types.to_status_leaf in
        { to_position = 0; to_data = Vertice v; to_status = status }
      let id = Fresh_integer.create ()  (* unique identifiers for vertices *)
      let create data = let rec res = {
        v_id = Fresh_integer.next id;
        v_data = data; 
        v_successors = Edge_set.empty; v_predecessors = Edge_set.empty; 
        v_parse_tag = false;
        v_abst = lazy (new_abst res) } in res
      
      let data n  = n.v_data
      let succs_e n = n.v_successors
      let preds_e n = n.v_predecessors
      let succs_v n = Edge_set_to_vertices.convert Edge.target (succs_e n)
      let preds_v n = Edge_set_to_vertices.convert Edge.origin (preds_e n)
      
      let parse_tag   v = v.v_parse_tag <- true
      let is_parsed   v = v.v_parse_tag
      let parse_untag v = v.v_parse_tag <- false
      
      let get_abst      v = Lazy.force v.v_abst
      let mod_abst abst v = v.v_abst <- Lazy.lazy_from_val abst
      
      let compare v1 v2 = v1.v_id - v2.v_id
      let equal v1 v2 = (compare v1 v2) = 0
    end and Vertice_set : Set.S with type elt = Vertice.t = Set.Make(Vertice) and Vertice_map : Map.S with type key = Vertice.t = Map.Make(Vertice)
    
    and Edge : sig
      type t = Data_types.edge                               (* type of an edge *)
      val create : Vertice.t -> edge_data -> Vertice.t -> t  (* create an edge with an origin, a data and a target *)
      val data   : t -> E.t                                  (* access the data of the edge *)
      val origin : t -> Vertice.t                            (* access the origin of the edge *)
      val target : t -> Vertice.t                            (* access the target of the edge *)

      val origin_abst : t -> Data_types.to_component         (* access the abstract component of the origin vertice *)
      val target_abst : t -> Data_types.to_component         (* access the abstract component of the target vertice *)
      
      val loop_tag     : Loop.t -> t -> unit                 (* set the edge to be part of the loop in parameter *)
      val loop_tag_in  : Loop.t -> t -> unit                 (* set the edge to enters the loop in parameter *)
      val loop_tag_out : Loop.t -> t -> unit                 (* set the edge to exit the loop in parameter *)
      val loop_untag   : t -> unit                           (* remove all information concerning loops from the edge *)
      
      val is_loop      : t -> bool                           (* returns if the edge is part of a loop *)
      val is_loop_in   : t -> bool                           (* returns if the edge enters a loop *)
      val is_loop_out  : t -> bool                           (* returns if the edge exits a loop *)
(*      val loop_get     : t -> Loop.t                         (* returns the loop concerned by the formentioned functions (TODO: Bug if an edge links two loops?) *)*)
      
      val compare : t -> t -> int                            (* compare two edges *)
      val equal   : t -> t -> bool                           (* wrapper over compare for simple equality test between two edges *)
    end = struct
      open Data_types
      type t = edge
      let id = Fresh_integer.create ()
      let create o data t = let res = {
        e_id = Fresh_integer.next id; 
        e_data = data; 
        e_origin = o; e_target = t; e_loop_tag = Edge_no_loop } in
        o.v_successors <- Edge_set.add res o.v_successors; t.v_predecessors <- Edge_set.add res t.v_predecessors; res
      
      let data e  = e.e_data
      let origin e = e.e_origin
      let target e = e.e_target
      
      let origin_abst e = Vertice.get_abst (origin e)
      let target_abst e = Vertice.get_abst (target e)
      
      let loop_tag     id e = e.e_loop_tag <- Edge_loop id
      let loop_tag_in  id e = e.e_loop_tag <- Edge_loop_in id
      let loop_tag_out id e = e.e_loop_tag <- Edge_loop_out id
      let loop_untag      e = e.e_loop_tag <- Edge_no_loop
      let is_loop     e = match e.e_loop_tag with | Edge_loop _ -> true | _ -> false (* TODO: replace *)
      let is_loop_in  e = match e.e_loop_tag with | Edge_loop_in _ -> true | _ -> false (* TODO: replace *)
      let is_loop_out e = match e.e_loop_tag with | Edge_loop_out _ -> true | _ -> false (* TODO: replace *)
      
      let compare e1 e2 = e1.e_id - e2.e_id
      let equal e1 e2 = (compare e1 e2) = 0
    end and Edge_set : Set.S with type elt = Edge.t = Set.Make(Edge) and Edge_map : Map.S with type key = Edge.t = Map.Make(Edge)
    and Edge_set_to_vertices : sig val convert : (Edge.t -> Vertice.t) -> Edge_set.t -> Vertice_set.t end = Set.Convert(Edge_set)(Vertice_set)
    and Vertice_set_to_edges : sig val set_convert : (Vertice.t -> Edge_set.t) -> Vertice_set.t -> Edge_set.t end = Set.Convert(Vertice_set)(Edge_set)

    and Path : sig
      type t = Data_types.path                               (* Type of paths *)
      exception Invalid_path_extension                       (* Exception raised when trying to extend a path with an edge that does not originate from the right vertice *)

      val create      : Vertice.t -> t                       (* Create an empty path originating from the vertice in parameter *)
      val is_empty    : t -> bool                            (* tests if the path is empty *)
      val add         : Edge.t -> t -> t                     (* extends the path with a new edge *)
      val of_edges    : Edge.t list -> t                     (* create a path from a non-empty list of edges, or raise Invalid_path_extension *)
      
      val origin      : t -> Vertice.t                       (* access the origin of the path *)
      val target      : t -> Vertice.t                       (* access the target of the path *)
      val mem         : Vertice.t -> t -> bool               (* tests if the vertice in parameter is part of the path *)
      val vertices    : t -> Vertice_set.t                   (* access the set of vertices that are part of the path *)
      val edges       : t -> Edge_set.t                      (* access the set of edges that are part of the path *)
      val edges_rev   : t -> Edge.t list                     (* returns the list of edges defining the path, going from the target until the origin *)
      
      val extract     : Vertice.t -> Vertice.t -> t -> t     (* Extract a sub path that starts and ends with the two vertices in parameter, or raise Not_found *)
      val split       : Vertice.t -> t -> t list             (* returns the maximal decomposition of p such that all sub-path originate or ends on the vertice in parameter (or returns p if the vertice is not part of it) *)
      val concat      : t -> t -> t                          (* returns p1.p2 when such a path is valid (or raise Invalid_path_extension) *)
      val concat_list : t list -> t                          (* wrapper over concat to concat a list of paths. Raise Invalid_path_extension if the input list is empty *)
      val is_loop     : t -> bool                            (* tests if the path is a loop *)

      val compare : t -> t -> int                            (* compare two paths *)
      val equal   : t -> t -> bool                           (* wrapper over compare for simple equality test between two paths *)
    end = struct
      open Data_types
      type t = path

      exception Invalid_path_extension

      let create o = { p_origin = o; p_end = o; p_vertices = Vertice_set.singleton o; p_path = []}
      let is_empty p = match p.p_path with | [] -> true | _ -> false
      let add e p = if Vertice.equal p.p_end (Edge.origin e) then raise (Invalid_path_extension) else let t = Edge.target e in {
          p_origin = p.p_origin;
          p_end = t;
          p_vertices = Vertice_set.add t p.p_vertices;
          p_path = e::p.p_path }
      
      let of_edges l = match l with
        | [] -> raise Invalid_path_extension
        | e::_ -> let p = create (Edge.origin e) in List.fold_right (fun e res -> add e res) l p
      
      let origin p = p.p_origin
      let target p = p.p_end
      let mem n p = Vertice_set.mem n p.p_vertices
      let vertices p = p.p_vertices
      let edges p = Edge_set.set_of_direct_list p.p_path
      let edges_rev p = p.p_path
      
      let extract v1 v2 p =
        let rec remove_end l = match l with
          | [] -> raise Not_found
          | e::l' -> if Vertice.equal (Edge.target e) v2 then l else remove_end l' in
        let rec keep_path l = match l with
          | [] -> raise Not_found
          | e::l' -> if Vertice.equal (Edge.origin e) v1 then [e] else e::(keep_path l') in
        of_edges (keep_path (remove_end p.p_path))
      
      let split v p =
        let rec f l = match l with
          | [] -> ([],[])
          | e::l' -> let (accu,res) = f l' in if Vertice.equal (Edge.target e) v then ([], (e::accu)::res) else (e::accu, res) in
        if is_empty p then [p]
        else (
          let (p', ps) = f p.p_path in
          if List.is_empty p' then List.map of_edges ps else List.map of_edges (p'::ps) )
        
      let concat p1 p2 = if Vertice.equal (origin p2) (target p1) then {
        p_origin = p1.p_origin;
        p_end = p2.p_end;
        p_vertices = Vertice_set.union p1.p_vertices p2.p_vertices;
        p_path = p2.p_path @ p1.p_path
      } else raise Invalid_path_extension
      let rec concat_list l = match l with
        | [] -> raise Invalid_path_extension
        | [p] -> p
        | p::l -> let p' = concat_list l in concat p p'
      
      let is_loop p = (Vertice.equal p.p_origin p.p_end) && (not (is_empty p))

      let compare p1 p2 =
        let rec f dl = match dl with
          | ([], [])         -> 0
          | (e1::l1, e2::l2) -> let tmp = Edge.compare e1 e2 in if tmp != 0 then tmp else f (l1,l2)
          | ( []   , _     ) -> -1
          | ( _    , []    ) ->  1 in f (p1.p_path, p2.p_path)
      let equal p1 p2 = (compare p1 p2 = 0)
    end and Path_set : Set.S with type elt = Path.t = Set.Make(Path) and Path_map : Map.S with type key = Path.t = Map.Make(Path)

(*    
    and Loop : sig
      type t = Data_types.loop                                      (* type of loops *)
      exception Invalid_loop_extension                              (* exception raised when extending a loop with a unrelated path *)
      
      val create   : Path.t -> t                                    (* create a loop and its abstract representation from a path that must be a loop *)
      val add      : Path.t -> t -> unit                            (* extends the loop with a new adjascent loop *)
      
      val vertices : t -> Vertice_set.t                             (* access the vertices that are part of the loop *)
      val edges    : t -> Edge_set.t                                (* access the edges that are part thof the loop *)
      val succs_e  : t -> Edge_set.t                                (* access the edges that exit the loop *)
      val preds_e  : t -> Edge_set.t                                (* access the edges that enter the loop *)
      val succs_v  : t -> Vertice_set.t                             (* access the vertices that are pointed by the loop *)
      val preds_v  : t -> Vertice_set.t                             (* access the vertices that point to the loop *)

      val mod_abst    : Data_types.to_component -> t -> unit        (* change the abstract representation of the loop *)
      val get_abst    : t -> Data_types.to_component                (* returns the abstract representation of the loop *)
      
      val mem    : Vertice.t -> t -> bool                           (* tests if the vertice is part of the loop *)
      val center : Vertice.t -> t -> unit                           (* set the origin and target of the inner path of the loop to the vertice in parameter *)
      
      val main_loop : t -> Path.t                                   (* returns the main path of the loop *)
      val sub_loops : t -> Path_set.t                               (* returns the minimal loops that are part of the one in parameter *)
      
      val compare : t -> t -> int                                   (* compare two loops *)
      val equal   : t -> t -> bool                                  (* wrapper over compare for simple equality test between two loops *)
    end = struct
      open Data_types
      type t = loop
      exception Invalid_loop_extension

      let vertices l = Path.vertices l.l_main_path
      let edges    l = Path.edges l.l_main_path
      let succs_e  l = l.l_edges_out
      let succs_v  l = Edge_set_to_vertices.convert Edge.target l.l_edges_out
      let preds_e  l = l.l_edges_in
      let preds_v  l = Edge_set_to_vertices.convert Edge.origin l.l_edges_in
      let mem v l = Path.mem v l.l_main_path
      let center v l = if mem v l then match Path.split v l.l_main_path with
        | p1::l1 -> l.l_main_path <- Path.concat_list (l1 @ [p1])
        | _ ->  raise Invalid_loop_extension (* NOT POSSIBLE CASE *)

      let mod_abst abst l = l.l_abst <- Lazy.lazy_from_val abst
      let get_abst      l = Lazy.force l.l_abst

      
      let main_loop l = l.l_main_path
      let sub_loops l = l.l_paths
      
      let new_abst l =
        let root = if Edge_set.is_empty (preds_e l) then Data_types.to_status_root else Data_types.to_status_inner in
        let leaf = if Edge_set.is_empty (succs_e l) then Data_types.to_status_leaf else Data_types.to_status_inner in
       { to_position = 0; to_data = Loop l; to_status = Data_types.to_status_combine root leaf; to_next = None }
      let id = Fresh_integer.create ()
      let create p = if not (Path.is_loop p) then raise Invalid_loop_extension else (
        let vs = Path.vertices p in let es = Path.edges p in
        let e_in  = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.preds_e v) res) vs Edge_set.empty) es in
        let e_out = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.succs_e v) res) vs Edge_set.empty) es in
        let paths = Path_set.singleton p (* loops are created inductively, and thus, are created minimal *) in
        let rec res = {
          l_id = Fresh_integer.next id;
          l_main_path = p;
          l_paths = paths; l_edges_in = e_in; l_edges_out = e_out;
          l_abst = lazy (new_abst res) } in
        Vertice_set.iter (fun v -> Vertice.mod_abst (get_abst res) v) vs; Edge_set.iter (fun e -> Edge.loop_tag res e) es; res
      )
      
      let add p l = () (* if Path.is_loop p then ( (* TODO: redo this function for inductive extension of loops *)
        let vs_common = Vertice_set.inter (Path.vertices p) (vertices l) in
        if Vertice_set.is_empty (vs_common) then raise Invalid_loop_extension
        else (
          let v = Vertice_set.choose vs_common in
          let p_res = match (Path.split v p, Path.split v l.l_main_path) with
            | (p1::l1, p2::l2) -> Path.concat_list ((l1 @ [p1]) @ (l2 @ [p2]))
            | _ ->  raise Invalid_loop_extension (* NOT POSSIBLE CASE *) in
          let vs = Path.vertices p in let es = Path.edges p in
          Vertice_set.iter (fun v -> Vertice.loop_tag l v) vs; Edge_set.iter (fun e -> Edge.loop_tag l e) es;
          let e_in  = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.preds_e v) res) vs l.l_edges_in) es in
          let e_out = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.succs_e v) res) vs l.l_edges_out) es in
          let paths = Path_set.set_of_direct_list (Path.split (Path.origin p) p) in
          l.l_main_path <- p_res; l.l_paths <- Path_set.union paths l.l_paths; l.l_edges_in  <- e_in; l.l_edges_out <- e_out
        )) else raise Invalid_loop_extension *)
            
      let compare l1 l2 = l1.l_id - l2.l_id
      let equal l1 l2 = (compare l1 l2) = 0
     end and Loop_set : Set.S with type elt = Loop.t = Set.Make(Loop) and Loop_map : Map.S with type key = Loop.t = Map.Make(Loop)
*)
    and Loop : sig
      type t = Data_types.loop                                      (* type of loops *)
      
      val create : Edge_set.t -> t                                  (* create a loop and its abstract representation from a set of edges that must be a loop (no check performed). *)
      
      val vertices : t -> Vertice_set.t                             (* access the vertices that are part of the loop *)
      val edges    : t -> Edge_set.t                                (* access the edges that are part thof the loop *)
      val succs_e  : t -> Edge_set.t                                (* access the edges that exit the loop *)
      val preds_e  : t -> Edge_set.t                                (* access the edges that enter the loop *)
      val succs_v  : t -> Vertice_set.t                             (* access the vertices that are pointed by the loop *)
      val preds_v  : t -> Vertice_set.t                             (* access the vertices that point to the loop *)
      
      val get_abst    : t -> Data_types.to_component                (* returns the abstract representation of the loop *)
      
      val mem    : Vertice.t -> t -> bool                           (* tests if the vertice is part of the loop *)
      val center : Vertice.t -> t -> unit                           (* set the origin and target of the inner path of the loop to the vertice in parameter *)
      val path : t -> Edge.t list                                   (* returns the list of ordered edges starting at the centered vertice, to parse the loop *)
      
      val compare : t -> t -> int                                   (* compare two loops *)
      val equal   : t -> t -> bool                                  (* wrapper over compare for simple equality test between two loops *)
    end = struct open Data_types
      type t = Data_types.loop                                      (* type of loops *)

      let edges l = l.l_edges
      let vertices l = Edge_set_to_vertices.convert Edge.origin (edges l)
      let succs_e  l = l.l_edges_out
      let succs_v  l = Edge_set_to_vertices.convert Edge.target l.l_edges_out
      let preds_e  l = l.l_edges_in
      let preds_v  l = Edge_set_to_vertices.convert Edge.origin l.l_edges_in
      
      let mem v l = Vertice_set.mem v (vertices l)
      let get_abst l = Lazy.force l.l_abst

      let new_abst l =                  (* the initial abstract representation of a loop *)
        let root = if Edge_set.is_empty l.l_edges_in then Data_types.to_status_root else Data_types.to_status_inner in
        let leaf = if Edge_set.is_empty l.l_edges_out then Data_types.to_status_leaf else Data_types.to_status_inner in
        { to_position = 0; to_data = Data_types.Loop l; to_status = Data_types.to_status_combine root leaf }

      let create es =
        let vs = Edge_set_to_vertices.convert Edge.origin es in
        let in_es = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.preds_e v) res) vs Edge_set.empty) es in
        let out_es = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.succs_e v) res) vs Edge_set.empty) es in
        let rec res = { l_path = []; l_edges = es; l_edges_in = in_es; l_edges_out = out_es; l_abst = lazy (new_abst res) } in res

      let center v l =
        let rec step vs es = 
          if Edge_set.is_empty es then [] (* we parsed all the edges *)
          else let next_es = Edge_set.inter es (Vertice_set_to_edges.set_convert Vertice.succs_e vs) in
          (Edge_set.elements next_es) @ (step (Edge_set_to_vertices.convert Edge.target next_es) (Edge_set.diff es next_es)) in
        l.l_path <- step (Vertice_set.singleton v) (edges l)

      let path l = (if List.is_empty l.l_path then center (Edge.target (Edge_set.choose (edges l))) l); l.l_path
      
      let compare = Pervasives.compare
      let equal l1 l2 = (compare l1 l2) = 0
    end and Loop_set : Set.S with type elt = Loop.t = Set.Make(Loop) and Loop_map : Map.S with type key = Loop.t = Map.Make(Loop)


    module Topological_order (*: sig
      module rec Component : sig
        type t = Data_types.to_component                   (* An element of the order, abstracting vertices and loops *)
        type data = Data_types.to_data 
        type status                                        (* an element of the order can be a root of the graph, a leaf, both or none *)

        val data : t -> data
        val vertices : t -> Vertice_set.t
        val succs : t -> Component_set.t                   (* returns the components that follow the given one following the edges of the real graph *)
        val preds : t -> Component_set.t                   (* returns the components that preceed the given one following the edges of the real graph *)

        val status_inner : status                          (* status of an element with nothing in particular *)
        val status_root  : status                          (* status of a root element *)
        val status_leaf  : status                          (* status of a leaf element *)
        val status_combine : status ->  status ->  status  (* function that combine status *)

        val is_loop : t -> bool                            (* tests if the element is a loop *)
        val is_root : t -> bool                            (* tests if the element is a root *)
        val is_leaf : t -> bool                            (* tests if the element is a leaf *)

        val compare : t -> t -> int
      end and Component_set : Set.S with type elt = Component.t
      
      type t                                             (* Type of total topological ordering, used for automatic loop detection *)
      val create : unit -> t                             (* create a new, empty order *)

      val add_vertice : Vertice.t -> t -> unit           (* add a new vertice to the order *)
      val add_edge : Edge.t -> t -> unit                 (* add a new edge to the order, which triggers the automatic restructuration of the order *)
            

    end *)= struct
      open Data_types
      
      module rec Component : sig
        type t = Data_types.to_component                   (* An element of the order, abstracting vertices and loops *)
        type data = Data_types.to_data  
        type status = Data_types.to_status                 (* an element of the order can be a root of the graph, a leaf, both or none *)

        val data : t -> data                               (* the data of the component *)
        val status : t -> status                           (* the status of the component *)
        val position : t -> int                            (* the position of the component in the order *)
        val vertices : t -> Vertice_set.t                  (* the vertices contained in the component *)
        val edges    : t -> Edge_set.t
        val succs_v : t -> Vertice_set.t                   (* returns the vertices that follow the given one following the edges of the real graph *)
        val succs_e : t -> Edge_set.t                      (* returns the edges that follow the given one following the edges of the real graph *)
        val succs_c : t -> Component_set.t                 (* returns the components that follow the given one following the edges of the real graph *)
        val preds_v : t -> Vertice_set.t                   (* returns the vertices that preceed the given one following the edges of the real graph *)
        val preds_e : t -> Edge_set.t                      (* returns the edges that preceed the given one following the edges of the real graph *)
        val preds_c : t -> Component_set.t                 (* returns the components that preceed the given one following the edges of the real graph *)
        
        (*val path : Vertice.t -> Vertice.t -> Path.t      (* returns a path between the two vertices, using the edges in their component, or raise Not_found *)*)
        
        val paths_towards : t -> t -> Abstract_path_set.t  (* returns the set of abstract path that goes from c1 until the maximal c such that c <= c2 *)
        
        val merge : Abstract_path_set.t -> Edge.t -> t     (* merge all components together with the edge in a big loop component *)
        val add_edge : Edge.t -> t -> unit                 (* addition of an edge to the component *)
        
        val is_loop : t -> bool                            (* tests if the element is a loop *)
        val is_root : t -> bool                            (* tests if the element is a root *)
        val is_leaf : t -> bool                            (* tests if the element is a leaf *)
        val isnt_anymore : status -> t -> unit             (* disable the status in parameter from the component *)

        val compare : t -> t -> int                        (* compare two components *)
        val equal   : t -> t -> bool                       (* wrapper over compare for simple equality test between two components *)
      end = struct
        type t = Data_types.to_component
        type data = Data_types.to_data  
        type status = Data_types.to_status

        let data c = c.to_data
        let status c = c.to_status
        let position c = c.to_position
        let vertices c = match data c with
          | Vertice v' -> Vertice_set.singleton v'
          | Loop l -> Loop.vertices l
        let edges c = match data c with
          | Vertice _ -> Edge_set.empty
          | Loop l -> Loop.edges l

        let compare c1 c2 = (position c1) - (position c2)
        let equal c1 c2 = (compare c1 c2) = 0

        let generic_add v s = Component_set.add (Vertice.get_abst v) s
        
        let succs_v c = match data c with | Vertice v' -> Vertice.succs_v v' | Loop l -> Loop.succs_v l
        let succs_e c = match data c with | Vertice v' -> Vertice.succs_e v' | Loop l -> Loop.succs_e l
        let succs_c c = match data c with
          | Vertice v' -> Vertice_set.fold generic_add (Vertice.succs_v v') Component_set.empty
          | Loop l -> Vertice_set.fold generic_add (Loop.succs_v l) Component_set.empty
        let preds_v c = match data c with | Vertice v' -> Vertice.preds_v v' | Loop l -> Loop.preds_v l
        let preds_e c = match data c with | Vertice v' -> Vertice.preds_e v' | Loop l -> Loop.preds_e l
        let preds_c c = match data c with
          | Vertice v' -> Vertice_set.fold generic_add (Vertice.preds_v v') Component_set.empty
          | Loop l -> Vertice_set.fold generic_add (Loop.preds_v l) Component_set.empty
        
(*        let path v1 v2 = let c = Vertice.get_abst v1 in if compare c (Vertice.get_abst v2) != 0 then raise Not_found else match data c with
          | Vertice v -> Path.create v
          | Loop l -> Path.extract v1 v2 (Loop.main_loop l)*) (* pb with main_loop. what could I do ?? *)
        
        
       (********************* for debug *)
       let print_c c = Printf.printf "%i " (Vertice_set.choose (vertices c)).v_id
       (********************* for debug *)

        
        let paths_towards c1 c2 =
          Printf.printf "stating with the component "; print_c c1; Printf.printf "\n";
          let rec step accu = Printf.printf "sub\n";
            let succs_e = Edge_set.filter (fun e -> ((compare (Edge.target_abst e) c2) <= 0) && ((compare c1 (Edge.target_abst e)) < 0)) (succs_e (Abstract_path.target accu)) in
            if Edge_set.is_empty succs_e then (Printf.printf "that path is finished\n"; Abstract_path_set.singleton accu)
            else Edge_set.fold (fun e res -> Printf.printf "going further -> "; print_c (Edge.target_abst e); Printf.printf "\n"; Abstract_path_set.union (step (Abstract_path.add e accu)) res) succs_e Abstract_path_set.empty in
          step (Abstract_path.create c1)

        let update_vertices c = Vertice_set.iter (fun v -> Printf.printf "updating data for vertice %i\n" (v.v_id);Vertice.mod_abst c v) (vertices c)

        let merge ps e = (* function called when the addition of the edge e from c2 to c1 created a loop between c1 and c2, involving the paths ps from c1 to c2  *)
          (* simply create a new component with all the edges of c1, c2,  ps and e inside *)
          let l = Loop.create (Abstract_path_set.fold (fun p res -> Edge_set.union (Abstract_path.edges p) res) ps (Edge_set.singleton e)) in
          let res = Loop.get_abst l in update_vertices res; res

        let add_edge e c = let res = Loop.get_abst (match data c with
           | Vertice v -> (Loop.create (Edge_set.singleton e))
           | Loop l -> (Loop.create (Edge_set.add e (Loop.edges l)))) in
           c.to_data <- data res;
           c.to_status <- status res;
           Vertice.mod_abst c (Edge.origin e); Vertice.mod_abst c (Edge.target e)
        
        let is_loop c = match data c with Loop _ -> true | _ -> false
        let is_root c = to_status_is_root (status c)
        let is_leaf c = to_status_is_leaf (status c)
        let isnt_anymore s c = c.to_status <- to_status_remove s (status c)
        
        
      end and Component_set : sig
        include Set.S with type elt = Component.t
        val vertices : t -> Vertice_set.t
        val loops    : t -> Loop_set.t
      end = struct
        include Set.Make(Component)
        
        let vertices cs = fold (fun c res -> match (Component.data c) with Vertice v -> Vertice_set.add v res | _ -> res) cs Vertice_set.empty
        let loops cs = fold (fun c res -> match (Component.data c) with Loop l -> Loop_set.add l res | _ -> res) cs Loop_set.empty
      end
      
      and Abstract_path : sig
        type t = { origin : Component.t; target : Component.t; path : Edge.t list } (* the type of an abstract path *)
        exception Invalid_path_extension                                            (* exception when trying to construct an ill-defined path *)
       
       val create : Component.t -> t                                                (* create a new empty path *)
       val add : Edge.t -> t -> t                                                   (* add a new edge to the path, or raise Invalid_path_extension *)
       val of_edges : Edge.t list -> t                                              (* create an abstract path from edges, or raise Invalid_path_extension *)
       val concat : t -> t -> t                                                     (* concat two abstract paths, or raise Invalid_path_extension *)
       
       val origin : t -> Component.t                                                (* the origin component of the path *)
       val target : t -> Component.t                                                (* the target component of the path *)
       val components : t -> Component_set.t                                        (* the components involved in the path *)
       val edges  : t -> Edge_set.t
(*       val path : Vertice.t -> t -> Vertice.t -> Path.t                             (* Compute a real path from an abstract one *) *)
       
       val compare : t -> t -> int                                                  (* compare two paths *)
      end = struct
        type t = { origin : Component.t; target : Component.t; path : Edge.t list }
        exception Invalid_path_extension
        
        let origin p = p.origin
        let target p = p.target
        let components p = List.fold_left (fun res e -> Component_set.add (Edge.target_abst e) res) (Component_set.singleton (origin p)) p.path
        let edges p = Component_set.fold (fun c res -> Edge_set.union (Component.edges c) res) (components p) (Edge_set.set_of_direct_list p.path)
        
        let create o = {origin = o; target = o; path = [] }
        let add e p =  if Component.equal (target p) (Edge.origin_abst e) then
          { origin = p.origin; target = Edge.target_abst e; path = e::p.path } else raise Invalid_path_extension
          
        let of_edges l = match l with
          | [] -> raise Invalid_path_extension
          | e::_ -> let p = create (Edge.origin_abst e) in List.fold_right (fun e res -> add e res) l p

        let concat p1 p2 = if Component.equal (target p1) (origin p2) then { origin = p1.origin; target = p2.target; path = p2.path @ p1.path } else raise Invalid_path_extension
        
(*        let path v1 p v2 =
          let edges_between v1 v2 = List.rev (Path.edges_rev (Component.path v1 v2)) in
          let rec inner_path l v_target = match l with
            | [] -> []
            | e::l -> (inner_path l (Edge.origin e)) @ (e::(edges_between (Edge.target e) v_target)) in
          match p.path with
            | [] -> Component.path v1 v2
            | e::l  -> Path.of_edges ((edges_between v1 (Edge.origin e)) @ (inner_path p.path v2))*)
        
        let compare = Pervasives.compare
      end and Abstract_path_set : Set.S with type elt = Abstract_path.t = Set.Make(Abstract_path)

      
      type t = Component.t Linked_list.t
      
      let iter_right = Linked_list.iter_right
      let iter_left = Linked_list.iter_left
      let iter = Linked_list.iter
      let fold = Linked_list.fold_right
      
      let prefix c o = Linked_list.filter (fun c' -> (Component.compare c' c) < 0) o
      let suffix c o = Linked_list.filter (fun c' -> (Component.compare c' c) > 0) o
      let sub c1 c2 o = Linked_list.filter (fun c' -> ((Component.compare c' c1) > 0) && ((Component.compare c' c2) < 0)) o
      
      let reset_order o = Pervasives.ignore (Linked_list.fold_right (fun c i -> c.to_position <- i; i + 1) o 0)
      
       (********************* for debug *)
       let string_of_c c = Printf.sprintf "(%i, % i)" ((Vertice_set.choose (Component.vertices c)).v_id) (Component.position c)
       let string_of_o o = String.concat " " ((Linked_list.fold_left (fun res c -> (string_of_c c)::res) [] o))
       (********************* for debug *)
      
      let create  = Linked_list.create
      
      let add_vertice v o = let c = Vertice.get_abst v in let id = try (Component.position (Linked_list.last o)) + 1 with | Not_found -> 0 in
        c.to_position <- id; Linked_list.add_last c o;
        Printf.printf "new vertice added. Now the order is : %s\n" (string_of_o o); flush stdout
      
      let add_edge e o = let c1 = Edge.origin_abst e in let c2 = Edge.target_abst e in let value = Component.compare c1 c2 in
        Printf.printf "Adding a new edge %s -> %s to the graph\n" (string_of_c c1) (string_of_c c2);
          Component.isnt_anymore to_status_leaf c1;
          Component.isnt_anymore to_status_root c2;
        if value < 0 then (Printf.printf "The edge is ok for the order. Nothing to do, yeah\n"; o)
        
        else if value > 0 then ( (* c2 is before c1, which is against the edge. We thus need to update the order *)
          Printf.printf "The edge is inverted for the order. Computing the paths from c2 toward c1...";
          let ps = Component.paths_towards c2 c1 in
          Printf.printf " ok\n"; flush stdout;
          let (ps_loop, ps_unloop) = Abstract_path_set.partition (fun p -> Component.equal c1 (Abstract_path.target p)) ps in
          Printf.printf " partition of the set of path done\n"; flush stdout;
          let path_set_to_component_set ps = Abstract_path_set.fold (fun p res -> Component_set.union (Abstract_path.components p) res) ps Component_set.empty in
          let cs_in_between = Component_set.set_of_direct_list (Linked_list.to_list (sub c2 c1 o)) in
          Printf.printf " computation of the components in between done\n"; flush stdout;
          let cs_loop = path_set_to_component_set ps_loop in
          Printf.printf " computation of the components in loop done\n"; flush stdout;
          let cs_unloop = path_set_to_component_set ps_unloop in
          Printf.printf " computation of the components in unloop done\n"; flush stdout;
          let cs_move = Component_set.diff cs_in_between cs_loop in
          Printf.printf " computation of the components to move done\n"; flush stdout;
          let (cs_forward, cs_backward) = Component_set.partition (fun c -> Component_set.mem c cs_unloop) cs_move in
          Printf.printf " partition of the set of components done\n"; flush stdout;
          Printf.printf "  we need to merge the components : { %s }\n" (String.concat " " (Component_set.fold (fun c res -> (string_of_c c)::res) cs_loop [])); flush stdout;
          Printf.printf "  we need to move backward the components  : { %s }\n" (String.concat " " (Component_set.fold (fun c res -> (string_of_c c)::res) cs_backward [])); flush stdout;
          Printf.printf "  we need to move forward the components   : { %s }\n" (String.concat " " (Component_set.fold (fun c res -> (string_of_c c)::res) cs_forward [])); flush stdout;
          (* until here, seems ok *)
          
          (* create the new order *)
          let prefix1 = prefix c2 o in                                              (* the ones that are bigger than c1 *)
          let prefix2 = Linked_list.filter (fun c -> Component_set.mem c cs_backward) o in  (* the ones that must be moved forward *)
          let center = let o' = Linked_list.create () in
            (if Component_set.is_empty cs_loop then (Linked_list.add c1 o'; Linked_list.add c2 o')
             else Linked_list.add (Component.merge ps_loop e) o'); o' in
          let suffix1 = Linked_list.filter (fun c -> Component_set.mem c cs_forward) o in
          let suffix2 = suffix c1 o in
          Printf.printf "The prefix1 is : %s\n" (string_of_o prefix1); flush stdout;
          Printf.printf "The prefix2 is : %s\n" (string_of_o prefix2); flush stdout;
          Printf.printf "The center is : %s\n" (string_of_o center); flush stdout;
          Printf.printf "The suffix1 is : %s\n" (string_of_o suffix1); flush stdout;
          Printf.printf "The suffix2 is : %s\n" (string_of_o suffix2); flush stdout;
            
          let new_o = Linked_list.concat [prefix1; prefix2; center; suffix1; suffix2] in
          
          reset_order new_o;
          Printf.printf "Order modification performed. Now the order is : %s\n" (string_of_o new_o); flush stdout;
          new_o

        ) else (  (* the edge is either an auto-reference, or connects two vertices inside a loop *)
          Printf.printf "The edge is inside a component. Simply adding it to the component\n";
          Component.add_edge e c1; o
        )
        
        
        let roots o = 
          let rec step i =
            if Linked_list.has_next i then
              let c = Linked_list.next i in
              if Data_types.to_status_is_root (Component.status c) then Component_set.add c (step i)
              else Component_set.empty
            else Component_set.empty in step (Linked_list.forward_iterator o)

        let leafs o = 
          let rec step i =
            if Linked_list.has_next i then
              let c = Linked_list.next i in
              if Data_types.to_status_is_leaf (Component.status c) then Component_set.add c (step i)
              else Component_set.empty
            else Component_set.empty in step (Linked_list.backward_iterator o)

        let components o = fold (fun c res -> Component_set.add c res) o Component_set.empty
    end

    module El = struct
      type t = V of Vertice.t | L of Loop.t
      
      let is_vertice el = match el with V _ -> true | L _ -> false
      let is_loop    el = match el with V _ -> false | L _ -> true
      
      let compare el1 el2 = match (el1, el2) with
        | (V(v1), V(v2)) -> Vertice.compare v1 v2
        | (L(l1), L(l2)) -> Loop.compare l1 l2
        | (V(_) , L(_) ) -> -1
        | (L(_) , V(_) ) -> 1
    end module El_set = Set.Make(El)
   
    type t = { mutable g_vertices : Vertice_set.t; mutable g_edges : Edge_set.t; mutable g_order : Topological_order.t;
      mutable g_loops_done : bool; mutable g_loops : Loop_set.t;
      mutable g_roots : El_set.t; mutable g_leafs : El_set.t }
    
    let create () = { g_vertices = Vertice_set.empty; g_edges = Edge_set.empty; g_order = Topological_order.create ();
       g_loops_done = false; g_loops = Loop_set.empty;
       g_roots = El_set.empty; g_leafs = El_set.empty }
    
    let add_vertice data g = let v = Vertice.create data in
      g.g_vertices <- Vertice_set.add v g.g_vertices;
      g.g_roots <- El_set.add (El.V v) g.g_roots;
      g.g_leafs <- El_set.add (El.V v) g.g_leafs; Topological_order.add_vertice v g.g_order; v
      
    let add_edge o data t g = let e = Edge.create o data t in
      g.g_edges <- Edge_set.add e g.g_edges;
      g.g_loops_done <- false;
      g.g_roots <- El_set.remove (El.V t) g.g_roots;
      g.g_leafs <- El_set.remove (El.V o) g.g_leafs; g.g_order <- Topological_order.add_edge e g.g_order; e
    

    (* traversal functions *)
    let graph_parse_untag_all g = Vertice_set.iter (fun v -> Vertice.parse_untag v) g.g_vertices
    
    module Traverse_depth = struct
    
      let iter step g = (* step path v already_parsed next *)
        graph_parse_untag_all g;
        let unseen = ref g.g_vertices in
        let rec traverse p v = (* function for parsing a vertice *)
          let next () = unseen := Vertice_set.remove v !unseen; Edge_set.iter (fun e -> traverse (Path.add e p) (Edge.target e)) (Vertice.succs_e v) in
          let b = Vertice.is_parsed v in Vertice.parse_tag v; step p v b next in
        let rec main () = if not (Vertice_set.is_empty !unseen) then (let v = Vertice_set.choose !unseen in traverse (Path.create v) v; main ()) in main ()
        
    end
    
    module Traverse_topology = struct
      let generic_step step_v step_l c = match (Topological_order.Component.data c) with
        | Data_types.Vertice v -> step_v v
        | Data_types.Loop l -> step_l l

      let iter_downward step_v step_l g = Topological_order.iter_right (generic_step step_v step_l) g.g_order
      let iter_upward   step_v step_l g = Topological_order.iter_left  (generic_step step_v step_l) g.g_order
(*
    
      let generic_iter init (pred_v, next_v) (pred_l, next_l)  step_v step_l g =
        let q = ref init in (* create the queue *)
        graph_parse_untag_all g;
        let check v = let preds = (* if Vertice.is_loop v then Vertice_set.empty (* pred_l (Vertice.loop_get v) *) else*) pred_v v in
          Vertice_set.fold (fun v res -> res && (Vertice.is_parsed v)) preds true in
        let add v = if check v then ((*if Vertice.is_loop v then () (* q := El_set.add (El.L (Vertice.loop_get v)) !q *) else*) q := El_set.add (El.V v) !q) in
        while not (El_set.is_empty !q) do
          let el = El_set.choose !q in q := El_set.remove el !q; match el with
          | El.V(v) -> step_v v; Vertice.parse_tag v; Vertice_set.iter add (next_v v)
          | El.L(l) -> step_l l; Vertice_set.iter (fun v -> Vertice.parse_tag v) (Loop.vertices l); Vertice_set.iter add (next_l l)
        done
        
      let iter_downward step_v step_l g = generic_iter g.g_roots (Vertice.preds_v, Vertice.succs_v) (Loop.preds_v, Loop.succs_v) step_v step_l g
      let iter_upward   step_v step_l g = generic_iter g.g_leafs (Vertice.succs_v, Vertice.preds_v) (Loop.succs_v, Loop.preds_v) step_v step_l g *)
    end
    
    (* Access to data *)
    
    let vertices g = g.g_vertices
    let edges    g = g.g_edges
    (*let loops    g =  if g.g_loops_done then g.g_loops else (
      let res = ref Loop_set.empty in
      (* 1. compute the set of loops *)
      let step p v is_parsed next =
        if Path.mem v p then (
          let p' = List.hd (Path.split v p) in (* the path of the loop *)
          let l = if Vertice.is_loop v then (let l = Vertice.loop_get v in Loop.add p' l; l) else Loop.create p' in
          res := Loop_set.add l !res
        ) else ( if not is_parsed then next ()) in
      Traverse_depth.iter step g;
      (* 2. initialize all data in the graph *)
      g.g_loops_done <- true;
      g.g_loops <- !res;
      let filter_fun el = match el with | El.V _ -> false | El.L _ -> true in
      let (rm_roots, rm_leafs) = (El_set.filter filter_fun g.g_roots, El_set.filter filter_fun g.g_leafs) in
      g.g_roots <- El_set.diff g.g_roots rm_roots;
      g.g_leafs <- El_set.diff g.g_leafs rm_leafs;
      (* 3. add relevant loops to roots and leafs *)
      let sort_loop l =
        if Edge_set.is_empty (Loop.preds_e l) then  g.g_roots <- El_set.add (El.L l) g.g_roots
        else (if Edge_set.is_empty (Loop.succs_e l) then  g.g_leafs <- El_set.add (El.L l) g.g_leafs) in
      Loop_set.iter sort_loop !res; !res)
    
    
    let vertice_roots g = El_set.fold (fun el res -> match el with El.V v -> Vertice_set.add v res | El.L _ -> res) g.g_roots Vertice_set.empty
    let loop_roots    g = El_set.fold (fun el res -> match el with El.V _ -> res | El.L v -> Loop_set.add v res) g.g_roots Loop_set.empty
    let vertice_leafs g = El_set.fold (fun el res -> match el with El.V v -> Vertice_set.add v res | El.L _ -> res) g.g_leafs Vertice_set.empty
    let loop_leafs    g = El_set.fold (fun el res -> match el with El.V _ -> res | El.L v -> Loop_set.add v res) g.g_leafs Loop_set.empty
    *)
    
    let loops g = Topological_order.Component_set.loops    (Topological_order.components g.g_order)
    
    let vertice_roots g = Topological_order.Component_set.vertices (Topological_order.roots g.g_order)
    let loop_roots    g = Topological_order.Component_set.loops    (Topological_order.roots g.g_order)
    let vertice_leafs g = Topological_order.Component_set.vertices (Topological_order.leafs g.g_order)
    let loop_leafs    g = Topological_order.Component_set.loops    (Topological_order.leafs g.g_order)
    
    
  end
end




