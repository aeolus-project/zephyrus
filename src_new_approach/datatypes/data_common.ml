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

module Map_global_from_stdlib = Map
module Set_global_from_stdlib = Set

module type OrderedType = Map_global_from_stdlib.OrderedType
module type Map_from_stblib = Map_global_from_stdlib.S
module type Set_from_stblib = Set_global_from_stdlib.S

module String = String
module Int = struct type t = int let compare = (-) end

module Set = struct

  module type S = sig
    include Set_from_stblib
    
    val set_of_direct_list: elt list -> t
    val set_of_list: ('a -> elt) -> 'a list -> t
  end
  
  module Make(Ord : OrderedType) : S with type elt = Ord.t = struct
    module Set_tmp = Set_global_from_stdlib.Make(Ord)
    include Set_tmp
    
    let set_of_direct_list l = List.fold_left (fun res v -> add v res) empty l
    let set_of_list f l = List.fold_left (fun res v -> add (f v) res) empty l
  end

  module Convert(Set_origin : S) (Set_target : S) = struct
    let convert f s = Set_origin.fold (fun v res -> Set_target.add (f v) res) s Set_target.empty
  end

  module EquivalenceClass(Set_origin : S)(Set_target : S with type elt = Set_origin.t) = struct
    let compute f s =
      let res = ref Set_target.empty in
      Set_origin.iter (fun e -> let b = Set_target.fold (fun s b -> 
          if b && (f (Set_origin.choose s) e) then (res := Set_target.add (Set_origin.add e s) (Set_target.remove s !res); false) else true) !res true in
        (if b then res := Set_target.add (Set_origin.singleton e) !res)
      ) s; !res
  end
end

module SetInt       = Set.Make(Int)
module SetSetInt    = Set.Make(SetInt)
module SetString    = Set.Make(String)
module SetSetString = Set.Make(SetString)
module SetInt_to_SetString = Set.Convert(SetInt)(SetString)
let setstring_of_setint s = SetInt_to_SetString.convert string_of_int s

module Map = struct

  module type S = sig
    include Map_from_stblib
    
    val map_of_associated_list: (key * 'a) list -> 'a t
    val map_of_list: ('a -> key * 'b) -> 'a list -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t

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
    let map_of_associated_list l = List.fold_left (fun res (k,v) -> add k v res) empty l
    let map f m = fold (fun k v res -> add k (f v) res) m empty    

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

module MapInt = Map.Make(Int)
module MapString = Map.Make(String)

module Keys_of_MapInt    = MapInt.Set_of_keys(SetInt)
module Keys_of_MapString = MapString.Set_of_keys(SetString)

