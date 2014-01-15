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
    - Data_common_int
    - Data_common_set
*)

open Data_common_int
open Data_common_set

module Map_from_stdlib = Map

module Map = struct

  module type OrderedType = Map_from_stdlib.OrderedType

  module type S = sig
    include Map_from_stdlib.S
    
    val of_assoc_list: (key * 'a) list -> 'a t
    val map_of_list: ('a -> key * 'b) -> 'a list -> 'b t
    val map_to_list: ( (key * 'a) -> 'b) -> 'a t -> 'b list
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
    include Map_from_stdlib.Make(Ord)
  
    let map_of_list f l = List.fold_left (fun res el -> let (k,v) = f el in add k v res) empty l
    let of_assoc_list l = List.fold_left (fun res (k,v) -> add k v res) empty l
    let map f m = fold (fun k v res -> add k (f v) res) m empty
    let map_to_list f l = List.map f (bindings l)
    
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

module Keys_of_Int_map    = Int_map   .Set_of_keys(Int_set)
module Keys_of_String_map = String_map.Set_of_keys(String_set)