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
    - Set
    - Data_common_int
*)

open Data_common_int

(** Extension of the Set module from the standard library with Construction and Conversion **)

module Set_from_stdlib = Set

module Set = struct

  module type OrderedType = Set_from_stdlib.OrderedType

  module type S = sig
    include Set_from_stdlib.S
    val of_list_directly : elt list -> t
    val of_list : ('a -> elt) -> 'a list -> t
    val map_to_list : (elt -> 'a) -> t -> 'a list
    val filter_map_to_list : (elt -> 'a option) -> t -> 'a list
    val keep_elements : int -> t -> t
    val keep_best_elements : int -> (elt -> elt -> int) -> t -> t
  end
  
  module Make(Ord : OrderedType) : S with type elt = Ord.t = struct
    include Set_from_stdlib.Make(Ord)
    let of_list_directly     l   = List.fold_left (fun res v -> add v res) empty l
    let of_list            f l   = List.fold_left (fun res v -> add (f v) res) empty l
    let map_to_list        f s   = fold (fun el res -> (f el)::res) s []
    let filter_map_to_list f s   = fold (fun el res -> let v_opt = f el in match v_opt with Some(v) -> v::res | None -> res) s []
    let keep_elements      n s   = if n <= 0 then empty else (let rec f n s = if n <= 0 then s else f (n - 1) (remove (choose s) s) in f ((cardinal s) - n) s)
    let keep_best_elements n c s = if n <= 0 then empty else if cardinal s <= n then s else (
      let l = List.sort (fun x y -> -(c x y)) (elements s) in
      let rec f n l = if n <= 0 then [] else match l with | e::l' -> e::(f (n-1) l') | [] -> [] in
      of_list_directly (f n l))
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
