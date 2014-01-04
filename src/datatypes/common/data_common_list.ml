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
    - List
*)

module List_from_stdlib = List

module List = struct
  include List_from_stdlib

  let filter_map (f : 'a -> 'b option) (l : 'a list) : ('b list) = 
    List.fold_right (fun (el : 'a) (l : 'b list) -> 
      match f el with
      | None   -> l 
      | Some x -> x :: l
    ) l []

  let rec indices l current_index =
    match l with
    | []   -> []
    | _::t -> current_index :: (indices t (current_index + 1))

  let mapi : (int -> 'a -> 'b) -> 'a list -> 'b list = 
    fun f l -> List.map2 f (indices l 0) l  

  let is_empty l = (l = [])

  let rec fold_combine conv combine l init = 
    match l with
    | [] -> init
    | [el] -> conv el
    | el::l' -> combine (conv el) (fold_combine conv combine l' init)

  let rec cartesian_product l =
    match l with
    | []     -> []
    | h::[]  -> map (fun el -> [el]) h
    | h::t   -> flatten (
                  map (fun el ->
                    map (fun res -> el::res) (cartesian_product t)
                  ) h)

end