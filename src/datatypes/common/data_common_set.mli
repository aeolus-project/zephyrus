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

(** Extention of the Set module from the standard library with construction and conversion. *)

(* Depends on
    - Set
    - Data_common_int
*)

open Data_common_int

module Set_from_stdlib : module type of Set

module Set : sig

  module type OrderedType = Set_from_stdlib.OrderedType

  module type S = sig
    include Set_from_stdlib.S
    
    (** TODO: What is this function doing? *)
    val keep_elements : int -> t -> t
    
    (** Convert a list directly to a set. *)
    val set_of_direct_list: elt list -> t

    (** Convert a list to a set, applying a given function to each element. *)
    val set_of_list: ('a -> elt) -> 'a list -> t

    (** Create a list of all the elements of a set, while applying a given function to each of them. *)
    val map_to_list: (elt -> 'a) -> t -> 'a list
  end
  
  module Make(Ord : OrderedType) : S with type elt = Ord.t

  (** A functor creating a module used to convert one type of sets to another. *)
  module Convert(Set_origin : S) (Set_target : S) : sig
    val convert : (Set_origin.elt -> Set_target.elt) -> Set_origin.t -> Set_target.t
    val filter_convert : (Set_origin.elt -> Set_target.elt option) -> Set_origin.t -> Set_target.t
    val set_convert : (Set_origin.elt -> Set_target.t) -> Set_origin.t -> Set_target.t
  end

  module EquivalenceClass(Set_origin : S)(Set_target : S with type elt = Set_origin.t) : sig
    val compute : (Set_origin.elt -> Set_origin.elt -> bool) -> Set_origin.t -> Set_target.t
  end

end

module Int_set        : Set.S with type elt = int
module Int_set_set    : Set.S with type elt = Int_set.t
module String_set     : Set.S with type elt = string
module String_set_set : Set.S with type elt = String_set.t