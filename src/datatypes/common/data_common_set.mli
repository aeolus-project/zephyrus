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

(** Custom extension of the [Set] module from the standard library with construction and conversion. *)

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
    
    (** [of_list_directly l] converts the list [l] directly into a set. *)
    val of_list_directly : elt list -> t

    (** [of_list f l] converts the list [l] to a set, applying the function [f] to each element of the list to get a set element. *)
    val of_list : ('a -> elt) -> 'a list -> t

    (** [map_to_list f s] converts the set [s] to a list, applying the function [f] to each element from the set to get a list element. *)
    val map_to_list : (elt -> 'a) -> t -> 'a list
    
    (** [filter_map_to_list f s] converts the set [s] to a list, applying the function [f] to each element from the set. If for a given element [el] function [f el] returns [Some x], then the value [x] is included in the returned list. If [f el] returns [None], then the element is discarded. *)
    val filter_map_to_list: (elt -> 'a option) -> t -> 'a list
    
    (** [keep_elements n s] extract one subset [s'] of [s] such that that the cardinality of [s'] is equal to [\min(n,card(s))] *)
    val keep_elements : int -> t -> t
    (** [keep_best_elements n c s] extract one subset [s'] of [s] such that that the cardinality of [s'] is equal to [min(n,card(s))] and that for all elements [e\in s'] and [e'\in s\setiminus s'] we have [e'\leq e] w.r.t. the comparison function [c]*)
    val keep_best_elements : int -> (elt -> elt -> int) -> t -> t

  end
  
  module Make(Ord : OrderedType) : S with type elt = Ord.t

  (** A functor creating a module used to convert one type of sets to another type of sets. *)
  module Convert(Set_origin : S) (Set_target : S) : sig
    (** [convert f s] applies the function [f] to all elements from the set [s] and builds a new set where each element [el] from the set [s] is replaced with [f el]. Functional "map" operation on sets of two different types. *)
    val convert : (Set_origin.elt -> Set_target.elt) -> Set_origin.t -> Set_target.t
    (** [convert f s] applies the function [f] to all elements from the set [s] and builds a new set where: for each element [el] from the set [s] if [f el] returns [Some x] then [x] is included in the new set, if [f el] returns [None], then the the element is discarded. Functional "filter_map" operation on sets of two different types. *)
    val filter_convert : (Set_origin.elt -> Set_target.elt option) -> Set_origin.t -> Set_target.t
    (** TODO: What is this function doing? *)
    val set_convert : (Set_origin.elt -> Set_target.t) -> Set_origin.t -> Set_target.t
  end

  (** TODO *)
  module EquivalenceClass(Set_origin : S)(Set_target : S with type elt = Set_origin.t) : sig
    val compute : (Set_origin.elt -> Set_origin.elt -> bool) -> Set_origin.t -> Set_target.t
  end

end

(** A set with integers for elements. *)
module Int_set        : Set.S with type elt = int
(** A set with sets of integers for elements. *)
module Int_set_set    : Set.S with type elt = Int_set.t
(** A set with strings for elements. *)
module String_set     : Set.S with type elt = string
(** A set with sets of strings for elements. *)
module String_set_set : Set.S with type elt = String_set.t
