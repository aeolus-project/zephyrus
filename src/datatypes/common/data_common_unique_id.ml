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
    - Data_common_set
*)

open Data_common_set

(* Modules for used tokens (names, ids, etc.) management. *)
module type Used_tokens_type = sig
  type t
  type token
  val empty : unit -> t
  val mem   : token -> t -> bool
  val add   : token -> t -> unit
end

module Used_tokens_set =
functor (Token_set : Set.S) -> struct
  type t     = Token_set.t ref
  type token = Token_set.elt
  let empty ()              = ref Token_set.empty
  let mem token used_tokens = Token_set.mem token !used_tokens
  let add token used_tokens = used_tokens := Token_set.add token !used_tokens
end

module Used_tokens_string : Used_tokens_type with type token = string = Used_tokens_set(String_set)
module Used_tokens_int    : Used_tokens_type with type token = int    = Used_tokens_set(Int_set)

(* Modules for unique identifier creation *)
module type Fresh = sig
  type t
  type id
  val create    : unit -> t
  val current   : t -> id
  val next      : t -> id
  val is_used   : t -> id -> bool
  val mark_used : t -> id -> unit
end

module Fresh_integer : Fresh with type id = int = struct

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
module type Fresh_with_requests = sig
  include Fresh
  type request (* type used to make requests for special identifiers *)
  val special : request -> id (* return an id corresponding to a special request *)
end

(* Only one special request: Deprecated *)
type request_deprecated = Deprecated

module Fresh_integer_with_deprecated = struct
  include Fresh_integer
  type request = request_deprecated
  let special = function Deprecated -> -1
end