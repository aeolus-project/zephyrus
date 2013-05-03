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


val string_of_printing_function : (out_channel -> 'a -> 'b) -> 'a -> string

val lines_of_strings        : string list -> string
val indent_lines            : string list -> string list
val indent_lines_of_strings : string list -> string

val string_of_input_channel : in_channel -> string

val did_process_exit_ok : Unix.process_status -> bool

type in_out_program = {
  name    : string;
  command : string;
  exe     : string -> string -> string;
}

val is_program_available : string -> bool

val check_if_programs_available : in_out_program list -> unit

module SetOfList :
  functor (S : Set.S) ->
  sig
    exception DoubleElement of S.elt
  	val translate : ('a -> S.elt) -> 'a list -> S.t
  end

module MapOfAssocList :
  functor (M : Map.S) ->
  sig
    exception DoubleKey of M.key
  	val translate : ('a -> M.key) -> ('b -> 'c) -> ('a * 'b) list -> 'c M.t
  end

module MapOfList :
  functor (M : Map.S) ->
  sig
    exception DoubleKey of M.key
  	val translate : ('a -> M.key) -> ('a -> 'b) -> 'a list -> 'b M.t
  end