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

open ExtLib

module List :
  sig
    include module type of List
    
    val flatten_map : ('a -> 'b list) -> 'a list -> 'b list
  end

(** Printing helpers. *)


(** Some operations concerning channels. *)

(** Transform a function printing something to a given channel into a function returning a string (which obvioulsly contains what would be written into that channel). *)
val string_of_printing_function : (out_channel -> 'a -> 'b) -> 'a -> string

(** Return contents of an input channel as a string. 
    Use with caution! Obviously will only work on some input channels: they have to be finite (like text files) and not too big, because their content has to fit into a single string (so not too big files). *)
val string_of_input_channel : in_channel -> string



(** Simple operations of lines of text. 
   Lines of text are represented as lists of strings: one string for each line. *)

(** Concatenate lines into a single string, simply adding a newline character between them. *)
val lines_of_strings : string list -> string

(** Add a fixed indentation in front of every line of text. *)
val indent_lines     : string list -> string list



(** Some helpers concerning execution of external commands. *)


(** Check if the process status returned by a terminated external command (i.e. returned by "Unix.system command") means that the program has finished well. *)
val did_process_exit_ok : Unix.process_status -> bool

(** Type describing an external command which can be launched with a single file as its input and which outputs to a single file. *)
type in_out_program = {
  name    : string;                     (** Name of the command (just for debug printing purposes). *)
  command : string;                     (** Exact command used to call the program on the command line. *)
  exe     : string -> string -> string; (** Function which takes the input file path, the output file path and produces a command which should be executed in 
                                            order to launch the program with the given input file and make it output its result to the given output file. 
                                            The result of this function can be directy used through the OCaml Unix standard library module (i.e. using "Unix.system"). *)
}

(** Check if the external command is available on the system. More exactly: if it is available in the standard PATH. *)
val is_program_available : string -> bool

(** Check if all of the provided in-out-programs are available on the system. Raises an exception if any of them is not available. *)
val check_if_programs_available : in_out_program list -> unit




(** Modules for converting lists into sets and maps. *)


(** Module which serves to translate a list of elements into a set. *)
module Set_of_list :
  functor (S : Set.S) ->
  sig
    (** An exception raised if a list contains two copies of the same element. *)
    exception Double_element of S.elt
    (** Takes a function transforming an element of the list into an element of the set,
        takes the list, returns a set containing all the elements of the list. *)
  	val translate : ('a -> S.elt) -> 'a list -> S.t
  end

(** Module which serves to translate an association list (i.e. list of key-value pairs) into a map. *)
module Map_of_assoc_list :
  functor (M : Map.S) ->
  sig
    (** An exception raised if a list contains two copies of the same element. *)
    exception Double_key of M.key
    (** Takes a function transforming an key of the association list (first element of a pair) into a key of the map, 
        a function transforming a value of the association list (second element of a pair) into a value of the map, 
        takes the list, returns a map corresponding to the elements of the association list. *)
  	val translate : ('a -> M.key) -> ('b -> 'c) -> ('a * 'b) list -> 'c M.t
  end

(** Module which serves to translate any list into a map. *)
module Map_of_list :
  functor (M : Map.S) ->
  sig
    (** An exception raised if a list contains two copies of the same element. *)
    exception Double_key of M.key
    (** Takes a function transforming an element of the list into a key of the map, 
        takes another function transforming the same element into a value of the map,
        takes the list, returns a map where for each original list element there is
        a binding between a key obtained by applying the first transformation function on this element
        and a value obtained by applying the second transformation function on this element. *)
  	val translate : ('a -> M.key) -> ('a -> 'b) -> 'a list -> 'b M.t
  end



(** Modules for transforming sets an converting map keys or values into a set. *)

(** Module which serves to create a set containing all values of a map. *)
module Set_of_map_values :
  functor (M : Map.S) ->
  functor (Set : Set.S) ->
  sig
    (** An exception raised if a map contains two identical values. *)
    exception Double_value of Set.elt
    (** Takes a map and returns a set containing all values of the map. *)
	  val set_of_map_values : Set.elt M.t -> Set.t
  end

(** Module which serves to create a set containing all keys of a map. *)
module Set_of_map_keys :
  functor (Map : Map.S) ->
  functor (Set : Set.S with type elt = Map.key) ->
  sig
    (** Takes a map and returns a set containing all keys of the map. *)
    val set_of_map_keys : 'a Map.t -> Set.t
  end

(** Module which serves to convert a set of one type into a set of another type. *)
module Set_of_set :
  functor (Set_origin : Set.S) ->
  functor (Set_target : Set.S) -> 
  sig
    (** Takes a function transforming an element of the origin set into an element of the targer set,
        takes an origin set and returns the targer set, which contains a transformed version of every
        element of the origin set. *)
    val convert : (Set_origin.elt -> Set_target.elt) -> Set_origin.t -> Set_target.t
  end