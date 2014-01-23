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

(** Helpers for operations on files and executing external programs. *)

(* Depends on
    - Unix
    - Filename
    - String
*)

(** 1. helpers on external commands. *)

(** Type describing an external command which can be launched with a single file as its input and which outputs to a single file. *)
type program = {
  name     : string;                     (** Name of the command (just for debug printing purposes). *)
  commands : string list;                (** Exact command used to call the program on the command line. *)
  exe      : string list -> string;      (** Function which takes the different parameters for the program, and generate the command line to execute *)
}

exception Wrong_argument_number

type pid

(** Check if the external command is available in the standard PATH. *)
val program_is_available : program -> bool
val programs_are_available : program list -> bool

(** executing a program *)
val program_sync_exec  : program -> (string list) -> Unix.process_status
val program_async_exec : program -> (string list) -> pid
val program_wait       : pid -> Unix.process_status

(** Check if the process status returned by a terminated external command (i.e. returned by {!program_sync_exec}) means that the program has finished well. *)
val program_did_exit_ok : Unix.process_status -> bool


(** Some useful programs (usually, there [exe] only takes two arguments *)

val mzn2fzn                : program
val g12_flatzinc_solver    : program
val gecode_flatzinc_solver : program
val g12_minizinc_solver    : program
val gecode_minizinc_solver : program


(** 2. Custom solver handling *)

val input_marker  : string
val output_marker : string

val make_minizinc_solver_of_custom_flatzinc_solver_command : string -> program
val make_minizinc_solver_of_custom_minizinc_solver_command : string -> program

(** 3. File name manipulation *)

type file = {dirname : string; basename : string; suffix : string}
val file_default : file

val file_process_name : string -> file
val file_create       : bool -> file -> string
val file_print        : bool -> file -> string -> string

(*
val file_create : ?keep:bool -> file -> string
val file_print : ?keep:bool -> file -> string -> string
*)
