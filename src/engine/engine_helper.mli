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

(** How the program should be run? *)
type program_exe_type =
  | Bash_command (** Program should be run directly as a bash command. *)

(** Type describing an external command which can be launched with a single file as its input and which outputs to a single file. *)
type program = {
  name     : string;                (** Name of the command (just for debug printing purposes). *)
  commands : string list;           (** Exact command used to call the program on the command line. *)
  exe      : string list -> string; (** Function which takes the different parameters for the program, and generate the command line to execute *)
  exe_type : program_exe_type;      (** How the program command should be run? *)
}

exception Wrong_argument_number

type pid

(** [string_of_process_status status] returns a printable string representation of a given process status. *)
val string_of_process_status : Unix.process_status -> string

(** [program_is_available p] checks if the external commands required to run a given program [p] are available in the standard PATH. *)
val program_is_available : program -> bool

(** [programs_are_available ps] checks if the external commands required to run all the given programs from the list [ps] are available in the standard PATH. *)
val programs_are_available : program list -> bool

(** [program_sync_exec p args] executes a given program [p] with arguments [args] synchronically and returns its termination status. *)
val program_sync_exec : program -> (string list) -> Unix.process_status

(** [program_async_exec p args] executes a given program [p] with arguments [args] asynchronically and returns its pid. *)
val program_async_exec : program -> (string list) -> pid

(** [process_wait_pid pid] waits for a process with the given pid to terminate, then it returns its termination status. *)
val process_wait_pid : pid -> Unix.process_status

(** [process_wait ()] waits for any child of the current process terminate, then it returns its pid and termination status. *)
val process_wait : unit -> (pid * Unix.process_status)

(** [did_program_exit_ok process_status] check if the process status returned by a terminated external command (i.e. returned by {!program_sync_exec}) means that the program has terminated successfuly. *)
val did_program_exit_ok : Unix.process_status -> bool

(** [kill pid] sends a KILL signal to a process with the given [pid] (if the process does not exist, nothing happens). *)
val kill : int -> unit

(** [make_zephyrus_temp_file ext] returns a fresh Zephyrus temporary file path with a given extension [ext] (e.g. [make_zephyrus_temp_file ".mzn"] will return something like [/tmp/zephyrus-cbe43b.mzn]). *)
val make_zephyrus_temp_file : string -> string


(** Some useful programs (usually, the [program.exe] only takes two arguments). *)

val coinst                  : program
val mzn2fzn                 : program
val g12_flatzinc_solver     : program
val gecode_flatzinc_solver  : program
val g12_minizinc_solver     : program
val g12_cpx_minizinc_solver : program
val gecode_minizinc_solver  : program


(** 2. Custom solver handling *)

(** Substrings in the custom solver command string which should be replaced by the input/output file path *)

val input_marker  : string
val output_marker : string

val make_minizinc_solver_of_custom_flatzinc_solver_command : string -> program
val make_minizinc_solver_of_custom_minizinc_solver_command : string -> program

(** 3. File name manipulation *)

type file = {
  dirname  : string;
  basename : string;
  suffix : string
}

val file_default : file

val file_process_name : string -> file

(** [file_create keep file] returns a name of a fresh temporary file, constructed using the [file] structure. *)
val file_create       : bool -> file -> string

(** [file_print keep file content] creates a fresh temporary file, constructed using the [file] structure, and fills it with given [content]. *)
val file_print        : bool -> file -> string -> string


(** 4. The portfolio method. *)

(** Portfolio method of running worker-processes (run many processes trying to find a solution in parallel, get the first correct solution, kill remaining processes). *)

(** [portfolio ps validate_output_file input_filename output_filename] *)
val portfolio : program list -> (string -> bool) -> string -> string -> bool

val portfolio_test : unit -> unit
