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

open Aeolus_types_t
open Generic_constraints


(** Generated constraints type. *)

(** Generated constraints type is simply a list of constraints arranged in groups, 
    which reflect where do a certain subset of constraints come from. *)
type generated_constraints = (string * (cstr list)) list
(* TODO: We should have some kind of a more abstract type here. *)

(** Priniting. *)
val string_of_generated_constraints : generated_constraints -> string

(** Extract a list of constraints from the "generated constraints". *)
val constraints_of_generated_constraints : generated_constraints -> cstr list


(** Translation *)

(** Takes a universe and an initial configuration, translates them into a constraint problem. *)
val translate_universe_and_initial_configuration : universe -> configuration -> generated_constraints

(** Takes a specification and an initial configuration, translates in into constraints. *)
val translate_specification : specification -> configuration -> generated_constraints
