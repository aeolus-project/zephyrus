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

(** Search strategies as defined in the FlatZinc specification. *)

(* Depends on:
    -Data_constraint
*)

(** How to choose the next variable to be assigned is chosen at each choice point. *)
type variable_choice_strategy =
  | Input_order      (** Choose variables in the order they appear in vars. *)
  | First_fail       (** Choose the variable with the smallest domain. *)
  | Anti_first_fail  (** Choose the variable with the largest domain. *)
  | Smallest         (** Choose the variable with the smallest value in its domain. *)
  | Largest          (** Choose the variable with the largest value in its domain. *)
  | Occurrence       (** Choose the variable with the largest number of attached constraints. *)
  | Most_constrained (** Choose the variable with the smallest domain, breaking ties using the number of constraints. *)
  | Max_regret       (** Choose the variable with the largest difference between the two smallest values in its domain. *)

(** How the chosen variable should be constrained. *)
type variable_assignment_strategy =
  | Indomain_min           (** Assign the smallest value in the variable's domain. *)
  | Indomain_max           (** Assign the largest value in the variable's domain. *)
  | Indomain_middle        (** Assign the value in the variable's domain closest to the mean of its current bounds. *)
  | Indomain_median        (** Assign the middle value in the variable's domain. *)
  | Indomain               (** Nondeterministically assign values to the variable in ascending order. *)
  | Indomain_random        (** Assign a random value from the variable's domain. *)
  | Indomain_split         (** Bisect the variable's domain, excluding the upper half first. *)
  | Indomain_reverse_split (** Bisect the variable's domain, excluding the lower half first. *)
  | Indomain_interval      (** If the variable's domain consists of several contiguous intervals, reduce the domain to the first interval. Otherwise just split the variable's domain. *)

(** The search strategy. *)
type strategy =
  | Complete (** Exhaustive search. *)

(** The parameters of single search strategy. *)
type search_parameters = {
  vars       : Data_constraint.variable list; (** List of variables to be assigned. *)
  var_choice : variable_choice_strategy;      (** Specifies how the next variable to be assigned is chosen at each choice point. *)
  assignment : variable_assignment_strategy;  (** Specifies how the chosen variable should be constrained. *)
  strategy   : strategy;                      (** Specifies the search strategy. *)
}

(** A complete search strategy. *)
type search_strategy =
  | Int_search of search_parameters
  | Seq_search of search_strategy list