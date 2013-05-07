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

open Facile
open Easy

open Variables
open Solution


(* Types *)
type native_facile_variable = Facile.Var.Fd.t
type facile_variables       = (variable * native_facile_variable) list

(* Accessing *)
val get_native_facile_variable  : facile_variables -> variable -> native_facile_variable
val get_native_facile_variables : facile_variables -> native_facile_variable list

(* Creating *)
val create_facile_variables : variable list -> facile_variables

(* Printing *)
val string_of_facile_variable  : native_facile_variable  -> string
val string_of_facile_variables : facile_variables        -> string

(* Extracting the solution from variables *)
val solution_of_facile_variables : facile_variables -> solution