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


open Facile_variables

module Facile_constraints :
  sig

  	type const = int
  	type var   = Facile.Var.Fd.t
    type cstr  = Facile.Cstr.t
    type expr  = Facile.Arith.t

    
    (* Printing *)

	val string_of_var  : var  -> string
	val string_of_expr : expr -> string
	val string_of_cstr : cstr -> string


	(* Building expressions *)

	val var2expr   : var   -> expr
	val const2expr : const -> expr

	(* Building constraints *)

	val truecstr  : cstr
	val falsecstr : cstr


	(* Reification *)

	val reify : cstr -> expr

	(* Arithmetic operators *)

	val ( +~ ) : expr -> expr -> expr
	val ( -~ ) : expr -> expr -> expr
	val ( *~ ) : expr -> expr -> expr
	val ( /~ ) : expr -> expr -> expr
	val ( %~ ) : expr -> expr -> expr
	val abs    : expr -> expr

	val sum : expr list -> expr


	(* Reified arithmetic comparisons *)

	val (  <~~ ) : expr -> expr -> expr
	val ( <=~~ ) : expr -> expr -> expr
	val (  =~~ ) : expr -> expr -> expr
	val ( >=~~ ) : expr -> expr -> expr
	val (  >~~ ) : expr -> expr -> expr
	val ( <>~~ ) : expr -> expr -> expr


	(* Arithmetic comparisons *)

	val (  <~ ) : expr -> expr -> cstr
	val ( <=~ ) : expr -> expr -> cstr
	val (  =~ ) : expr -> expr -> cstr
	val ( >=~ ) : expr -> expr -> cstr
	val (  >~ ) : expr -> expr -> cstr
	val ( <>~ ) : expr -> expr -> cstr


	(* Constraint operators *)

	val (  &&~~ ) : cstr -> cstr -> cstr    
	val (  ||~~ ) : cstr -> cstr -> cstr    
	val (  =>~~ ) : cstr -> cstr -> cstr    
	val ( <=>~~ ) : cstr -> cstr -> cstr    
	val xor       : cstr -> cstr -> cstr    
	val not       : cstr -> cstr


	(* Post constraints *)

    val post : cstr -> unit


    (* Translation from the generic constraints *)

    val translate_var  : facile_variables -> Generic_constraints.var  -> var
	val translate_expr : facile_variables -> Generic_constraints.expr -> expr
	val translate_cstr : facile_variables -> Generic_constraints.cstr -> cstr

  end


type generated_constraints = (string * (Facile_constraints.cstr list)) list

val post_constraints : Facile_constraints.cstr list -> unit

val post_translation_constraints : generated_constraints -> unit

val string_of_constraint : Facile_constraints.cstr -> string

val string_of_constraints : generated_constraints -> string

val create_optimized_goal : facile_variables -> Facile_constraints.expr -> (Solution.solution ref) -> bool -> Facile.Goals.t
