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

(** Trimming operations concerning different parts of the Zephyrus model. *)

open Data_model

(** {2 Trim the universe.} 

Simplify the universe by stripping it of all
the information which is not necessary in context of the given configuration
and specification. This is done in order to reduce the size and complexity 
of constraints generated from this universe. 
*)

(** {e Trimming component types}: we need only a transitive closure of the component types mentioned in the configuration and specification. *)
val trim_component_types : universe -> configuration -> specification -> universe

(** Trimming repositories consists of two steps: {ol
	{- {e Trimming the component implementation information}: 

	   we only need the information
	   about implementation of these component types which are still available after
	   the component type trimming.}
	{- {e Trimming the package repositories}: 

	   as package information is important only if
	   package conflicts can potentially cause conflicts between components, we can
	   remove all these packages which cannot cause conflicts in the context of our
	   available (i.e. available after trimming) component types.} }
*)
val trim_repositories    : universe -> configuration -> specification -> universe

(** {2 Trim the configuration.} *)

val transitive_closure_domain : configuration -> Location_id_set.t -> Location_id_set.t
val configuration : configuration -> Location_id_set.t -> configuration * configuration

(** [empty c] returns the configuration [c], but without any packages or components. *)
val empty : configuration -> configuration 

