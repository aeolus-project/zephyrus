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


open Model_t

(** Trim the universe: simplify the universe by stripping it the universe of all
    the information which is not necessary in context of the given configuration
    and specification. This is done in order to reduce the size and complexity 
    of constraints generated from this universe. 

	Trimming involves three steps:

	1. Trimming component types: we need only a transitive closure of the component
	   types mentioned in the configuration and specification.

	2. Trimming the component implementation information: we only need the information
	   about implementation of these component types which are still available after
	   the component type trimming.

	2. Trimming the package repositories: as package information is important only if
	   package conflicts can potentially cause conflicts between components, we can
	   remove all these packages which cannot cause conflicts in the context of our
	   available (i.e. available after trimming) component types.
*)
val trim : universe -> configuration -> specification -> universe
 