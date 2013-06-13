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

(* Depends on
    - datatypes/Data_constraint
    - datatypes/Data_model
    - datatypes/Data_functions
*)

module Plain : sig
  val universe : Data_model.Location_name_set.t -> Data_model.Resource_name_set.t -> Data_model.universe -> Data_constraint.t
  val configuration : Data_model.Resource_name_set.t -> Data_model.configuration -> Data_constraint.t
  val specification : Data_model.specification -> Data_constraint.t
  val optimization_function :

  val universe_flat : Data_model.universe -> Data_constraint.t
end

module Improved : sig

end
