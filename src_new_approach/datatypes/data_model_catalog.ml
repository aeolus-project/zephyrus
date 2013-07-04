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
(*    along with Zephyrus.  If not, see <http=//www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)

open Data_model


(* this module is used to get from a package name the right package, by giving in addition in which repository to look in for *)
module Repository_id_package_name = struct
  type t = repository_id * package_name
  let compare v1 v2 =
    let r1 = Repository_id.compare (fst v1) (fst v2) in if r1 = 0 then
      Package_name.compare (snd v1) (snd v2)
    else r1
end 

module Repository_id_package_name_set = Data_common.Set.Make(Repository_id_package_name)
module Repository_id_package_name_map = Data_common.Map.Make(Repository_id_package_name)



module Fresh_id = Data_common.Fresh_integer_with_deprecated

(* Catalog modules for types of all our objects. *)
(* Five-functor structure makes it quite long but we are sure it types well. *)
module Component_type_obj_catalog = Data_common.Catalog(Fresh_id)(Component_type_id_set)(Component_type_set            )(Component_type_id_map)(Component_type_map            )
module Port_obj_catalog           = Data_common.Catalog(Fresh_id)(Port_id_set          )(Port_set                      )(Port_id_map          )(Port_map                      )
module Repository_obj_catalog     = Data_common.Catalog(Fresh_id)(Repository_id_set    )(Repository_set                )(Repository_id_map    )(Repository_map                )
module Package_obj_catalog        = Data_common.Catalog(Fresh_id)(Package_id_set       )(Package_set                   )(Package_id_map       )(Package_map                   )
module Resource_obj_catalog       = Data_common.Catalog(Fresh_id)(Resource_id_set      )(Resource_set                  )(Resource_id_map      )(Resource_map                  )
module Location_obj_catalog       = Data_common.Catalog(Fresh_id)(Location_id_set      )(Location_set                  )(Location_id_map      )(Location_map                  )
module Component_obj_catalog      = Data_common.Catalog(Fresh_id)(Component_id_set     )(Component_set                 )(Component_id_map     )(Component_map                 )

module Component_type_catalog     = Data_common.Catalog(Fresh_id)(Component_type_id_set)(Component_type_name_set       )(Component_type_id_map)(Component_type_name_map       )
module Port_catalog               = Data_common.Catalog(Fresh_id)(Port_id_set          )(Port_name_set                 )(Port_id_map          )(Port_name_map                 )
module Repository_catalog         = Data_common.Catalog(Fresh_id)(Repository_id_set    )(Repository_name_set           )(Repository_id_map    )(Repository_name_map           )
module Package_catalog            = Data_common.Catalog(Fresh_id)(Package_id_set       )(Repository_id_package_name_set)(Package_id_map       )(Repository_id_package_name_map)
module Resource_catalog           = Data_common.Catalog(Fresh_id)(Resource_id_set      )(Resource_name_set             )(Resource_id_map      )(Resource_name_map             )
module Location_catalog           = Data_common.Catalog(Fresh_id)(Location_id_set      )(Location_name_set             )(Location_id_map      )(Location_name_map             )
module Component_catalog          = Data_common.Catalog(Fresh_id)(Component_id_set     )(Component_name_set            )(Component_id_map     )(Component_name_map            )


(* A meta-catalog containing catalogs of all objects from our model. *)
class type model_catalog = object
  method component_type : Component_type_catalog .catalog_iface  (* component types *)
  method port           : Port_catalog           .catalog_iface  (* ports *)
  method repository     : Repository_catalog     .catalog_iface  (* repositories *)
  method package        : Package_catalog        .catalog_iface  (* packages *)
  method resource       : Resource_catalog       .catalog_iface  (* resources *)
  method location       : Location_catalog       .catalog_iface  (* locations *)
  method component      : Component_catalog      .catalog_iface  (* components *)

  method to_string : string
end

class type closed_model_catalog = object
  method component_type : Component_type_catalog .closed_catalog_iface
  method port           : Port_catalog           .closed_catalog_iface
  method repository     : Repository_catalog     .closed_catalog_iface
  method package        : Package_catalog        .closed_catalog_iface
  method resource       : Resource_catalog       .closed_catalog_iface
  method location       : Location_catalog       .closed_catalog_iface
  method component      : Component_catalog      .closed_catalog_iface

  method to_string : string
end