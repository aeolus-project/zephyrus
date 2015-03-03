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

(** Catalogs for all the id <-> object and id <-> name two-way mappings which can be found in the Zephyrus model. *)

(* Depends on
     - Data_model
     - Abstract_io
     - Data_common.Catalog
     - Data_common.Fresh_integer_with_deprecated
     - Data_common.Set
     - Data_common.Map
 *)


(* this module is used to get from a package name the right package, by giving in addition in which repository to look in for *)
module Repository_id_package_name = struct
  type t = Data_model.repository_id * Abstract_io.package_name
  let compare = Pervasives.compare
end 

module Repository_id_package_name_set = Data_common.Set.Make(Repository_id_package_name)
module Repository_id_package_name_map = Data_common.Map.Make(Repository_id_package_name)

module Component_type_subs = struct
  type t = Data_model.component_type_id list
  let compare = Pervasives.compare
end

module Component_type_subs_set = Data_common.Set.Make(Component_type_subs)
module Component_type_subs_map = Data_common.Map.Make(Component_type_subs)

module Fresh_id = Data_common.Fresh_integer_with_deprecated

module Fresh_string_dummy = struct
  type t = int
  type id = string
  let create    () = 0
  let current   n  = ""
  let next      n  = ""
  let is_used   n s = false
  let mark_used n s = ()
end


(*/************************************************************************\*)
(*| 1. Mapping Name <-> ID                                                 |*)
(*\************************************************************************/*)

module Component_type_catalog     = Data_common.Catalog(Fresh_id)
                                      (Data_model.Component_type_id_set)(Abstract_io.Component_type_ref_set)
                                      (Data_model.Component_type_id_map)(Abstract_io.Component_type_ref_map)
module Component_type_subs_catalog= Data_common.Catalog(Fresh_string_dummy)
                                      (Abstract_io.Component_type_name_set)(Component_type_subs_set)
                                      (Abstract_io.Component_type_name_map)(Component_type_subs_map)
module Port_catalog               = Data_common.Catalog(Fresh_id)
                                      (Data_model.Port_id_set          )(Abstract_io.Port_name_set         )
                                      (Data_model.Port_id_map          )(Abstract_io.Port_name_map         )
module Repository_catalog         = Data_common.Catalog(Fresh_id)
                                      (Data_model.Repository_id_set    )(Abstract_io.Repository_name_set   )
                                      (Data_model.Repository_id_map    )(Abstract_io.Repository_name_map   )
module Package_catalog            = Data_common.Catalog(Fresh_id)
                                      (Data_model.Package_id_set       )(Repository_id_package_name_set    )
                                      (Data_model.Package_id_map       )(Repository_id_package_name_map    )
module Resource_catalog           = Data_common.Catalog(Fresh_id)
                                      (Data_model.Resource_id_set      )(Abstract_io.Resource_name_set     )
                                      (Data_model.Resource_id_map      )(Abstract_io.Resource_name_map     )
module Location_catalog           = Data_common.Catalog(Fresh_id)
                                      (Data_model.Location_id_set      )(Abstract_io.Location_name_set     )
                                      (Data_model.Location_id_map      )(Abstract_io.Location_name_map     )
module Component_catalog          = Data_common.Catalog(Fresh_id)
                                      (Data_model.Component_id_set     )(Abstract_io.Component_name_set    )
                                      (Data_model.Component_id_map     )(Abstract_io.Component_name_map    )



(*/************************************************************************\*)
(*| 2. Mapping ID <-> Structure                                            |*)
(*\************************************************************************/*)
open Data_model

module Component_type_obj_catalog = Data_common.Catalog(Fresh_id)
                                      (Component_type_id_set)(Component_type_set            )
                                      (Component_type_id_map)(Component_type_map            )
module Port_obj_catalog           = Data_common.Catalog(Fresh_id)
                                      (Port_id_set          )(Port_set                      )
                                      (Port_id_map          )(Port_map                      )
module Repository_obj_catalog     = Data_common.Catalog(Fresh_id)
                                      (Repository_id_set    )(Repository_set                )
                                      (Repository_id_map    )(Repository_map                )
module Package_obj_catalog        = Data_common.Catalog(Fresh_id)
                                      (Package_id_set       )(Package_set                   )
                                      (Package_id_map       )(Package_map                   )
module Resource_obj_catalog       = Data_common.Catalog(Fresh_id)
                                      (Resource_id_set      )(Resource_set                  )
                                      (Resource_id_map      )(Resource_map                  )
module Location_obj_catalog       = Data_common.Catalog(Fresh_id)
                                      (Location_id_set      )(Location_set                  )
                                      (Location_id_map      )(Location_map                  )
module Component_obj_catalog      = Data_common.Catalog(Fresh_id)
                                      (Component_id_set     )(Component_set                 )
                                      (Component_id_map     )(Component_map                 )



(*/************************************************************************\*)
(*| 3. Class with all the necessary mappings                               |*)
(*\************************************************************************/*)

class model_catalog 
  ~component_type_catalog
  ~component_type_subs_catalog
  ~port_catalog
  ~repository_catalog
  ~package_catalog
  ~resource_catalog
  ~location_catalog
  ~component_catalog
  = object
  
  val    component_type_catalog      : Component_type_catalog      .catalog_iface = component_type_catalog
  val    component_type_subs_catalog : Component_type_subs_catalog .catalog_iface = component_type_subs_catalog
  val    port_catalog                : Port_catalog                .catalog_iface = port_catalog
  val    repository_catalog          : Repository_catalog          .catalog_iface = repository_catalog
  val    package_catalog             : Package_catalog             .catalog_iface = package_catalog
  val    resource_catalog            : Resource_catalog            .catalog_iface = resource_catalog
  val    location_catalog            : Location_catalog            .catalog_iface = location_catalog
  val    component_catalog           : Component_catalog           .catalog_iface = component_catalog

  method component_type              : Component_type_catalog      .catalog_iface = component_type_catalog
  method component_type_subs         : Component_type_subs_catalog .catalog_iface = component_type_subs_catalog
  method port                        : Port_catalog                .catalog_iface = port_catalog
  method repository                  : Repository_catalog          .catalog_iface = repository_catalog
  method package                     : Package_catalog             .catalog_iface = package_catalog
  method resource                    : Resource_catalog            .catalog_iface = resource_catalog
  method location                    : Location_catalog            .catalog_iface = location_catalog
  method component                   : Component_catalog           .catalog_iface = component_catalog
end

class closed_model_catalog 
  ~component_type_catalog
  ~component_type_subs_catalog
  ~port_catalog
  ~repository_catalog
  ~package_catalog
  ~resource_catalog
  ~location_catalog
  ~component_catalog
  = object
  
  val    component_type_catalog : Component_type_catalog .closed_catalog_iface = component_type_catalog
  val    component_type_subs_catalog : Component_type_subs_catalog .closed_catalog_iface = component_type_subs_catalog
  val    port_catalog           : Port_catalog           .closed_catalog_iface = port_catalog
  val    repository_catalog     : Repository_catalog     .closed_catalog_iface = repository_catalog
  val    package_catalog        : Package_catalog        .closed_catalog_iface = package_catalog
  val    resource_catalog       : Resource_catalog       .closed_catalog_iface = resource_catalog
  val    location_catalog       : Location_catalog       .closed_catalog_iface = location_catalog
  val    component_catalog      : Component_catalog      .closed_catalog_iface = component_catalog

  method component_type         : Component_type_catalog .closed_catalog_iface = component_type_catalog
  method component_type_subs         : Component_type_subs_catalog .closed_catalog_iface = component_type_subs_catalog
  method port                   : Port_catalog           .closed_catalog_iface = port_catalog
  method repository             : Repository_catalog     .closed_catalog_iface = repository_catalog
  method package                : Package_catalog        .closed_catalog_iface = package_catalog
  method resource               : Resource_catalog       .closed_catalog_iface = resource_catalog
  method location               : Location_catalog       .closed_catalog_iface = location_catalog
  method component              : Component_catalog      .closed_catalog_iface = component_catalog

end

let close_model_catalog (model_catalog : model_catalog) : closed_model_catalog =
  new closed_model_catalog
    ~component_type_catalog: (Component_type_catalog.close_catalog model_catalog#component_type)
    ~component_type_subs_catalog: (Component_type_subs_catalog.close_catalog model_catalog#component_type_subs)
    ~port_catalog:           (Port_catalog          .close_catalog model_catalog#port)
    ~repository_catalog:     (Repository_catalog    .close_catalog model_catalog#repository)
    ~package_catalog:        (Package_catalog       .close_catalog model_catalog#package)
    ~resource_catalog:       (Resource_catalog      .close_catalog model_catalog#resource)
    ~location_catalog:       (Location_catalog      .close_catalog model_catalog#location)
    ~component_catalog:      (Component_catalog     .close_catalog model_catalog#component)





