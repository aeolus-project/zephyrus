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
    - Data_common_set
    - Data_common_map
*)

open Data_common_set
open Data_common_map


(* Inductive construction of loops inspired from the first pages of "Incremental Cycle Detection, Topological Ordering, and Strong Component Maintenance" *)

module Graph : sig
  module type Vertice_data = sig type t end
  module type Edge_data   = sig type t end

  module type S = sig
    type vertice_data
    type edge_data
    
    module rec Vertice : sig
      type t                           (* type of vertices *)
      val data   : t -> vertice_data   (* returns the data contained in that vertice *)
      val succs_e: t -> Edge_set.t     (* returns the set of edges exiting from that vertice *)
      val preds_e: t -> Edge_set.t     (* returns the set of edges targeting that vertice *)
      val succs_v: t -> Vertice_set.t  (* returns the set of vertices that have an edge from that vertice *)
      val preds_v: t -> Vertice_set.t  (* returns the set of vertices that have an edge to that vertice *)

(*      
      (* Functions used internally. Do not use. TODO: clean this by providing two signatures in the implementation, one internal, one for the .mli *)
      val parse_tag   : t -> unit
      val is_parsed   : t -> bool
      val parse_untag : t -> unit
      val loop_tag    : Loop.t -> t -> unit
      val loop_untag  : t -> unit

      val is_loop     : t -> bool      (* returns if the vertice is part of a loop *)
      val loop_get    : t -> Loop.t    (* returns the loop in which the vertice is, or raise Not_found *)
*)      
      val compare : t -> t -> int      (* classic comparison function *)
      val equal   : t -> t -> bool     (* comparison that returns a bool instead of an int *)
    end and Vertice_set : (Set.S with type elt = Vertice.t) and Vertice_map : (Map.S with type key = Vertice.t)
    
    and Edge : sig
      type t                           (* type of edges *)
      val data   : t -> edge_data      (* returns the data contained in that edge *)
      val origin : t -> Vertice.t      (* returns the origin vertice of that edge *)
      val target : t -> Vertice.t      (* returns the target vertice of that edge *)
(*      
      (* Functions used internally. Do not use. TODO: clean this by providing two signatures in the implementation, one internal, one for the .mli *)
      val loop_tag     : Loop.t -> t -> unit
      val loop_tag_in  : Loop.t -> t -> unit
      val loop_tag_out : Loop.t -> t -> unit
      val loop_untag   : t -> unit
*)      
      val is_loop      : t -> bool     (* returns if that edge is part of a loop *)
      val is_loop_in   : t -> bool     (* returns if that edge targets a vertice in a loop *)
      val is_loop_out  : t -> bool     (* returns if that edge originates from a vertice in a loop *)
(*      val loop_get     : t -> Loop.t   (* returns the loop linked to that edge, or raise Not_found *) *)
      
      val compare : t -> t -> int      (* classic comparison function *)
      val equal   : t -> t -> bool     (* comparison that returns a bool instead of an int *)
    end and Edge_set : (Set.S with type elt = Edge.t) and Edge_map : (Map.S with type key = Edge.t)

    and Path : sig
      type t                                      (* type of path *)
      exception Invalid_path_extension            (* exception raised when erroneous operation is done on a path *)

      val create      : Vertice.t -> t            (* create an empty path starting on that vertice *)
      val is_empty    : t -> bool                 (* returns if the path is empty *)
      val add         : Edge.t -> t -> t          (* returns that path extended with that edge, or raise Invalid_path_extension *)
      val mem         : Vertice.t -> t -> bool    (* returns if that vertice is in that path *)
      val vertices    : t -> Vertice_set.t        (* returns the set of vertices in that path *)
      val edges       : t -> Edge_set.t           (* returns the set of edges in that path *)
      val split       : Vertice.t -> t -> t list  (* [split v p] returns the list [p1;...;pn] with [p = pn -> v -> ... -> v -> p1. Does not create empty path in case origin or target is [v] *)
      val concat      : t -> t -> t               (* concat the two paths together, or raise Invalid_path_extension  *)
      val concat_list : t list -> t               (* concat a non-empty list of paths *)
      val is_loop     : t -> bool                 (* tests if that path is a loop *)
      
      val compare : t -> t -> int                 (* classic comparison function *)
      val equal   : t -> t -> bool                (* comparison that returns a bool instead of an int *)
    end and Path_set : (Set.S with type elt = Path.t) and Path_map : (Map.S with type key = Path.t)

    and Loop : sig
      type t                                     (* type of loop *)
      
      val vertices : t -> Vertice_set.t          (* returns the set of vertices that are part of that loop *)
      val edges    : t -> Edge_set.t             (* returns the set of edges that are part of that loop *)
      val succs_e  : t -> Edge_set.t             (* returns the set of edges whose origin is part of the loop (and which are not part of the loop) *)
      val preds_e  : t -> Edge_set.t             (* returns the set of edges whose target is part of the loop (and which are not part of the loop) *)
      val succs_v  : t -> Vertice_set.t          (* returns the targets of the edges in [succs_e] of that loop *)
      val preds_v  : t -> Vertice_set.t          (* returns the origins of the edges in [preds_e] of that loop *)
      
      val mem    : Vertice.t -> t -> bool        (* tests if that vertice is part of that loop *)
      val center : Vertice.t -> t -> unit        (* if that vertice is part of that loop, move the starting point of the main path of the loop on that vertice *)
      val path   : t -> Edge.t list              (* returns the main path of the loop, once it has been centered *)
      
      val compare : t -> t -> int                 (* classic comparison function *)
      val equal   : t -> t -> bool                (* comparison that returns a bool instead of an int *)
    end and Loop_set : (Set.S with type elt = Loop.t) and Loop_map : (Map.S with type key = Loop.t)

    type t

    val create      : unit -> t                                            (* creates a new empty graph *)
    val add_vertice : vertice_data -> t -> Vertice.t                       (* adds a new vertice to the graph *)
    val add_edge    : Vertice.t -> edge_data -> Vertice.t -> t -> Edge.t   (* adds a new edge to the graph *)

    val vertices : t -> Vertice_set.t  (* returns the set of all vertices in the graph *)
    val edges    : t -> Edge_set.t     (* returns the set of all edges in the graph *)
    val loops    : t -> Loop_set.t     (* returns the set of all loops in the graph *) 
    
    val vertice_roots : t -> Vertice_set.t  (* returns the set of vertices that are roots (no ancestors) in the graph *)
    val loop_roots    : t -> Loop_set.t     (* returns the set of loops that are roots in the graph *) 
    val vertice_leafs : t -> Vertice_set.t  (* returns the set of vertices that are leafs (no children) in the graph *)
    val loop_leafs    : t -> Loop_set.t     (* returns the set of loops that are leafs in the graph *) 
    
    module Traverse_depth : sig
      val iter : (Path.t -> Vertice.t -> bool -> (unit -> unit) -> unit) -> t -> unit
    end
    
    module Traverse_topology : sig
      val iter_downward : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit
      val iter_upward   : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit 
    end    
  end

  module Make(V: Vertice_data)(E: Edge_data) : S with type vertice_data = V.t and type edge_data = E.t

end