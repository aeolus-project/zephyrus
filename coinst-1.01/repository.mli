
module type S = sig
  type pool

  module Package : sig
    type t
    val compare : t -> t -> int
    val print : pool -> Format.formatter -> t -> unit
    val print_name : pool -> Format.formatter -> t -> unit
    val index : t -> int
    val of_index : int -> t
    val of_index_list : int list -> t list
  end

  module PSet : Set.S with type elt = Package.t
  module PMap : Map.S with type key = Package.t

  module PTbl : sig
    type 'a t
    val create : pool -> 'a -> 'a t
    val init : pool -> (Package.t -> 'a) -> 'a t
    val get : 'a t -> Package.t -> 'a
    val set : 'a t -> Package.t -> 'a -> unit
    val iteri : (Package.t -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (Package.t -> 'a -> 'b) -> 'a t -> 'b t
  end

  module type DISJ = sig
    type t
    val print : pool -> Format.formatter -> t -> unit
    val implies : t -> t -> bool
    val equiv : t -> t -> bool
    val lit : Package.t -> t
    val lit_disj : Package.t list -> t
    val _false : t
    val disj : t -> t -> t
    val disjl : t list -> t
  end

  module Disj : sig
    include DISJ
    val iter : t -> (Package.t -> unit) -> unit
    val fold : (Package.t -> 'a -> 'a) -> t -> 'a -> 'a
    val cut : t -> Package.t -> t -> t
    val for_all : (Package.t -> bool) -> t -> bool
    val exists : (Package.t -> bool) -> t -> bool
    val filter : (Package.t -> bool) -> t -> t
    val compare : t -> t -> int
    val implies1 : Package.t -> t -> bool
    val to_lit : t -> Package.t option
    val to_lits : t -> PSet.t
  end

  module Formula : sig
    include DISJ
    val _true : t
    val conj : t -> t -> t
    val conjl : t list -> t
    val implies1 : t -> Disj.t -> bool
    val iter : t -> (Disj.t -> unit) -> unit
    val of_disj : Disj.t -> t
    val fold : (Disj.t -> 'a -> 'a) -> t -> 'a -> 'a
    val filter : (Disj.t -> bool) -> t -> t
    val exists : (Disj.t -> bool) -> t -> bool
    val map : (Disj.t -> Disj.t) -> t -> t
    val normalize : t -> t
  end

  type dependencies = Formula.t PTbl.t

  module Conflict : sig
    type t
    val create : pool -> t

    val check : t -> Package.t -> Package.t -> bool (*XXX rename in [mem]*)
    val add : t -> Package.t -> Package.t -> unit
    val remove : t -> Package.t -> Package.t -> unit
    val iter : t -> (Package.t -> Package.t -> unit) -> unit

    val has : t -> Package.t -> bool
    val of_package : t -> Package.t -> PSet.t

    val iter_on_packages : t -> (Package.t -> PSet.t -> unit) -> unit
    val exists : t -> (Package.t -> bool) -> Package.t -> bool
    val for_all : t -> (Package.t -> bool) -> Package.t -> bool
  end
end

module F : functor (M : Api.S) -> S with type pool = M.pool
