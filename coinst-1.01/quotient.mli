
module F (R : Repository.S) : sig
  open R

  type t

  val perform : pool -> ?packages:PSet.t -> Formula.t PTbl.t -> t
  val trivial : pool -> t
  val subset : pool -> PSet.t -> t

  val formula : t -> Formula.t -> Formula.t
  val dependencies : t -> dependencies -> dependencies
  val conflicts : t -> Conflict.t -> Conflict.t
  val package_set : t -> PSet.t -> PSet.t

  val iter : (Package.t -> unit) -> t -> unit

  val print : t -> dependencies -> unit
  val print_class : t -> Format.formatter -> Package.t -> unit
  val clss : t -> Package.t -> PSet.t
  val class_size : t -> Package.t -> int

  val pool : t -> pool
end
