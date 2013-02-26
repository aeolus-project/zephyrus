
module F (R : Repository.S) : sig
  open R

  val output :
    ?package_weight:(Package.t -> float) ->
    ?edge_color:(Package.t -> Formula.t -> Disj.t -> string option) ->
    string -> ?mark_all:bool -> ?roots:Package.t list ->
    Quotient.F(R).t -> dependencies -> Conflict.t -> unit
end
