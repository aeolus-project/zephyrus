
module type S = sig
  type reason
end

module type SOLVER = sig
  type state

  type reason

  type var = int
  type lit
  val lit_of_var : var -> bool -> lit

  val initialize_problem :
    ?print_var:(Format.formatter -> int -> unit) -> int -> state
  val propagate : state -> unit

  val protect : state -> unit
  val reset : state -> unit

  type value = True | False | Unknown
  val assignment : state -> value array

  val add_rule : state -> lit array -> reason list -> unit
  val associate_vars : state -> lit -> var list -> unit

  val solve : state -> var -> bool
  val solve_lst : state -> var list -> bool

  val collect_reasons : state -> var -> reason list
  val collect_reasons_lst : state -> var list -> reason list
end

module F (X : S) : SOLVER with type reason = X.reason
