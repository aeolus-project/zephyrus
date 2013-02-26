
module type S = sig

  type pool
  type reason

  module Solver : Solver.SOLVER with type reason = reason

  val new_pool : unit -> pool

  val parse_packages : pool -> string list -> in_channel -> unit

  val parse_package_dependency : pool -> string -> int list
  val parse_package_name : pool -> string -> int list

  val print_pack : pool -> Format.formatter -> int -> unit
  val print_pack_name : pool -> Format.formatter -> int -> unit

  val show_reasons : pool -> reason list -> unit
  val conflicts_in_reasons : reason list -> (int * int) list

  val generate_rules : pool -> Solver.state

  val compute_conflicts : pool -> int list array
  val compute_deps : pool -> int list list array

  val pool_size : pool -> int


end

