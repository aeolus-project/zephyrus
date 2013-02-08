
open Helpers

module type BARE_ARCHITECTURE =
  sig
    type bare_architecture_location
    type bare_architecture
  end

module BA = Aeolus_types_j

module type BARE_ARCHITECTURE_INPUT =
  sig
    val bare_architecture_location_of_string : string -> BA.bare_architecture_location
    val bare_architecture_of_string          : string -> BA.bare_architecture
 end

module JSON_bare_architecture_input : BARE_ARCHITECTURE_INPUT = Aeolus_types_j
