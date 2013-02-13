
open Helpers

module type CONFIGURATION =
  sig
    type location
    type configuration
  end

module C = Aeolus_types_j

module type CONFIGURATION_INPUT =
  sig
    val location_of_string      : string -> C.location
    val configuration_of_string : string -> C.configuration
 end

module JSON_configuration_input : CONFIGURATION_INPUT = Aeolus_types_j
