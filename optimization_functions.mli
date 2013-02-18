
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

val cost_expr_number_of_all_components :                  universe -> expr
val cost_expr_number_of_used_locations : configuration -> universe -> expr
val cost_expr_compact                  : configuration -> universe -> expr
val cost_expr_difference_of_components : configuration -> universe -> expr