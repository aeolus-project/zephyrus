(* Auto-generated from "coinst_conflicts.atd" *)


type id = Coinst_conflicts_t.id

type class_definition = Coinst_conflicts_t.class_definition

type incompatibility = Coinst_conflicts_t.incompatibility

type coinst_conflicts = Coinst_conflicts_t.coinst_conflicts = {
  classes: class_definition list;
  incompatibilities: incompatibility list
}

let validate_id = (
  (fun _ _ -> None)
)
let validate__1 = (
  fun _ _ -> None
)
let validate_class_definition = (
  fun _ _ -> None
)
let validate_incompatibility = (
  validate__1
)
let validate__2 = (
  fun _ _ -> None
)
let validate__3 = (
  fun _ _ -> None
)
let validate_coinst_conflicts = (
  fun _ _ -> None
)
let create_coinst_conflicts 
  ~classes
  ~incompatibilities
  () =
  {
    classes = classes;
    incompatibilities = incompatibilities;
  }
