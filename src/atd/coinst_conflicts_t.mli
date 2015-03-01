(* Auto-generated from "coinst_conflicts.atd" *)


type id = string

type incompatibility = id list

type class_definition = (id * id list)

type coinst_conflicts = {
  classes: class_definition list;
  incompatibilities: incompatibility list
}
