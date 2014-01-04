(* Auto-generated from "coinst_conflicts.atd" *)


type id = string

type class_definition = (id * id list)

type incompatibility = id list

type coinst_conflicts = {
  classes: class_definition list;
  incompatibilities: incompatibility list
}
