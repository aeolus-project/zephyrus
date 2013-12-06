(* Auto-generated from "json_versions.atd" *)


type version = Json_versions_t.version

type versioned_object = Json_versions_t.versioned_object = {
  version: version
}

let validate_version = (
  fun x -> 0 <= x && x <= 1
)
let validate_versioned_object = (
  fun path x ->
    (
      validate_version
    ) (`Field "version" :: path) x.version
)
let create_versioned_object 
  ?(version = 0)
  () =
  {
    version = version;
  }
