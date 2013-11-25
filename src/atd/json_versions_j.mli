(* Auto-generated from "json_versions.atd" *)


type version = Json_versions_t.version

type versioned_object = Json_versions_t.versioned_object = {
  version: version
}

val write_version :
  Bi_outbuf.t -> version -> unit
  (** Output a JSON value of type {!version}. *)

val string_of_version :
  ?len:int -> version -> string
  (** Serialize a value of type {!version}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val read_version :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> version
  (** Input JSON data of type {!version}. *)

val version_of_string :
  string -> version
  (** Deserialize JSON data of type {!version}. *)

val write_versioned_object :
  Bi_outbuf.t -> versioned_object -> unit
  (** Output a JSON value of type {!versioned_object}. *)

val string_of_versioned_object :
  ?len:int -> versioned_object -> string
  (** Serialize a value of type {!versioned_object}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val read_versioned_object :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> versioned_object
  (** Input JSON data of type {!versioned_object}. *)

val versioned_object_of_string :
  string -> versioned_object
  (** Deserialize JSON data of type {!versioned_object}. *)

