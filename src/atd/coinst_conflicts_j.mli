(* Auto-generated from "coinst_conflicts.atd" *)


type id = Coinst_conflicts_t.id

type incompatibility = Coinst_conflicts_t.incompatibility

type class_definition = Coinst_conflicts_t.class_definition

type coinst_conflicts = Coinst_conflicts_t.coinst_conflicts = {
  classes: class_definition list;
  incompatibilities: incompatibility list
}

val write_id :
  Bi_outbuf.t -> id -> unit
  (** Output a JSON value of type {!id}. *)

val string_of_id :
  ?len:int -> id -> string
  (** Serialize a value of type {!id}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_id :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> id
  (** Input JSON data of type {!id}. *)

val id_of_string :
  string -> id
  (** Deserialize JSON data of type {!id}. *)

val write_incompatibility :
  Bi_outbuf.t -> incompatibility -> unit
  (** Output a JSON value of type {!incompatibility}. *)

val string_of_incompatibility :
  ?len:int -> incompatibility -> string
  (** Serialize a value of type {!incompatibility}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_incompatibility :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> incompatibility
  (** Input JSON data of type {!incompatibility}. *)

val incompatibility_of_string :
  string -> incompatibility
  (** Deserialize JSON data of type {!incompatibility}. *)

val write_class_definition :
  Bi_outbuf.t -> class_definition -> unit
  (** Output a JSON value of type {!class_definition}. *)

val string_of_class_definition :
  ?len:int -> class_definition -> string
  (** Serialize a value of type {!class_definition}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_class_definition :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> class_definition
  (** Input JSON data of type {!class_definition}. *)

val class_definition_of_string :
  string -> class_definition
  (** Deserialize JSON data of type {!class_definition}. *)

val write_coinst_conflicts :
  Bi_outbuf.t -> coinst_conflicts -> unit
  (** Output a JSON value of type {!coinst_conflicts}. *)

val string_of_coinst_conflicts :
  ?len:int -> coinst_conflicts -> string
  (** Serialize a value of type {!coinst_conflicts}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_coinst_conflicts :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> coinst_conflicts
  (** Input JSON data of type {!coinst_conflicts}. *)

val coinst_conflicts_of_string :
  string -> coinst_conflicts
  (** Deserialize JSON data of type {!coinst_conflicts}. *)

