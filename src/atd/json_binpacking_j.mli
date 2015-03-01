(* Auto-generated from "json_binpacking.atd" *)


(** Resources. *)

(** Items. *)
type size = Json_binpacking_t.size

type repository_name = Json_binpacking_t.repository_name

type item_name = Json_binpacking_t.item_name

type item_arity = Json_binpacking_t.item_arity

type dimension = Json_binpacking_t.dimension

(** Bins. *)
type item = Json_binpacking_t.item = {
  item_name (*atd name *): item_name;
  item_sizes (*atd sizes *): (dimension * size) list;
  item_arity (*atd arity *): item_arity
}

type incompatibility = Json_binpacking_t.incompatibility

(** Binpacking problem. *)
type incompatibilities = Json_binpacking_t.incompatibilities

type bin_name = Json_binpacking_t.bin_name

type bin_cost = Json_binpacking_t.bin_cost

type bin_arity = Json_binpacking_t.bin_arity

(** Incompatibilities. *)
type bin = Json_binpacking_t.bin = {
  bin_name (*atd name *): bin_name;
  bin_sizes (*atd sizes *): (dimension * size) list;
  bin_cost (*atd cost *): bin_cost;
  bin_arity (*atd arity *): bin_arity
}

type binpacking_problem = Json_binpacking_t.binpacking_problem = {
  binpacking_problem_items (*atd items *): item list;
  binpacking_problem_bins (*atd bins *): bin list;
  binpacking_problem_incompatibilities (*atd incompatibilities *):
    (repository_name * incompatibilities) list
}

val write_size :
  Bi_outbuf.t -> size -> unit
  (** Output a JSON value of type {!size}. *)

val string_of_size :
  ?len:int -> size -> string
  (** Serialize a value of type {!size}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_size :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> size
  (** Input JSON data of type {!size}. *)

val size_of_string :
  string -> size
  (** Deserialize JSON data of type {!size}. *)

val write_repository_name :
  Bi_outbuf.t -> repository_name -> unit
  (** Output a JSON value of type {!repository_name}. *)

val string_of_repository_name :
  ?len:int -> repository_name -> string
  (** Serialize a value of type {!repository_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repository_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repository_name
  (** Input JSON data of type {!repository_name}. *)

val repository_name_of_string :
  string -> repository_name
  (** Deserialize JSON data of type {!repository_name}. *)

val write_item_name :
  Bi_outbuf.t -> item_name -> unit
  (** Output a JSON value of type {!item_name}. *)

val string_of_item_name :
  ?len:int -> item_name -> string
  (** Serialize a value of type {!item_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_item_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> item_name
  (** Input JSON data of type {!item_name}. *)

val item_name_of_string :
  string -> item_name
  (** Deserialize JSON data of type {!item_name}. *)

val write_item_arity :
  Bi_outbuf.t -> item_arity -> unit
  (** Output a JSON value of type {!item_arity}. *)

val string_of_item_arity :
  ?len:int -> item_arity -> string
  (** Serialize a value of type {!item_arity}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_item_arity :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> item_arity
  (** Input JSON data of type {!item_arity}. *)

val item_arity_of_string :
  string -> item_arity
  (** Deserialize JSON data of type {!item_arity}. *)

val write_dimension :
  Bi_outbuf.t -> dimension -> unit
  (** Output a JSON value of type {!dimension}. *)

val string_of_dimension :
  ?len:int -> dimension -> string
  (** Serialize a value of type {!dimension}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dimension :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dimension
  (** Input JSON data of type {!dimension}. *)

val dimension_of_string :
  string -> dimension
  (** Deserialize JSON data of type {!dimension}. *)

val write_item :
  Bi_outbuf.t -> item -> unit
  (** Output a JSON value of type {!item}. *)

val string_of_item :
  ?len:int -> item -> string
  (** Serialize a value of type {!item}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_item :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> item
  (** Input JSON data of type {!item}. *)

val item_of_string :
  string -> item
  (** Deserialize JSON data of type {!item}. *)

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

val write_incompatibilities :
  Bi_outbuf.t -> incompatibilities -> unit
  (** Output a JSON value of type {!incompatibilities}. *)

val string_of_incompatibilities :
  ?len:int -> incompatibilities -> string
  (** Serialize a value of type {!incompatibilities}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_incompatibilities :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> incompatibilities
  (** Input JSON data of type {!incompatibilities}. *)

val incompatibilities_of_string :
  string -> incompatibilities
  (** Deserialize JSON data of type {!incompatibilities}. *)

val write_bin_name :
  Bi_outbuf.t -> bin_name -> unit
  (** Output a JSON value of type {!bin_name}. *)

val string_of_bin_name :
  ?len:int -> bin_name -> string
  (** Serialize a value of type {!bin_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_bin_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> bin_name
  (** Input JSON data of type {!bin_name}. *)

val bin_name_of_string :
  string -> bin_name
  (** Deserialize JSON data of type {!bin_name}. *)

val write_bin_cost :
  Bi_outbuf.t -> bin_cost -> unit
  (** Output a JSON value of type {!bin_cost}. *)

val string_of_bin_cost :
  ?len:int -> bin_cost -> string
  (** Serialize a value of type {!bin_cost}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_bin_cost :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> bin_cost
  (** Input JSON data of type {!bin_cost}. *)

val bin_cost_of_string :
  string -> bin_cost
  (** Deserialize JSON data of type {!bin_cost}. *)

val write_bin_arity :
  Bi_outbuf.t -> bin_arity -> unit
  (** Output a JSON value of type {!bin_arity}. *)

val string_of_bin_arity :
  ?len:int -> bin_arity -> string
  (** Serialize a value of type {!bin_arity}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_bin_arity :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> bin_arity
  (** Input JSON data of type {!bin_arity}. *)

val bin_arity_of_string :
  string -> bin_arity
  (** Deserialize JSON data of type {!bin_arity}. *)

val write_bin :
  Bi_outbuf.t -> bin -> unit
  (** Output a JSON value of type {!bin}. *)

val string_of_bin :
  ?len:int -> bin -> string
  (** Serialize a value of type {!bin}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_bin :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> bin
  (** Input JSON data of type {!bin}. *)

val bin_of_string :
  string -> bin
  (** Deserialize JSON data of type {!bin}. *)

val write_binpacking_problem :
  Bi_outbuf.t -> binpacking_problem -> unit
  (** Output a JSON value of type {!binpacking_problem}. *)

val string_of_binpacking_problem :
  ?len:int -> binpacking_problem -> string
  (** Serialize a value of type {!binpacking_problem}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_binpacking_problem :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> binpacking_problem
  (** Input JSON data of type {!binpacking_problem}. *)

val binpacking_problem_of_string :
  string -> binpacking_problem
  (** Deserialize JSON data of type {!binpacking_problem}. *)

