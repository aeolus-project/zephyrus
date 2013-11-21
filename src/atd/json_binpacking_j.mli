(* Auto-generated from "json_binpacking.atd" *)


(** Resources. *)

type resource_name = Json_binpacking_t.resource_name

type resource_consume_arity = Json_binpacking_t.resource_consume_arity

(** Items. *)
type resource_provide_arity = Json_binpacking_t.resource_provide_arity

type item_name = Json_binpacking_t.item_name

type item_arity = Json_binpacking_t.item_arity

(** Bins. *)
type item = Json_binpacking_t.item = {
  item_name (*atd name *): item_name;
  item_consume (*atd consume *):
    (resource_name * resource_consume_arity) list;
  item_arity (*atd arity *): item_arity
}

type bin_name = Json_binpacking_t.bin_name

type bin_cost = Json_binpacking_t.bin_cost

type bin_arity = Json_binpacking_t.bin_arity

(** Binpacking problem. *)
type bin = Json_binpacking_t.bin = {
  bin_name (*atd name *): bin_name;
  bin_provide (*atd provide *): (resource_name * resource_provide_arity) list;
  bin_cost (*atd cost *): bin_cost;
  bin_arity (*atd arity *): bin_arity
}

type binpacking_problem = Json_binpacking_t.binpacking_problem = {
  binpacking_problem_items (*atd items *): item list;
  binpacking_problem_bins (*atd bins *): bin list
}

val write_resource_name :
  Bi_outbuf.t -> resource_name -> unit
  (** Output a JSON value of type {!resource_name}. *)

val string_of_resource_name :
  ?len:int -> resource_name -> string
  (** Serialize a value of type {!resource_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resource_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resource_name
  (** Input JSON data of type {!resource_name}. *)

val resource_name_of_string :
  string -> resource_name
  (** Deserialize JSON data of type {!resource_name}. *)

val write_resource_consume_arity :
  Bi_outbuf.t -> resource_consume_arity -> unit
  (** Output a JSON value of type {!resource_consume_arity}. *)

val string_of_resource_consume_arity :
  ?len:int -> resource_consume_arity -> string
  (** Serialize a value of type {!resource_consume_arity}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resource_consume_arity :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resource_consume_arity
  (** Input JSON data of type {!resource_consume_arity}. *)

val resource_consume_arity_of_string :
  string -> resource_consume_arity
  (** Deserialize JSON data of type {!resource_consume_arity}. *)

val write_resource_provide_arity :
  Bi_outbuf.t -> resource_provide_arity -> unit
  (** Output a JSON value of type {!resource_provide_arity}. *)

val string_of_resource_provide_arity :
  ?len:int -> resource_provide_arity -> string
  (** Serialize a value of type {!resource_provide_arity}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resource_provide_arity :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resource_provide_arity
  (** Input JSON data of type {!resource_provide_arity}. *)

val resource_provide_arity_of_string :
  string -> resource_provide_arity
  (** Deserialize JSON data of type {!resource_provide_arity}. *)

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

