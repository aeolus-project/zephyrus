(****************************************************************************)
(*                                                                          *)
(*    This file is part of Zephyrus.                                        *)
(*                                                                          *)
(*    Zephyrus is free software: you can redistribute it and/or modify      *)
(*    it under the terms of the GNU General Public License as published by  *)
(*    the Free Software Foundation, either version 3 of the License, or     *)
(*    (at your option) any later version.                                   *)
(*                                                                          *)
(*    Zephyrus is distributed in the hope that it will be useful,           *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*    GNU General Public License for more details.                          *)
(*                                                                          *)
(*    You should have received a copy of the GNU General Public License     *)
(*    along with Zephyrus.  If not, see <http://www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)

(** Type safe modules and functors for a database. *)

(* Depends on
    - Hashtbl
*)

(* type safe modules and functors for a database. Support for SQL is not planned *)
module Database : sig 
  exception Table_not_found  (* raised when trying to access a table not present in the database *)
  exception Column_not_found (* raised with trying to access a column that is not present in a table *)

  (* type safe modules and functors for the definition of a database table *)
  module Table : sig
    (* type of a table module *)
    module type S = sig
      type t                (* type of the table *)
      type key              (* type of the main key of the table *)
      type 'a column  (* type for columns: we support value conversion, so ['a] is what is inserted, and ['b] is what is stored *)

      val create : int -> t (* create an empty table *)
      
      val mem : t -> key -> bool (* check if an entry was inserted for the given key *)
      val mem_in_column : t -> 'a column -> key -> bool (* check if an entry was inserted on the specific column -- has sense as we all columns to be optional *)
      
      type add_type (* the part of the type of [add] that varies.
           It has the form ['a -> 'b -> 'c -> unit] where ['a] is the type of the last non optional column, ['b] the second last, etc *)
      val add : t -> key -> add_type (* the function to insert an entry in the table. If present, replace the previous entry with the same key *)
      val add_to_column : t -> 'a column -> key -> 'a -> unit (* Used for insertion in optional columns *)
    
      val find : t -> 'a column -> key -> 'a (* get the value stored in a column for a given key (or the default value, if one is given) *)
      val find_key : t -> 'a column -> 'a -> key (* returns the key corresponding to the value *)
    end

    module Empty(K : sig type key type 'a column val compare : key -> key -> int end) 
      : S with type key = K.key and type 'a column = 'a K.column and type add_type = unit

   (* The creation of a table work in several steps:
        1. first, you create an empty table module using the [Empty] functor
        2-n. then, you iteratively add columns of the desired type and the desired functionality.
      The addition of a new column is done in 4 or 5 steps:
        1. first you define a module of type [General_input], with or without the [convert] function
        2. You feed this module to either [WithConversion] or [WithoutConversion] to enable or not value conversion during insertion in the column
        3. You feed the resulting module to either [WithChecking] or [WithoutChecking] to enable or not checking what is inserted in the table.
           This way, insertion can be canceled if an exception is raise by the [check] function
        4. You feed the resulting module to either [WithDefaultValue] or [WithoutDefaultValue] to set a default value for the column
        5. Finally, you finalize the addition by feeding the module you have to one of the following functors, each of then giving you different functionalities:
          - [Mandatory] has the classic column features: when inserting a new entry, you have to specify a value for that column
          - [Optional] do not require a value for a given entry (a value for that column can be inserted separately using [add_to_column])
          - [List] is a find of optional column, that keeps, instead of replacing previous value upon re-insertion of a key.
            The list of everything that was inserted for a key can be accessed with [find_list]
   *)

    module type Input = sig type t type key type 'a column val name : t  column end
    module type First_intermediate = sig include Input val check : (key, t) Hashtbl.t -> key -> t -> unit end
    module type Second_intermediate  = sig include First_intermediate val find : (key, t) Hashtbl.t -> key -> t end
    module type Third_intermediate = sig include Second_intermediate val aggregate : ((key, t) Hashtbl.t) -> ((t, key) Hashtbl.t) -> key -> t -> t end
    
    module WithChecking(C : Input)(P : sig val check : C.key -> C.t -> C.t option -> unit end)
      : First_intermediate with type t = C.t and type key = C.key and type 'a column = 'a C.column
    module WithoutChecking(C : Input)
      : First_intermediate with type t = C.t and type key = C.key and type 'a column = 'a C.column

    module WithDefaultValue(C : First_intermediate)(P : sig val default : C.t end)
      : Second_intermediate  with type t = C.t and type key = C.key and type 'a column = 'a C.column
    module WithoutDefaultValue(C : First_intermediate)
      : Second_intermediate  with type t = C.t and type key = C.key and type 'a column = 'a C.column

    module WithAggregate(C : Second_intermediate)(P : sig val aggregate : C.t -> C.t -> C.t end)
      : Third_intermediate with type t = C.t and type key = C.key and type 'a column = 'a C.column
    module WithoutAggregate(C : Second_intermediate)
      : Third_intermediate with type t = C.t and type key = C.key and type 'a column = 'a C.column


    module AddMandatory(C : Third_intermediate)(T : S with type key = C.key and type 'a column = 'a C.column)
      : S with type key = T.key and type 'a column = 'a C.column and type add_type = C.t -> T.add_type

    module AddOptional (C : Third_intermediate)(T : S with type key = C.key and type 'a column = 'a C.column)
      : S with type key = T.key and type 'a column = 'a C.column and type add_type = T.add_type
  end

  (* type of a database module *)
  module type S = sig
    type t               (* type of a database *)
    type 'a key          (* type of keys. The parameter 'a allows different table in the database to use different kind of keys *)
    type ('a, 'b) table  (* type for tables: 'a if the kind of key of the table, and 'b is its type [add_type] *)
    type 'a column       (* generic type for all columns *)

    val create : int -> t
      
    val mem : t -> ('a, 'b) table -> 'a key -> bool
    val mem_in_column : t -> ('a, 'b) table -> 'c column -> 'a key -> bool
      
    val add : t -> ('a, 'b) table -> 'a key -> 'b
    val add_to_column : t -> ('a, 'b) table -> 'c column -> 'a key -> 'c -> unit
    
    val find : t -> ('a, 'b) table -> 'c column -> 'a key -> 'd
    val find_key : t -> ('a, 'b) table -> 'c column -> 'c -> 'a key
  end

  module Empty(K : sig type 'a key type ('a, 'b) table type 'a column end)
    : S with type 'a key = 'a K.key and type ('a, 'b) table = ('a, 'b) K.table and type 'a column = 'a K.column


  module AddTable(T : Table.S)(Id : sig type ('a, 'b) table val name : (T.key, T.add_type) table end)
      (DB : S with type ('a, 'b) table = ('a, 'b) Id.table and type 'a column = 'a T.column)
    : S with type 'a key = 'a DB.key and type ('a, 'b) table = ('a, 'b) DB.table and type 'a column = 'a DB.column

end


