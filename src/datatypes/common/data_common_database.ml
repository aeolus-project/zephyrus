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

(** Database. *)

(* Depends on
    - Hashtbl
    - Data_common_set
*)

open Data_common_set

module Database = struct
  exception Table_not_found  (* exception raised when trying to access a table that is not present in the database *)
  exception Column_not_found (* exception raised when trying to access a column that is not present in the table *)


  module Table = struct
    module type S = sig      (* Interface of the table module *)
      type t                 (* Type of a table *)
      type key               (* Type of the key used in the table *)
      type 'a column         (* Generic type for all columns used in the table *)

      val create : int -> t  (* Create a new table with the initial capacity in parameter (automatically grow if necessary) *)
      
      val mem : t -> key -> bool                          (* Tests if the key was inserted in the table *)
      val mem_in_column : t -> 'a column -> key -> bool   (* Tests if the key was inserted in the column in parameter (useful, as columns can be optional) *)
      
      type add_type                                           (*  *)
      val add : t -> key -> add_type
      val add_to_column : t -> 'a column ->  key ->'a -> unit (*  *)
    
      val find : t -> 'a column -> key -> 'a
      val find_key : t -> 'a column -> 'a -> key
    end

    module Empty(K : sig type key type 'a column val compare : key -> key -> int end) = struct
      module Key_set = Set.Make(struct type t = K.key let compare = K.compare end)
      type t = { mutable mem : Key_set.t }
      type key = K.key
      type 'a column = 'a K.column
      
      let create _ = { mem = Key_set.empty }
      
      let mem m k = Key_set.mem k m.mem
      let mem_in_column m c k = raise Column_not_found
      
      type add_type = unit
      let add m k = m.mem <- Key_set.add k m.mem
      let add_to_column m c k v = raise Column_not_found
    
      let find m c k      = raise Column_not_found
      let find_key m c v  = raise Column_not_found
    end



    module type Input = sig type t type key type 'a column val name : t  column end
    module type First_intermediate = sig include Input val check : (key, t) Hashtbl.t -> key -> t -> unit end
    module type Second_intermediate  = sig include First_intermediate val find : (key, t) Hashtbl.t -> key -> t end
    module type Third_intermediate = sig include Second_intermediate val aggregate : ((key, t) Hashtbl.t) -> ((t, key) Hashtbl.t) -> key -> t -> t end
    
    module WithChecking(C : Input)(P : sig val check : C.key -> C.t -> C.t option -> unit end) = struct
      include C let check m k v = P.check k v (try Some (Hashtbl.find m k) with Not_found -> None) end
    module WithoutChecking(C : Input) = struct
      include C let check m k v = () end

    module WithDefaultValue(C : First_intermediate)(P : sig val default : C.t end) = struct
      include C let find m k = try Hashtbl.find m k with Not_found -> P.default
    end module WithoutDefaultValue(C : First_intermediate) = struct
      include C let find m k = Hashtbl.find m k
    end
    
    module WithAggregate(C : Second_intermediate)(P : sig val aggregate : C.t -> C.t -> C.t end) = struct
      include C let aggregate m1 m2 k v = try let v' = Hashtbl.find m1 k in Hashtbl.remove m2 v'; P.aggregate v' v with Not_found -> v end
    module WithoutAggregate(C : Second_intermediate) = struct
      include C let aggregate m1 m2 k v = v end


    module Structure(C : Third_intermediate)(T : S with type key = C.key and type 'a column = 'a C.column) = struct
      type t = (((C.key, C.t) Hashtbl.t) * ((C.t, C.key) Hashtbl.t)) * T.t
      type key = T.key
      type 'a column = 'a T.column

      let create n = ((Hashtbl.create n, Hashtbl.create n), T.create n)

      let mem (_,p) = T.mem p
      let mem_in_column ((m1,m2),p) c k = if (Obj.magic c) == C.name then Hashtbl.mem m1 k else T.mem_in_column p c k
    
      let local_add (m1,m2) k v' =
        let v_tmp: C.t = Obj.magic v' in C.check m1 k v_tmp;
        let v = C.aggregate m1 m2 k v_tmp in Hashtbl.replace m1 k v; Hashtbl.replace m2 v k

      let add_to_column (m,p) c k v = if (Obj.magic c) != C.name then T.add_to_column p c k v
        else (if not (T.mem p k) then raise Not_found else local_add m k v)

      let find ((m1,m2),p) c k = if (Obj.magic c) == C.name then Obj.magic (C.find m1 k) else T.find p c k
      let find_key ((m1,m2),p) c v = if (Obj.magic c) == C.name then Hashtbl.find m2 (Obj.magic v) else T.find_key p c v
    end


    module AddMandatory(C : Third_intermediate)(T : S with type key = C.key and type 'a column = 'a C.column) = struct
      include Structure(C)(T)

      type add_type = C.t -> T.add_type
      let add (m,p) k v = local_add m k v; T.add p k
    end

    module AddOptional(C : Third_intermediate)(T : S with type key = C.key and type 'a column = 'a C.column) = struct
      include Structure(C)(T)

      type add_type = T.add_type
      let add (_,p) = T.add p
    end
    
  end

  module type S = sig
    type t
    type 'a key
    type ('a, 'b) table
    type 'a column

    val create : int -> t
      
    val mem : t -> ('a, 'b) table -> 'a key -> bool
    val mem_in_column : t -> ('a, 'b) table -> 'c column -> 'a key -> bool
      
    val add : t -> ('a, 'b) table -> 'a key -> 'b
    val add_to_column : t -> ('a, 'b) table -> 'c column -> 'a key -> 'c -> unit
    
    val find : t -> ('a, 'b) table -> 'c column -> 'a key -> 'd
    val find_key : t -> ('a, 'b) table -> 'c column -> 'c -> 'a key
  end

  module Empty(K : sig type 'a key type ('a, 'b) table type 'a column end) = struct
    type t = unit
    type 'a key = 'a K.key
    type ('a, 'b) table = ('a, 'b) K.table
    type 'a column = 'a K.column
      
    let create _ = ()
      
    let mem m t k             = raise Table_not_found
    let mem_in_column m t c k = raise Table_not_found
      
    let add m t k               = raise Table_not_found
    let add_to_column m t c k v = raise Table_not_found
    
    let find m t c k      = raise Table_not_found
    let find_list m t c k = raise Table_not_found
    let find_key m t c v  = raise Table_not_found
  end

  module AddTable(T : Table.S)(Id : sig type ('a, 'b) table val name : (T.key, T.add_type) table end)
      (DB : S with type ('a, 'b) table = ('a, 'b) Id.table and type 'a column = 'a T.column) = struct
    type t = T.t * DB.t
    type 'a key = 'a DB.key
    type ('a, 'b) table = ('a, 'b) DB.table
    type 'a column = 'a DB.column
    
    let create n = (T.create n, DB.create n)
    
    let mem (m,p) t k = if (Obj.magic t) == Id.name then T.mem m (Obj.magic k) else DB.mem p t k
    let mem_in_column (m,p) t c k = if (Obj.magic t) == Id.name then T.mem_in_column m c (Obj.magic k) else DB.mem_in_column p t c k
    
    let add (m,p) t k = if (Obj.magic t) == Id.name then Obj.magic (T.add m (Obj.magic k)) else DB.add p t k
    let add_to_column (m,p) t c k v = if (Obj.magic t) == Id.name then T.add_to_column m c (Obj.magic k) v else DB.add_to_column p t c k v
    
    let find (m,p) t c k      = if (Obj.magic t) == Id.name then Obj.magic (T.find m c (Obj.magic k)) else DB.find p t c k
    let find_key (m,p) t c v  = if (Obj.magic t) == Id.name then Obj.magic (T.find_key m c v) else DB.find_key p t c v
  end

end


(*
(* NOTE: Moved from zephyrus.ml *)

(* test the database *)
module Database_test = struct
  open Data_common_database.Database
  module DBBase = struct
    type 'a column = int
    type key = int
    let compare = (-)
  end

  module DBBool = struct
    include DBBase
    type t = bool
    let name : bool column = 1
  end

  module DBString = struct
    include DBBase
    type t = string
    let name : string column = 2
  end

  module T = Table.AddOptional(Table.WithoutAggregate(Table.WithDefaultValue(Table.WithoutChecking((DBBool)))(struct let default = false end))) (
             Table.AddOptional(Table.WithoutAggregate(Table.WithDefaultValue(Table.WithoutChecking((DBString)))(struct let default = "no one" end))) (
               Data_common.Database.Table.Empty(struct include DBBase type t = key end)))
  let () = 
    let table = T.create 5 in
      print_string "step 1\n"; flush stdout;
      T.add table 0;
      print_string "step 2\n"; flush stdout;
      T.add_to_column table DBString.name 0 "is_working? ";
      print_string "step 3\n"; flush stdout;
      T.add_to_column table DBBool.name 0 true;
      print_string "step 4\n"; flush stdout;
      print_string ((T.find table DBString.name 0) ^ (string_of_bool (T.find table DBBool.name 0)) ^ "\n");
      print_string "step 5\n"; flush stdout;
      T.add table 1;
      print_string "step 6\n"; flush stdout;
      print_string ((T.find table DBString.name 1) ^ (string_of_bool (T.find table DBBool.name 1)) ^ "\n");
      print_string "step 7\n"; flush stdout
  end

*)
