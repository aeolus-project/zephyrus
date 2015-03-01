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

let write_size = (
  Yojson.Safe.write_int
)
let string_of_size ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_size ob x;
  Bi_outbuf.contents ob
let read_size = (
  Ag_oj_run.read_int
)
let size_of_string s =
  read_size (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_repository_name = (
  Yojson.Safe.write_string
)
let string_of_repository_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_repository_name ob x;
  Bi_outbuf.contents ob
let read_repository_name = (
  Ag_oj_run.read_string
)
let repository_name_of_string s =
  read_repository_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_item_name = (
  Yojson.Safe.write_string
)
let string_of_item_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_item_name ob x;
  Bi_outbuf.contents ob
let read_item_name = (
  Ag_oj_run.read_string
)
let item_name_of_string s =
  read_item_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_item_arity = (
  Yojson.Safe.write_int
)
let string_of_item_arity ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_item_arity ob x;
  Bi_outbuf.contents ob
let read_item_arity = (
  Ag_oj_run.read_int
)
let item_arity_of_string s =
  read_item_arity (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_dimension = (
  Yojson.Safe.write_string
)
let string_of_dimension ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_dimension ob x;
  Bi_outbuf.contents ob
let read_dimension = (
  Ag_oj_run.read_string
)
let dimension_of_string s =
  read_dimension (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Ag_oj_run.write_assoc_list (
    write_size
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  Ag_oj_run.read_assoc_list (
    read_size
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_item : _ -> item -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      write_item_name
    )
      ob x.item_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"sizes\":";
    (
      write__1
    )
      ob x.item_sizes;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"arity\":";
    (
      write_item_arity
    )
      ob x.item_arity;
    Bi_outbuf.add_char ob '}';
)
let string_of_item ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_item ob x;
  Bi_outbuf.contents ob
let read_item = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : item) =
      {
        item_name = Obj.magic 0.0;
        item_sizes = Obj.magic 0.0;
        item_arity = Obj.magic 0.0;
      }
    in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'y' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'z' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            let v =
              (
                read_item_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read__1
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 2 ->
            let v =
              (
                read_item_arity
              ) p lb
            in
            Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            bits0 := !bits0 lor 0x4;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'y' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'z' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              let v =
                (
                  read_item_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read__1
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 2 ->
              let v =
                (
                  read_item_arity
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              bits0 := !bits0 lor 0x4;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields [| !bits0 |] [| "name"; "sizes"; "arity" |];
        Ag_oj_run.identity x
      )
)
let item_of_string s =
  read_item (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Ag_oj_run.write_list (
    write_item_name
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  Ag_oj_run.read_list (
    read_item_name
  )
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_incompatibility = (
  write__2
)
let string_of_incompatibility ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_incompatibility ob x;
  Bi_outbuf.contents ob
let read_incompatibility = (
  read__2
)
let incompatibility_of_string s =
  read_incompatibility (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Ag_oj_run.write_list (
    write_incompatibility
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
  Ag_oj_run.read_list (
    read_incompatibility
  )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_incompatibilities = (
  write__3
)
let string_of_incompatibilities ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_incompatibilities ob x;
  Bi_outbuf.contents ob
let read_incompatibilities = (
  read__3
)
let incompatibilities_of_string s =
  read_incompatibilities (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_bin_name = (
  Yojson.Safe.write_string
)
let string_of_bin_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_bin_name ob x;
  Bi_outbuf.contents ob
let read_bin_name = (
  Ag_oj_run.read_string
)
let bin_name_of_string s =
  read_bin_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_bin_cost = (
  Yojson.Safe.write_int
)
let string_of_bin_cost ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_bin_cost ob x;
  Bi_outbuf.contents ob
let read_bin_cost = (
  Ag_oj_run.read_int
)
let bin_cost_of_string s =
  read_bin_cost (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_bin_arity = (
  Yojson.Safe.write_int
)
let string_of_bin_arity ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_bin_arity ob x;
  Bi_outbuf.contents ob
let read_bin_arity = (
  Ag_oj_run.read_int
)
let bin_arity_of_string s =
  read_bin_arity (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_bin : _ -> bin -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      write_bin_name
    )
      ob x.bin_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"sizes\":";
    (
      write__1
    )
      ob x.bin_sizes;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"cost\":";
    (
      write_bin_cost
    )
      ob x.bin_cost;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"arity\":";
    (
      write_bin_arity
    )
      ob x.bin_arity;
    Bi_outbuf.add_char ob '}';
)
let string_of_bin ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_bin ob x;
  Bi_outbuf.contents ob
let read_bin = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : bin) =
      {
        bin_name = Obj.magic 0.0;
        bin_sizes = Obj.magic 0.0;
        bin_cost = Obj.magic 0.0;
        bin_arity = Obj.magic 0.0;
      }
    in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | 'n' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 5 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'y' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'z' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            let v =
              (
                read_bin_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read__1
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 2 ->
            let v =
              (
                read_bin_cost
              ) p lb
            in
            Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            bits0 := !bits0 lor 0x4;
          | 3 ->
            let v =
              (
                read_bin_arity
              ) p lb
            in
            Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            bits0 := !bits0 lor 0x8;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 'n' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 5 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'y' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'z' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              let v =
                (
                  read_bin_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read__1
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 2 ->
              let v =
                (
                  read_bin_cost
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              bits0 := !bits0 lor 0x4;
            | 3 ->
              let v =
                (
                  read_bin_arity
                ) p lb
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              bits0 := !bits0 lor 0x8;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0xf then Ag_oj_run.missing_fields [| !bits0 |] [| "name"; "sizes"; "cost"; "arity" |];
        Ag_oj_run.identity x
      )
)
let bin_of_string s =
  read_bin (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__6 = (
  Ag_oj_run.write_assoc_list (
    write_incompatibilities
  )
)
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let read__6 = (
  Ag_oj_run.read_assoc_list (
    read_incompatibilities
  )
)
let _6_of_string s =
  read__6 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__5 = (
  Ag_oj_run.write_list (
    write_bin
  )
)
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let read__5 = (
  Ag_oj_run.read_list (
    read_bin
  )
)
let _5_of_string s =
  read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__4 = (
  Ag_oj_run.write_list (
    write_item
  )
)
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let read__4 = (
  Ag_oj_run.read_list (
    read_item
  )
)
let _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_binpacking_problem : _ -> binpacking_problem -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"items\":";
    (
      write__4
    )
      ob x.binpacking_problem_items;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"bins\":";
    (
      write__5
    )
      ob x.binpacking_problem_bins;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"incompatibilities\":";
    (
      write__6
    )
      ob x.binpacking_problem_incompatibilities;
    Bi_outbuf.add_char ob '}';
)
let string_of_binpacking_problem ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_binpacking_problem ob x;
  Bi_outbuf.contents ob
let read_binpacking_problem = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : binpacking_problem) =
      {
        binpacking_problem_items = Obj.magic 0.0;
        binpacking_problem_bins = Obj.magic 0.0;
        binpacking_problem_incompatibilities = Obj.magic 0.0;
      }
    in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 17 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'p' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'b' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 's' then (
                  2
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            let v =
              (
                read__4
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read__5
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 2 ->
            let v =
              (
                read__6
              ) p lb
            in
            Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            bits0 := !bits0 lor 0x4;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 17 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'p' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'b' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 's' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              let v =
                (
                  read__4
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read__5
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 2 ->
              let v =
                (
                  read__6
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              bits0 := !bits0 lor 0x4;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields [| !bits0 |] [| "items"; "bins"; "incompatibilities" |];
        Ag_oj_run.identity x
      )
)
let binpacking_problem_of_string s =
  read_binpacking_problem (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
