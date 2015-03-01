(* Auto-generated from "coinst_conflicts.atd" *)


type id = Coinst_conflicts_t.id

type incompatibility = Coinst_conflicts_t.incompatibility

type class_definition = Coinst_conflicts_t.class_definition

type coinst_conflicts = Coinst_conflicts_t.coinst_conflicts = {
  classes: class_definition list;
  incompatibilities: incompatibility list
}

let write_id = (
  Yojson.Safe.write_string
)
let string_of_id ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_id ob x;
  Bi_outbuf.contents ob
let read_id = (
  Ag_oj_run.read_string
)
let id_of_string s =
  read_id (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Ag_oj_run.write_list (
    write_id
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  Ag_oj_run.read_list (
    read_id
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_incompatibility = (
  write__1
)
let string_of_incompatibility ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_incompatibility ob x;
  Bi_outbuf.contents ob
let read_incompatibility = (
  read__1
)
let incompatibility_of_string s =
  read_incompatibility (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_class_definition = (
  fun ob x ->
    Bi_outbuf.add_char ob '[';
    (let x, _ = x in
    (
      write_id
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x = x in
    (
      write__1
    ) ob x
    );
    Bi_outbuf.add_char ob ']';
)
let string_of_class_definition ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_class_definition ob x;
  Bi_outbuf.contents ob
let read_class_definition = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            read_id
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            read__1
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1)
    with Yojson.End_of_tuple ->
      Ag_oj_run.missing_tuple_fields !len [ 0; 1 ]);
)
let class_definition_of_string s =
  read_class_definition (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
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
let write__2 = (
  Ag_oj_run.write_list (
    write_class_definition
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  Ag_oj_run.read_list (
    read_class_definition
  )
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_coinst_conflicts : _ -> coinst_conflicts -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"classes\":";
    (
      write__2
    )
      ob x.classes;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"incompatibilities\":";
    (
      write__3
    )
      ob x.incompatibilities;
    Bi_outbuf.add_char ob '}';
)
let string_of_coinst_conflicts ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_coinst_conflicts ob x;
  Bi_outbuf.contents ob
let read_coinst_conflicts = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : coinst_conflicts) =
      {
        classes = Obj.magic 0.0;
        incompatibilities = Obj.magic 0.0;
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
            | 7 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 17 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'p' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'b' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 's' then (
                  1
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
                read__2
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read__3
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
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
              | 7 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 17 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'p' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'b' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 's' then (
                    1
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
                  read__2
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read__3
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields [| !bits0 |] [| "classes"; "incompatibilities" |];
        Ag_oj_run.identity x
      )
)
let coinst_conflicts_of_string s =
  read_coinst_conflicts (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
