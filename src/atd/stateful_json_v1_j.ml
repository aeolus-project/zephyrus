(* Auto-generated from "stateful_json_v1.atd" *)


(** Type definition for stateful syntax version. *)

(** Type definitions for naming. *)
type version = Json_versions_t.version

type state_name = Stateful_json_v1_t.state_name

type require_arity = Stateful_json_v1_t.require_arity

type provide_arity = Stateful_json_v1_t.provide_arity

type port_name = Stateful_json_v1_t.port_name

type state = Stateful_json_v1_t.state = {
  state_name (*atd name *): state_name;
  state_initial (*atd initial *): bool;
  state_final (*atd final *): bool;
  state_provide (*atd provide *): (port_name * provide_arity) list;
  state_require (*atd require *): (port_name * require_arity) list;
  state_conflict (*atd conflict *): port_name list;
  state_successors (*atd successors *): state_name list
}

(** Type definitions for Universe. *)
type resource_name = Stateful_json_v1_t.resource_name

type resource_consumption = Stateful_json_v1_t.resource_consumption

type repository_name = Stateful_json_v1_t.repository_name

type package_name = Stateful_json_v1_t.package_name

type package = Stateful_json_v1_t.package = {
  package_name (*atd name *): package_name;
  package_depend (*atd depend *): package_name list list;
  package_conflict (*atd conflict *): package_name list;
  package_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type repository = Stateful_json_v1_t.repository = {
  repository_name (*atd name *): repository_name;
  repository_packages (*atd packages *): package list
}

type repositories = Stateful_json_v1_t.repositories

type implementation_package = Stateful_json_v1_t.implementation_package = {
  implementation_package_repository (*atd repository *): repository_name;
  implementation_package_package (*atd package *): package_name
}

type implementation_packages = Stateful_json_v1_t.implementation_packages

type component_type_name = Stateful_json_v1_t.component_type_name

type component_type_stateful = Stateful_json_v1_t.component_type_stateful = {
  component_type_stateful_name (*atd name *): component_type_name;
  component_type_stateful_states (*atd states *): state list;
  component_type_stateful_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type component_type = Stateful_json_v1_t.component_type

type component_types = Stateful_json_v1_t.component_types

(** Type definitions for Configuration. *)
type universe = Stateful_json_v1_t.universe = {
  universe_version (*atd version *): version;
  universe_component_types (*atd component_types *): component_types;
  universe_implementation (*atd implementation *):
    (component_type_name * implementation_packages) list;
  universe_repositories (*atd repositories *): repositories;
  universe_port_hierarchy (*atd port_hierarchy *):
    (port_name * (port_name list)) list
}

type resource_provide_arity = Stateful_json_v1_t.resource_provide_arity

type resources_provided = Stateful_json_v1_t.resources_provided

type port_hierarchy = Stateful_json_v1_t.port_hierarchy = {
  port_hierarchy_port (*atd port *): port_name;
  port_hierarchy_subports (*atd subports *): port_name list
}

type packages = Stateful_json_v1_t.packages

type location_name = Stateful_json_v1_t.location_name

type location_cost = Stateful_json_v1_t.location_cost

type location = Stateful_json_v1_t.location = {
  location_name (*atd name *): location_name;
  location_provide_resources (*atd provide_resources *): resources_provided;
  location_repository (*atd repository *): repository_name;
  location_packages_installed (*atd packages_installed *): package_name list;
  location_cost (*atd cost *): location_cost
}

type component_name = Stateful_json_v1_t.component_name

type component_stateful = Stateful_json_v1_t.component_stateful = {
  component_stateful_name (*atd name *): component_name;
  component_stateful_type (*atd component_type_workaround *):
    component_type_name;
  component_stateful_state (*atd state *): state_name;
  component_stateful_location (*atd location *): location_name
}

type component = Stateful_json_v1_t.component

type binding_hierarchical = Stateful_json_v1_t.binding_hierarchical = {
  binding_hierarchical_port_required (*atd port_required *): port_name;
  binding_hierarchical_port_provided (*atd port_provided *): port_name;
  binding_hierarchical_requirer (*atd requirer *): component_name;
  binding_hierarchical_provider (*atd provider *): component_name
}

type binding = Stateful_json_v1_t.binding

type configuration = Stateful_json_v1_t.configuration = {
  configuration_version (*atd version *): version;
  configuration_locations (*atd locations *): location list;
  configuration_components (*atd components *): component list;
  configuration_bindings (*atd bindings *): binding list
}

type component_type_simple = Stateful_json_v1_t.component_type_simple = {
  component_type_simple_name (*atd name *): component_type_name;
  component_type_simple_provide (*atd provide *):
    (port_name * provide_arity) list;
  component_type_simple_require (*atd require *):
    (port_name * require_arity) list;
  component_type_simple_conflict (*atd conflict *): port_name list;
  component_type_simple_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type component_simple = Stateful_json_v1_t.component_simple = {
  component_simple_name (*atd name *): component_name;
  component_simple_type (*atd component_type_workaround *):
    component_type_name;
  component_simple_location (*atd location *): location_name
}

type binding_simple = Stateful_json_v1_t.binding_simple = {
  binding_simple_port (*atd port *): port_name;
  binding_simple_requirer (*atd requirer *): component_name;
  binding_simple_provider (*atd provider *): component_name
}

let write_version = (
  Json_versions_j.write_version
)
let string_of_version ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_version ob x;
  Bi_outbuf.contents ob
let read_version = (
  Json_versions_j.read_version
)
let version_of_string s =
  read_version (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_state_name = (
  Yojson.Safe.write_string
)
let string_of_state_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_state_name ob x;
  Bi_outbuf.contents ob
let read_state_name = (
  Ag_oj_run.read_string
)
let state_name_of_string s =
  read_state_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_require_arity = (
  Yojson.Safe.write_int
)
let string_of_require_arity ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_require_arity ob x;
  Bi_outbuf.contents ob
let read_require_arity = (
  Ag_oj_run.read_int
)
let require_arity_of_string s =
  read_require_arity (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_provide_arity = (
  (* Workaround: 
     - a string containing a number should be printed as a JSON number (i.e. no surrounding quotes), 
     - any other string should be printed as a JSON string (i.e. with surrounding quotes). *)
  fun ob x -> 
  try
    let i = int_of_string x in
    Yojson.Safe.write_int ob i
  with _ -> 
    Yojson.Safe.write_string ob x
)
let string_of_provide_arity ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_provide_arity ob x;
  Bi_outbuf.contents ob
let read_provide_arity = (
  (* Workaround: as we expect a JSON string or a JSON integer here,
     and we have to output a string either way:
     - in case of a string we return it directly,
     - in case of an integer we convert it to string before returning,
     - otherwise we raise an exception. *)
  fun (p : Yojson.Safe.lexer_state) (lb : Lexing.lexbuf) ->
    Yojson.Safe.read_space p lb;
    let json = Yojson.Safe.from_lexbuf p lb ~stream: true in
    match json with
    | `Int    (i) -> string_of_int i
    | `String (s) -> s
    | _ -> failwith "Universe parsing: provide arity must be a string or an integer." (* TODO: Raise a proper ATD exception? *)
)
let provide_arity_of_string s =
  read_provide_arity (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_port_name = (
  Yojson.Safe.write_string
)
let string_of_port_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_port_name ob x;
  Bi_outbuf.contents ob
let read_port_name = (
  Ag_oj_run.read_string
)
let port_name_of_string s =
  read_port_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__4 = (
  Ag_oj_run.write_list (
    write_state_name
  )
)
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let read__4 = (
  Ag_oj_run.read_list (
    read_state_name
  )
)
let _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Ag_oj_run.write_list (
    write_port_name
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
  Ag_oj_run.read_list (
    read_port_name
  )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Ag_oj_run.write_assoc_list (
    write_require_arity
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  Ag_oj_run.read_assoc_list (
    read_require_arity
  )
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Ag_oj_run.write_assoc_list (
    write_provide_arity
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  Ag_oj_run.read_assoc_list (
    read_provide_arity
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_state : _ -> state -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      write_state_name
    )
      ob x.state_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"initial\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.state_initial;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"final\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.state_final;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"provide\":";
    (
      write__1
    )
      ob x.state_provide;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"require\":";
    (
      write__2
    )
      ob x.state_require;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"conflict\":";
    (
      write__3
    )
      ob x.state_conflict;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"successors\":";
    (
      write__4
    )
      ob x.state_successors;
    Bi_outbuf.add_char ob '}';
)
let string_of_state ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_state ob x;
  Bi_outbuf.contents ob
let read_state = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : state) =
      {
        state_name = Obj.magic 0.0;
        state_initial = false;
        state_final = false;
        state_provide = [];
        state_require = [];
        state_conflict = [];
        state_successors = [];
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
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'l' then (
                  2
                )
                else (
                  -1
                )
              )
            | 7 -> (
                match String.unsafe_get s pos with
                  | 'i' -> (
                      if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | 'r' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'q' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 't' then (
                  5
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 's' then (
                  6
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
                read_state_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  Ag_oj_run.read_bool
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            )
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  Ag_oj_run.read_bool
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__1
                ) p lb
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            )
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__2
                ) p lb
              in
              Obj.set_field (Obj.repr x) 4 (Obj.repr v);
            )
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__3
                ) p lb
              in
              Obj.set_field (Obj.repr x) 5 (Obj.repr v);
            )
          | 6 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__4
                ) p lb
              in
              Obj.set_field (Obj.repr x) 6 (Obj.repr v);
            )
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
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'l' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  match String.unsafe_get s pos with
                    | 'i' -> (
                        if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | 'r' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'q' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 't' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 's' then (
                    6
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
                  read_state_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    Ag_oj_run.read_bool
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              )
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    Ag_oj_run.read_bool
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__1
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              )
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__2
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 4 (Obj.repr v);
              )
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__3
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 5 (Obj.repr v);
              )
            | 6 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__4
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 6 (Obj.repr v);
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields [| !bits0 |] [| "name" |];
        Ag_oj_run.identity x
      )
)
let state_of_string s =
  read_state (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_resource_name = (
  Yojson.Safe.write_string
)
let string_of_resource_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_resource_name ob x;
  Bi_outbuf.contents ob
let read_resource_name = (
  Ag_oj_run.read_string
)
let resource_name_of_string s =
  read_resource_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_resource_consumption = (
  Yojson.Safe.write_int
)
let string_of_resource_consumption ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_resource_consumption ob x;
  Bi_outbuf.contents ob
let read_resource_consumption = (
  Ag_oj_run.read_int
)
let resource_consumption_of_string s =
  read_resource_consumption (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
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
let write_package_name = (
  Yojson.Safe.write_string
)
let string_of_package_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_package_name ob x;
  Bi_outbuf.contents ob
let read_package_name = (
  Ag_oj_run.read_string
)
let package_name_of_string s =
  read_package_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__8 = (
  Ag_oj_run.write_list (
    write_package_name
  )
)
let string_of__8 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__8 ob x;
  Bi_outbuf.contents ob
let read__8 = (
  Ag_oj_run.read_list (
    read_package_name
  )
)
let _8_of_string s =
  read__8 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__9 = (
  Ag_oj_run.write_list (
    write__8
  )
)
let string_of__9 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__9 ob x;
  Bi_outbuf.contents ob
let read__9 = (
  Ag_oj_run.read_list (
    read__8
  )
)
let _9_of_string s =
  read__9 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__6 = (
  Ag_oj_run.write_assoc_list (
    write_resource_consumption
  )
)
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let read__6 = (
  Ag_oj_run.read_assoc_list (
    read_resource_consumption
  )
)
let _6_of_string s =
  read__6 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_package : _ -> package -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      write_package_name
    )
      ob x.package_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"depend\":";
    (
      write__9
    )
      ob x.package_depend;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"conflict\":";
    (
      write__8
    )
      ob x.package_conflict;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"consume\":";
    (
      write__6
    )
      ob x.package_consume;
    Bi_outbuf.add_char ob '}';
)
let string_of_package ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_package ob x;
  Bi_outbuf.contents ob
let read_package = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : package) =
      {
        package_name = Obj.magic 0.0;
        package_depend = [];
        package_conflict = [];
        package_consume = [];
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
            | 6 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'd' then (
                  1
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' then (
                  3
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 't' then (
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
                read_package_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__9
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            )
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__8
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__6
                ) p lb
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            )
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
              | 6 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 't' then (
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
                  read_package_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__9
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              )
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__8
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__6
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields [| !bits0 |] [| "name" |];
        Ag_oj_run.identity x
      )
)
let package_of_string s =
  read_package (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__10 = (
  Ag_oj_run.write_list (
    write_package
  )
)
let string_of__10 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__10 ob x;
  Bi_outbuf.contents ob
let read__10 = (
  Ag_oj_run.read_list (
    read_package
  )
)
let _10_of_string s =
  read__10 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_repository : _ -> repository -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      write_repository_name
    )
      ob x.repository_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"packages\":";
    (
      write__10
    )
      ob x.repository_packages;
    Bi_outbuf.add_char ob '}';
)
let string_of_repository ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_repository ob x;
  Bi_outbuf.contents ob
let read_repository = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : repository) =
      {
        repository_name = Obj.magic 0.0;
        repository_packages = [];
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
            | 8 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' then (
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
                read_repository_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__10
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            )
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
              | 8 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' then (
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
                  read_repository_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__10
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields [| !bits0 |] [| "name" |];
        Ag_oj_run.identity x
      )
)
let repository_of_string s =
  read_repository (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__11 = (
  Ag_oj_run.write_list (
    write_repository
  )
)
let string_of__11 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__11 ob x;
  Bi_outbuf.contents ob
let read__11 = (
  Ag_oj_run.read_list (
    read_repository
  )
)
let _11_of_string s =
  read__11 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_repositories = (
  write__11
)
let string_of_repositories ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_repositories ob x;
  Bi_outbuf.contents ob
let read_repositories = (
  read__11
)
let repositories_of_string s =
  read_repositories (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_implementation_package : _ -> implementation_package -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"repository\":";
    (
      write_repository_name
    )
      ob x.implementation_package_repository;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"package\":";
    (
      write_package_name
    )
      ob x.implementation_package_package;
    Bi_outbuf.add_char ob '}';
)
let string_of_implementation_package ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_implementation_package ob x;
  Bi_outbuf.contents ob
let read_implementation_package = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : implementation_package) =
      {
        implementation_package_repository = Obj.magic 0.0;
        implementation_package_package = Obj.magic 0.0;
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
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 'e' then (
                  1
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'y' then (
                  0
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
                read_repository_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read_package_name
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
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 'e' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'y' then (
                    0
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
                  read_repository_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read_package_name
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
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields [| !bits0 |] [| "repository"; "package" |];
        Ag_oj_run.identity x
      )
)
let implementation_package_of_string s =
  read_implementation_package (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__12 = (
  Ag_oj_run.write_list (
    write_implementation_package
  )
)
let string_of__12 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__12 ob x;
  Bi_outbuf.contents ob
let read__12 = (
  Ag_oj_run.read_list (
    read_implementation_package
  )
)
let _12_of_string s =
  read__12 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_implementation_packages = (
  write__12
)
let string_of_implementation_packages ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_implementation_packages ob x;
  Bi_outbuf.contents ob
let read_implementation_packages = (
  read__12
)
let implementation_packages_of_string s =
  read_implementation_packages (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component_type_name = (
  Yojson.Safe.write_string
)
let string_of_component_type_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_type_name ob x;
  Bi_outbuf.contents ob
let read_component_type_name = (
  Ag_oj_run.read_string
)
let component_type_name_of_string s =
  read_component_type_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__5 = (
  Ag_oj_run.write_list (
    write_state
  )
)
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let read__5 = (
  Ag_oj_run.read_list (
    read_state
  )
)
let _5_of_string s =
  read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component_type_stateful : _ -> component_type_stateful -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      write_component_type_name
    )
      ob x.component_type_stateful_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"states\":";
    (
      write__5
    )
      ob x.component_type_stateful_states;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"consume\":";
    (
      write__6
    )
      ob x.component_type_stateful_consume;
    Bi_outbuf.add_char ob '}';
)
let string_of_component_type_stateful ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_type_stateful ob x;
  Bi_outbuf.contents ob
let read_component_type_stateful = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : component_type_stateful) =
      {
        component_type_stateful_name = Obj.magic 0.0;
        component_type_stateful_states = Obj.magic 0.0;
        component_type_stateful_consume = [];
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
            | 6 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' then (
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
                read_component_type_name
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
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__6
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            )
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
              | 6 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' then (
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
                  read_component_type_name
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
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__6
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields [| !bits0 |] [| "name"; "states" |];
        Ag_oj_run.identity x
      )
)
let component_type_stateful_of_string s =
  read_component_type_stateful (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component_type = (
  write_component_type_stateful
)
let string_of_component_type ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_type ob x;
  Bi_outbuf.contents ob
let read_component_type = (
  read_component_type_stateful
)
let component_type_of_string s =
  read_component_type (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__7 = (
  Ag_oj_run.write_list (
    write_component_type
  )
)
let string_of__7 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__7 ob x;
  Bi_outbuf.contents ob
let read__7 = (
  Ag_oj_run.read_list (
    read_component_type
  )
)
let _7_of_string s =
  read__7 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component_types = (
  write__7
)
let string_of_component_types ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_types ob x;
  Bi_outbuf.contents ob
let read_component_types = (
  read__7
)
let component_types_of_string s =
  read_component_types (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__14 = (
  Ag_oj_run.write_assoc_list (
    fun ob x ->
      Bi_outbuf.add_char ob '[';
      (let x = x in
      (
        write__3
      ) ob x
      );
      Bi_outbuf.add_char ob ']';
  )
)
let string_of__14 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__14 ob x;
  Bi_outbuf.contents ob
let read__14 = (
  Ag_oj_run.read_assoc_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              read__3
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
        (x0)
      with Yojson.End_of_tuple ->
        Ag_oj_run.missing_tuple_fields !len [ 0 ]);
  )
)
let _14_of_string s =
  read__14 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__13 = (
  Ag_oj_run.write_assoc_list (
    write_implementation_packages
  )
)
let string_of__13 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__13 ob x;
  Bi_outbuf.contents ob
let read__13 = (
  Ag_oj_run.read_assoc_list (
    read_implementation_packages
  )
)
let _13_of_string s =
  read__13 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_universe : _ -> universe -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"version\":";
    (
      write_version
    )
      ob x.universe_version;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"component_types\":";
    (
      write_component_types
    )
      ob x.universe_component_types;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"implementation\":";
    (
      write__13
    )
      ob x.universe_implementation;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"repositories\":";
    (
      write_repositories
    )
      ob x.universe_repositories;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"port_hierarchy\":";
    (
      write__14
    )
      ob x.universe_port_hierarchy;
    Bi_outbuf.add_char ob '}';
)
let string_of_universe ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_universe ob x;
  Bi_outbuf.contents ob
let read_universe = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : universe) =
      {
        universe_version = Obj.magic 0.0;
        universe_component_types = [];
        universe_implementation = [];
        universe_repositories = [];
        universe_port_hierarchy = [];
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
                if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'n' then (
                  0
                )
                else (
                  -1
                )
              )
            | 12 -> (
                if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 's' then (
                  3
                )
                else (
                  -1
                )
              )
            | 14 -> (
                match String.unsafe_get s pos with
                  | 'i' -> (
                      if String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'n' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'c' && String.unsafe_get s (pos+12) = 'h' && String.unsafe_get s (pos+13) = 'y' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 15 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'y' && String.unsafe_get s (pos+12) = 'p' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 's' then (
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
                read_version
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read_component_types
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            )
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__13
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read_repositories
                ) p lb
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            )
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__14
                ) p lb
              in
              Obj.set_field (Obj.repr x) 4 (Obj.repr v);
            )
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
                  if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'n' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 12 -> (
                  if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 's' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 14 -> (
                  match String.unsafe_get s pos with
                    | 'i' -> (
                        if String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'n' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'c' && String.unsafe_get s (pos+12) = 'h' && String.unsafe_get s (pos+13) = 'y' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'y' && String.unsafe_get s (pos+12) = 'p' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 's' then (
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
                  read_version
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read_component_types
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              )
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__13
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read_repositories
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              )
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__14
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 4 (Obj.repr v);
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields [| !bits0 |] [| "version" |];
        Ag_oj_run.identity x
      )
)
let universe_of_string s =
  read_universe (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_resource_provide_arity = (
  Yojson.Safe.write_int
)
let string_of_resource_provide_arity ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_resource_provide_arity ob x;
  Bi_outbuf.contents ob
let read_resource_provide_arity = (
  Ag_oj_run.read_int
)
let resource_provide_arity_of_string s =
  read_resource_provide_arity (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__15 = (
  Ag_oj_run.write_assoc_list (
    write_resource_provide_arity
  )
)
let string_of__15 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__15 ob x;
  Bi_outbuf.contents ob
let read__15 = (
  Ag_oj_run.read_assoc_list (
    read_resource_provide_arity
  )
)
let _15_of_string s =
  read__15 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_resources_provided = (
  write__15
)
let string_of_resources_provided ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_resources_provided ob x;
  Bi_outbuf.contents ob
let read_resources_provided = (
  read__15
)
let resources_provided_of_string s =
  read_resources_provided (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_port_hierarchy : _ -> port_hierarchy -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"port\":";
    (
      write_port_name
    )
      ob x.port_hierarchy_port;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"subports\":";
    (
      write__3
    )
      ob x.port_hierarchy_subports;
    Bi_outbuf.add_char ob '}';
)
let string_of_port_hierarchy ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_port_hierarchy ob x;
  Bi_outbuf.contents ob
let read_port_hierarchy = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : port_hierarchy) =
      {
        port_hierarchy_port = Obj.magic 0.0;
        port_hierarchy_subports = Obj.magic 0.0;
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
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' then (
                  0
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 's' then (
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
                read_port_name
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
              | 4 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 's' then (
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
                  read_port_name
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
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields [| !bits0 |] [| "port"; "subports" |];
        Ag_oj_run.identity x
      )
)
let port_hierarchy_of_string s =
  read_port_hierarchy (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_packages = (
  write__10
)
let string_of_packages ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_packages ob x;
  Bi_outbuf.contents ob
let read_packages = (
  read__10
)
let packages_of_string s =
  read_packages (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_location_name = (
  Yojson.Safe.write_string
)
let string_of_location_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_location_name ob x;
  Bi_outbuf.contents ob
let read_location_name = (
  Ag_oj_run.read_string
)
let location_name_of_string s =
  read_location_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_location_cost = (
  Yojson.Safe.write_int
)
let string_of_location_cost ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_location_cost ob x;
  Bi_outbuf.contents ob
let read_location_cost = (
  Ag_oj_run.read_int
)
let location_cost_of_string s =
  read_location_cost (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_location : _ -> location -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      write_location_name
    )
      ob x.location_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"provide_resources\":";
    (
      write_resources_provided
    )
      ob x.location_provide_resources;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"repository\":";
    (
      write_repository_name
    )
      ob x.location_repository;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"packages_installed\":";
    (
      write__8
    )
      ob x.location_packages_installed;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"cost\":";
    (
      write_location_cost
    )
      ob x.location_cost;
    Bi_outbuf.add_char ob '}';
)
let string_of_location ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_location ob x;
  Bi_outbuf.contents ob
let read_location = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : location) =
      {
        location_name = Obj.magic 0.0;
        location_provide_resources = [];
        location_repository = Obj.magic 0.0;
        location_packages_installed = [];
        location_cost = 1;
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
                        4
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
            | 10 -> (
                if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'y' then (
                  2
                )
                else (
                  -1
                )
              )
            | 17 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'r' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | 18 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 's' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = 'a' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'l' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = 'd' then (
                  3
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
                read_location_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read_resources_provided
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            )
          | 2 ->
            let v =
              (
                read_repository_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__8
                ) p lb
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            )
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read_location_cost
                ) p lb
              in
              Obj.set_field (Obj.repr x) 4 (Obj.repr v);
            )
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
                          4
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
              | 10 -> (
                  if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'y' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 17 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'r' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 18 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = 's' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = 'a' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'l' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = 'd' then (
                    3
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
                  read_location_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read_resources_provided
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              )
            | 2 ->
              let v =
                (
                  read_repository_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__8
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              )
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read_location_cost
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 4 (Obj.repr v);
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields [| !bits0 |] [| "name"; "repository" |];
        Ag_oj_run.identity x
      )
)
let location_of_string s =
  read_location (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component_name = (
  Yojson.Safe.write_string
)
let string_of_component_name ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_name ob x;
  Bi_outbuf.contents ob
let read_component_name = (
  Ag_oj_run.read_string
)
let component_name_of_string s =
  read_component_name (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component_stateful : _ -> component_stateful -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      write_component_name
    )
      ob x.component_stateful_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"type\":";
    (
      write_component_type_name
    )
      ob x.component_stateful_type;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"state\":";
    (
      write_state_name
    )
      ob x.component_stateful_state;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"location\":";
    (
      write_location_name
    )
      ob x.component_stateful_location;
    Bi_outbuf.add_char ob '}';
)
let string_of_component_stateful ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_stateful ob x;
  Bi_outbuf.contents ob
let read_component_stateful = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : component_stateful) =
      {
        component_stateful_name = Obj.magic 0.0;
        component_stateful_type = Obj.magic 0.0;
        component_stateful_state = Obj.magic 0.0;
        component_stateful_location = Obj.magic 0.0;
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
                  | 'n' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
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
            | 5 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' then (
                  2
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' then (
                  3
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
                read_component_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read_component_type_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 2 ->
            let v =
              (
                read_state_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            bits0 := !bits0 lor 0x4;
          | 3 ->
            let v =
              (
                read_location_name
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
                    | 'n' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
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
              | 5 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' then (
                    3
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
                  read_component_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read_component_type_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 2 ->
              let v =
                (
                  read_state_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              bits0 := !bits0 lor 0x4;
            | 3 ->
              let v =
                (
                  read_location_name
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
        if !bits0 <> 0xf then Ag_oj_run.missing_fields [| !bits0 |] [| "name"; "component_type_workaround"; "state"; "location" |];
        Ag_oj_run.identity x
      )
)
let component_stateful_of_string s =
  read_component_stateful (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component = (
  write_component_stateful
)
let string_of_component ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component ob x;
  Bi_outbuf.contents ob
let read_component = (
  read_component_stateful
)
let component_of_string s =
  read_component (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_binding_hierarchical : _ -> binding_hierarchical -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"port_required\":";
    (
      write_port_name
    )
      ob x.binding_hierarchical_port_required;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"port_provided\":";
    (
      write_port_name
    )
      ob x.binding_hierarchical_port_provided;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"requirer\":";
    (
      write_component_name
    )
      ob x.binding_hierarchical_requirer;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"provider\":";
    (
      write_component_name
    )
      ob x.binding_hierarchical_provider;
    Bi_outbuf.add_char ob '}';
)
let string_of_binding_hierarchical ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_binding_hierarchical ob x;
  Bi_outbuf.contents ob
let read_binding_hierarchical = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : binding_hierarchical) =
      {
        binding_hierarchical_port_required = Obj.magic 0.0;
        binding_hierarchical_port_provided = Obj.magic 0.0;
        binding_hierarchical_requirer = Obj.magic 0.0;
        binding_hierarchical_provider = Obj.magic 0.0;
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
            | 8 -> (
                match String.unsafe_get s pos with
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'r' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | 'r' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'q' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'r' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 13 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' then (
                  match String.unsafe_get s (pos+5) with
                    | 'p' -> (
                        if String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'v' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'd' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'd' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'r' -> (
                        if String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'q' && String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'd' then (
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
                read_port_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read_port_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 2 ->
            let v =
              (
                read_component_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            bits0 := !bits0 lor 0x4;
          | 3 ->
            let v =
              (
                read_component_name
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
              | 8 -> (
                  match String.unsafe_get s pos with
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'r' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | 'r' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'q' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'r' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 13 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' then (
                    match String.unsafe_get s (pos+5) with
                      | 'p' -> (
                          if String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'v' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'd' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'd' then (
                            1
                          )
                          else (
                            -1
                          )
                        )
                      | 'r' -> (
                          if String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'q' && String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'd' then (
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
                  read_port_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read_port_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 2 ->
              let v =
                (
                  read_component_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              bits0 := !bits0 lor 0x4;
            | 3 ->
              let v =
                (
                  read_component_name
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
        if !bits0 <> 0xf then Ag_oj_run.missing_fields [| !bits0 |] [| "port_required"; "port_provided"; "requirer"; "provider" |];
        Ag_oj_run.identity x
      )
)
let binding_hierarchical_of_string s =
  read_binding_hierarchical (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_binding = (
  write_binding_hierarchical
)
let string_of_binding ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_binding ob x;
  Bi_outbuf.contents ob
let read_binding = (
  read_binding_hierarchical
)
let binding_of_string s =
  read_binding (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__18 = (
  Ag_oj_run.write_list (
    write_binding
  )
)
let string_of__18 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__18 ob x;
  Bi_outbuf.contents ob
let read__18 = (
  Ag_oj_run.read_list (
    read_binding
  )
)
let _18_of_string s =
  read__18 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__17 = (
  Ag_oj_run.write_list (
    write_component
  )
)
let string_of__17 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__17 ob x;
  Bi_outbuf.contents ob
let read__17 = (
  Ag_oj_run.read_list (
    read_component
  )
)
let _17_of_string s =
  read__17 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__16 = (
  Ag_oj_run.write_list (
    write_location
  )
)
let string_of__16 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__16 ob x;
  Bi_outbuf.contents ob
let read__16 = (
  Ag_oj_run.read_list (
    read_location
  )
)
let _16_of_string s =
  read__16 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_configuration : _ -> configuration -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"version\":";
    (
      write_version
    )
      ob x.configuration_version;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"locations\":";
    (
      write__16
    )
      ob x.configuration_locations;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"components\":";
    (
      write__17
    )
      ob x.configuration_components;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"bindings\":";
    (
      write__18
    )
      ob x.configuration_bindings;
    Bi_outbuf.add_char ob '}';
)
let string_of_configuration ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_configuration ob x;
  Bi_outbuf.contents ob
let read_configuration = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : configuration) =
      {
        configuration_version = Obj.magic 0.0;
        configuration_locations = [];
        configuration_components = [];
        configuration_bindings = [];
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
                if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'n' then (
                  0
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'd' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 's' then (
                  3
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 's' then (
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
                read_version
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__16
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            )
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__17
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__18
                ) p lb
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            )
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
                  if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'n' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'd' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 's' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 's' then (
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
                  read_version
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__16
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              )
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__17
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__18
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields [| !bits0 |] [| "version" |];
        Ag_oj_run.identity x
      )
)
let configuration_of_string s =
  read_configuration (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component_type_simple : _ -> component_type_simple -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      write_component_type_name
    )
      ob x.component_type_simple_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"provide\":";
    (
      write__1
    )
      ob x.component_type_simple_provide;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"require\":";
    (
      write__2
    )
      ob x.component_type_simple_require;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"conflict\":";
    (
      write__3
    )
      ob x.component_type_simple_conflict;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"consume\":";
    (
      write__6
    )
      ob x.component_type_simple_consume;
    Bi_outbuf.add_char ob '}';
)
let string_of_component_type_simple ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_type_simple ob x;
  Bi_outbuf.contents ob
let read_component_type_simple = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : component_type_simple) =
      {
        component_type_simple_name = Obj.magic 0.0;
        component_type_simple_provide = [];
        component_type_simple_require = [];
        component_type_simple_conflict = [];
        component_type_simple_consume = [];
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
            | 7 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 'r' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'q' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 't' then (
                  3
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
                read_component_type_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__1
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            )
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__2
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__3
                ) p lb
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            )
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                (
                  read__6
                ) p lb
              in
              Obj.set_field (Obj.repr x) 4 (Obj.repr v);
            )
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
              | 7 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'e' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'r' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'q' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 't' then (
                    3
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
                  read_component_type_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__1
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              )
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__2
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__3
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              )
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  (
                    read__6
                  ) p lb
                in
                Obj.set_field (Obj.repr x) 4 (Obj.repr v);
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields [| !bits0 |] [| "name" |];
        Ag_oj_run.identity x
      )
)
let component_type_simple_of_string s =
  read_component_type_simple (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_component_simple : _ -> component_simple -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      write_component_name
    )
      ob x.component_simple_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"type\":";
    (
      write_component_type_name
    )
      ob x.component_simple_type;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"location\":";
    (
      write_location_name
    )
      ob x.component_simple_location;
    Bi_outbuf.add_char ob '}';
)
let string_of_component_simple ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_component_simple ob x;
  Bi_outbuf.contents ob
let read_component_simple = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : component_simple) =
      {
        component_simple_name = Obj.magic 0.0;
        component_simple_type = Obj.magic 0.0;
        component_simple_location = Obj.magic 0.0;
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
                  | 'n' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
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
            | 8 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' then (
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
                read_component_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read_component_type_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 2 ->
            let v =
              (
                read_location_name
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
                  match String.unsafe_get s pos with
                    | 'n' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
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
              | 8 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' then (
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
                  read_component_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read_component_type_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 2 ->
              let v =
                (
                  read_location_name
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
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields [| !bits0 |] [| "name"; "component_type_workaround"; "location" |];
        Ag_oj_run.identity x
      )
)
let component_simple_of_string s =
  read_component_simple (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_binding_simple : _ -> binding_simple -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"port\":";
    (
      write_port_name
    )
      ob x.binding_simple_port;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"requirer\":";
    (
      write_component_name
    )
      ob x.binding_simple_requirer;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"provider\":";
    (
      write_component_name
    )
      ob x.binding_simple_provider;
    Bi_outbuf.add_char ob '}';
)
let string_of_binding_simple ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_binding_simple ob x;
  Bi_outbuf.contents ob
let read_binding_simple = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let (x : binding_simple) =
      {
        binding_simple_port = Obj.magic 0.0;
        binding_simple_requirer = Obj.magic 0.0;
        binding_simple_provider = Obj.magic 0.0;
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
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' then (
                  0
                )
                else (
                  -1
                )
              )
            | 8 -> (
                match String.unsafe_get s pos with
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'r' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | 'r' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'q' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'r' then (
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
                read_port_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read_component_name
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 2 ->
            let v =
              (
                read_component_name
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
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  match String.unsafe_get s pos with
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'r' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 'r' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'q' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'r' then (
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
                  read_port_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read_component_name
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 2 ->
              let v =
                (
                  read_component_name
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
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields [| !bits0 |] [| "port"; "requirer"; "provider" |];
        Ag_oj_run.identity x
      )
)
let binding_simple_of_string s =
  read_binding_simple (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
