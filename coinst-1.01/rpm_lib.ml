(*
References
----------
http://rpm.org/
rpm sources
*)

(*
XXXX Validator (check that the fields all have the right type)
XXXX Check results
XXXX Print generated rules
XXXX Share more code with deb.ml
*)

let rec match_prefix_rec p d i l =
  i = l || (d.[i] = p.[i] && match_prefix_rec p d (i + 1) l)
let match_prefix p d =
  let l = String.length p in
  String.length d >= l && match_prefix_rec p d 0 l

(* Mandriva has a patch that makes rpm ignore conflict on some
   documentation files *)
let doc_dirs =
  ["/usr/share/man/"; "/usr/share/gtk-doc/html/"; "/usr/share/gnome/html/"]
let keep_directory d =
  not (List.exists (fun p -> match_prefix p d) doc_dirs)

type typ =
    NULL | CHAR | INT8 | INT16 | INT32 | INT64
  | STRING | BIN | STRING_ARRAY | I18NSTRING
  | UNKOWNTYPE of int

let intern_typ i =
  match i with
    0 -> NULL  | 1 -> CHAR   | 2 -> INT8 | 3 -> INT16        | 4 -> INT32
  | 5 -> INT64 | 6 -> STRING | 7 -> BIN  | 8 -> STRING_ARRAY | 9 -> I18NSTRING
  | _ -> Util.print_warning (Format.sprintf "unknown type %d" i);
         UNKOWNTYPE i

let substring ch l =
 let s = String.create l in
 really_input ch s 0 l;
 s

let int ch =
  let s = substring ch 4 in
  Char.code s.[0] lsl 24 + Char.code s.[1] lsl 16 +
  Char.code s.[2] lsl 8 + Char.code s.[3]

let sstring store pos =
  let len = ref 0 in
  while store.[pos + !len] <> '\000' do incr len done;
  String.sub store pos !len

let rec sstring_array_rec store pos count =
  if count = 0 then [] else
  let s = sstring store pos in
  s :: sstring_array_rec store (pos + String.length s + 1) (count - 1)

let sstring_array store pos count =
  Array.of_list (sstring_array_rec store pos count)

let rec sarray_rec l f store pos count =
  if count = 0 then [] else
  let s = f store pos in
  s :: sarray_rec l f store (pos + l) (count - 1)

let sarray l f store pos count = Array.of_list (sarray_rec l f store pos count)

let sint32 s pos =
  Char.code s.[pos] lsl 24 + Char.code s.[pos + 1] lsl 16 +
  Char.code s.[pos + 2] lsl 8 + Char.code s.[pos + 3]

let sint32_array = sarray 4 sint32

let sint16 s pos = Char.code s.[pos] lsl 8 + Char.code s.[pos + 1]

let sint16_array = sarray 2 sint16

(****)

let get_package_list' h n =
  try
    Hashtbl.find h n
  with Not_found ->
    let r = ref [] in
    Hashtbl.add h n r;
    r

let add_to_package_list h n p =
  let l = get_package_list' h n in
  l := p :: !l

let get_package_list h n = try !(Hashtbl.find h n) with Not_found -> []

(****)

let pr_typ ch t =
  Format.fprintf ch "%s"
    (match t with
       NULL -> "NULL" | CHAR -> "CHAR" | INT8 -> "INT8" | INT16 -> "INT16"
     | INT32 -> "INT32" | INT64 -> "INT64" | STRING -> "STRING" | BIN -> "BIN"
     | STRING_ARRAY -> "STRING_ARRAY" | I18NSTRING -> "I18NSTRING"
     | UNKOWNTYPE i -> "UNKOWNTYPE(" ^ string_of_int i ^ ")")

let tags =
  [(  63, (BIN,          "HEADERIMMUTABLE", false));
   ( 100, (STRING_ARRAY, "HEADERI18NTABLE", false));
   ( 257, (INT32,        "SIGSIZE", false));
   ( 261, (BIN,          "SIGMD5", false));
   ( 262, (BIN,          "SIGGPG", false));
   ( 266, (STRING_ARRAY, "PUBKEYS", false));
   ( 267, (BIN,          "DSAHEADER", false));
   ( 269, (STRING,       "SHA1HEADER", false));
   (1000, (STRING,       "NAME", true));             (* ! *)
   (1001, (STRING,       "VERSION", true));          (* ! *)
   (1002, (STRING,       "RELEASE", true));          (* ! *)
   (1003, (INT32,        "EPOCH", true));            (* ! *)
   (1004, (I18NSTRING,   "SUMMARY", false));
   (1005, (I18NSTRING,   "DESCRIPTION", false));
   (1006, (INT32,        "BUILDTIME", false));
   (1007, (STRING,       "BUILDHOST", false));
   (1009, (INT32,        "SIZE", false));
   (1010, (STRING,       "DISTRIBUTION", false));
   (1011, (STRING,       "VENDOR", false));
   (1012, (BIN,          "GIF", false));
   (1013, (BIN,          "XPM", false));
   (1014, (STRING,       "LICENSE", false));
   (1015, (STRING,       "PACKAGER", false));
   (1016, (I18NSTRING,   "GROUP", false));
   (1020, (STRING,       "URL", false));
   (1021, (STRING,       "OS", false));
   (1022, (STRING,       "ARCH", false));
   (1023, (STRING,       "PREIN", false));
   (1024, (STRING,       "POSTIN", false));
   (1025, (STRING,       "PREUN", false));
   (1026, (STRING,       "POSTUN", false));
   (1028, (INT32,        "FILESIZES", false));
   (1030, (INT16,        "FILEMODES", true));        (* ! *)
   (1033, (INT16,        "FILERDEVS", false));
   (1034, (INT32,        "FILEMTIMES", false));
   (1035, (STRING_ARRAY, "FILEMD5S", true));         (* ! *)
   (1036, (STRING_ARRAY, "FILELINKTOS", true));      (* ! *)
   (1037, (INT32,        "FILEFLAGS", true));        (* ! *)
   (1039, (STRING_ARRAY, "FILEUSERNAME", false));
   (1040, (STRING_ARRAY, "FILEGROUPNAME", false));
   (1044, (STRING,       "SOURCERPM", false));
   (1045, (INT32,        "FILEVERIFYFLAGS", false));
   (1046, (INT32,        "ARCHIVESIZE", false));
   (1047, (STRING_ARRAY, "PROVIDENAME", true));      (* ! *)
   (1048, (INT32,        "REQUIREFLAGS", true));     (* ! *)
   (1049, (STRING_ARRAY, "REQUIRENAME", true));      (* ! *)
   (1050, (STRING_ARRAY, "REQUIREVERSION", true));   (* ! *)
   (1053, (INT32,        "CONFLICTFLAGS", true));    (* ! *)
   (1054, (STRING_ARRAY, "CONFLICTNAME", true));     (* ! *)
   (1055, (STRING_ARRAY, "CONFLICTVERSION", true));  (* ! *)
   (1064, (STRING,       "RPMVERSION", false));
   (1065, (STRING_ARRAY, "TRIGGERSCRIPTS", false));
   (1066, (STRING_ARRAY, "TRIGGERNAME", false));
   (1067, (STRING_ARRAY, "TRIGGERVERSION", false));
   (1068, (INT32,        "TRIGGERFLAGS", false));
   (1069, (INT32,        "TRIGGERINDEX", false));
   (1079, (STRING,       "VERIFYSCRIPT", false));
   (1080, (INT32,        "CHANGELOGTIME", false));
   (1081, (STRING_ARRAY, "CHANGELOGNAME", false));
   (1082, (STRING_ARRAY, "CHANGELOGTEXT", false));
   (1085, (STRING,       "PREINPROG", false));
   (1086, (STRING,       "POSTINPROG", false));
(*   (1087, (STRING,       "PREUNPROG", false));*)
   (1087, (STRING_ARRAY, "PREUNPROG", false));
   (1088, (STRING,       "POSTUNPROG", false));
   (1090, (STRING_ARRAY, "OBSOLETENAME", false));
   (1091, (STRING,       "VERIFYSCRIPTPROG", false));
   (1092, (STRING_ARRAY, "TRIGGERSCRIPTPROG", false));
   (1094, (STRING,       "COOKIE", false));
   (1095, (INT32,        "FILEDEVICES", false));
   (1096, (INT32,        "FILEINODES", false));
   (1097, (STRING_ARRAY, "FILELANGS", false));
   (1098, (STRING_ARRAY, "PREFIXES", false));        (* ? *)
   (1112, (INT32,        "PROVIDEFLAGS", true));     (* ! *)
   (1113, (STRING_ARRAY, "PROVIDEVERSION", true));   (* ! *)
   (1114, (INT32,        "OBSOLETEFLAGS", false));
   (1115, (STRING_ARRAY, "OBSOLETEVERSION", false));
   (1116, (INT32,        "DIRINDEXES", true));       (* ! *)
   (1117, (STRING_ARRAY, "BASENAMES", true));        (* ! *)
   (1118, (STRING_ARRAY, "DIRNAMES", true));         (* ! *)
   (1122, (STRING,       "OPTFLAGS", false));
   (1123, (STRING,       "DISTURL", false));
   (1124, (STRING,       "PAYLOADFORMAT", false));
   (1125, (STRING,       "PAYLOADCOMPRESSOR", false));
   (1126, (STRING,       "PAYLOADFLAGS", false));
   (1131, (STRING,       "RHNPLATFORM", false));
   (1132, (STRING,       "PLATFORM", false));
   (1140, (INT32,        "FILECOLORS", false));      (* ? *)
                         (* 1 = elf32, 2 = elf64, 0 = other *)
   (1141, (INT32,        "FILECLASS", false));
   (1142, (STRING_ARRAY, "CLASSDICT", false));
   (1143, (INT32,        "FILEDEPENDSX", false));    (* ? *)
   (1144, (INT32,        "FILEDEPENDSN", false));    (* ? *)
   (1145, (INT32,        "DEPENDSDICT", false));     (* ? *)
   (1146, (BIN,          "SOURCEPKGID", false));
   (1152, (STRING,       "POSTTRANS", false));
   (1154, (STRING,       "POSTTRANSPROG", false));
   (1155, (STRING,       "DISTTAG", false));
   (1156, (STRING_ARRAY, "SUGGESTSNAME", false));
   (1157, (STRING_ARRAY, "SUGGESTSVERSION", false));
   (1158, (INT32,        "SUGGESTSFLAGS", false));
   (1177, (INT32,        "FILEDIGESTALGOS", false));
   (1199, (INT32,        "RPMLIBVERSION", false));
   (1200, (INT32,        "RPMLIBTIMESTAMP", false));
   (1201, (INT32,        "RPMLIBVENDOR", false));
   (1218, (STRING,       "DISTEPOCH", true));
   (5012, (STRING,       "BUGURL", false));
   (1000000, (STRING,    "FILENAME", false));
   (1000001, (INT32,     "FILESIZE", false));
   (1000005, (STRING,    "MD5", false));
   (1000010, (STRING,    "DIRECTORY", false))]

let tag_name tag typ =
  try
    List.assoc tag tags
  with Not_found ->
    Util.print_warning (Format.sprintf "unknown tag %d" tag);
    (typ, Format.sprintf "UNKNOWN(%d)" tag, true)

let pr_tag ch tag =
  let (_, nm, _) = tag_name tag BIN (* Dummy type*) in
  Format.fprintf ch "%s" nm

let pr_field_contents ch (store, (_, typ, pos, count)) =
  match typ with
    STRING ->
      Format.fprintf ch "\"%s\"" (String.escaped (sstring store pos))
  | STRING_ARRAY ->
      Array.iter (fun s -> Format.fprintf ch "\"%s\" " (String.escaped s))
        (sstring_array store pos count)
  | INT32 ->
      Array.iter
        (fun i -> Format.fprintf ch "0x%x " i) (sint32_array store pos count)
  | INT16 ->
      Array.iter
        (fun i -> Format.fprintf ch "0x%x " i) (sint16_array store pos count)
  | _ ->
      Format.fprintf ch "(not shown)"

let show_all = ref false

let pr_field ch ((store, (tag, typ, pos, count)) as field) =
  let (typ', nm, shown) = tag_name tag typ in
  if typ <> typ' then
    Util.print_warning (Format.sprintf "wrong type for tag %s" nm);
  if shown || !show_all then begin
    Format.fprintf ch "%s %a 0x%x %d" nm pr_typ typ pos count;
    Format.fprintf ch "  %a@." pr_field_contents field
  end

let pr_fields store entry =
  for i = 0 to Array.length entry - 1 do
    let (tag, typ, pos, count) as field = entry.(i) in
    Format.printf "%a" pr_field (store, field)
  done;
  Format.printf "@."

(****)

let _NAME = 1000
let _VERSION = 1001
let _RELEASE = 1002
let _EPOCH = 1003
let _FILEMODES = 1030
let _FILEMD5S = 1035
let _FILELINKTOS = 1036
let _FILEFLAGS = 1037
let _PROVIDENAME = 1047
let _REQUIREFLAGS = 1048
let _REQUIRENAME = 1049
let _REQUIREVERSION = 1050
let _CONFLICTFLAGS = 1053
let _CONFLICTNAME = 1054
let _CONFLICTVERSION = 1055
let _OBSOLETENAME = 1090
let _PROVIDEFLAGS = 1112
let _PROVIDEVERSION = 1113
let _OBSOLETEFLAGS = 1114
let _OBSOLETENVERSION = 1115
let _DIRINDEXES = 1116
let _BASENAMES = 1117
let _DIRNAMES = 1118
let _DISTEPOCH = 1218

let etag entry i = let (tag, _, _, _) = entry.(i) in tag

let rec move_to entry i tag =
  if etag entry i >= tag then i else move_to entry (i + 1) tag

exception Skip

let check_entry tag typ tag' typ' =
  if tag <> tag' then begin
    let b = Buffer.create 80 in
    Format.bprintf b "Expected tag %a but actual tag is %a@?"
      pr_tag tag pr_tag tag';
    Util.print_warning (Buffer.contents b);
    raise Skip
  end;
  if typ <> typ' then begin
    let b = Buffer.create 80 in
    Format.bprintf b "Entry %a has expected type %a but actual typ is %a@?"
      pr_tag tag pr_typ typ pr_typ typ';
    Util.fail (Buffer.contents b)
  end

let estring store entry i tag =
  let (tag', typ, pos, count) = entry.(i) in
  check_entry tag STRING tag' typ;
  if count <> 1 then begin
    let b = Buffer.create 80 in
    Format.bprintf b "Entry %a has type STRING with count %d > 1@?"
      pr_tag tag count;
    Util.fail (Buffer.contents b)
  end;
  sstring store pos

let estring_array store entry i tag =
  let (tag', typ, pos, count) = entry.(i) in
  check_entry tag STRING_ARRAY tag' typ;
  sstring_array store pos count

let eint32 store entry i tag =
  let (tag', typ, pos, count) = entry.(i) in
  check_entry tag INT32 tag' typ;
  if count <> 1 then begin
    let b = Buffer.create 80 in
    Format.bprintf b "Expecting a single INT32 for entry %a but got %d@?"
      pr_tag tag count;
    Util.fail (Buffer.contents b)
  end;
  sint32 store pos

let eint32_array store entry i tag =
  let (tag', typ, pos, count) = entry.(i) in
  check_entry tag INT32 tag' typ;
  sint32_array store pos count

let eint16_array store entry i tag =
  let (tag', typ, pos, count) = entry.(i) in
  check_entry tag INT16 tag' typ;
  sint16_array store pos count

(****)

type file_info =
  Dir | Char | Block | Link of string | Sock | Pipe | Reg of string

let intern_file filemodes filemd5s filelinktos i =
  let mode = filemodes.(i) in
  match mode land 0o170000 with
    0o40000  -> Dir
  | 0o20000  -> Char
  | 0o60000  -> Block
  | 0o120000 -> Link (filelinktos.(i))
  | 0o140000 -> Sock
  | 0o10000  -> Pipe
  | 0o100000 -> Reg (filemd5s.(i))
  | _        -> Util.fail (Format.sprintf "unknown mode %o" mode)

let pr_info ch i =
  match i with
    Dir ->
      Format.fprintf ch "DIR"
  | Char ->
      Format.fprintf ch "CHAR"
  | Block ->
      Format.fprintf ch "BLOCK"
  | Link l ->
      Format.fprintf ch "LINK(%s)" l
  | Sock ->
      Format.fprintf ch "SOCK"
  | Pipe ->
      Format.fprintf ch "PIPE"
  | Reg s ->
      Format.fprintf ch "REG(%s)" s


(****)

type rel = SE | E | EQ | L | SL | ALL

let pr_rel ch rel =
  Format.fprintf ch "%s"
    (match rel with
       SE -> "<<"
     | E  -> "<="
     | EQ -> "="
     | L  -> ">="
     | SL -> ">>"
     | ALL -> "ALL")

let intern_flags f =
  match f land 15 with
     0 -> ALL
  |  2 -> SE
  | 10 -> E
  |  8 -> EQ
  | 12 -> L
  |  4 -> SL
  |  _ -> Util.fail (Format.sprintf "Wrong flag %d" (f land 15))

(* RPMSENSE_RPMLIB | RPMSENSE_MISSINGOK *)
let requires_to_skip_bitmask = (1 lsl 24) lor (1 lsl 19)

(* Dependencies on rpmlib and "suggests" dependencies are skipped *)
let skipped_dep name flags i =
  flags.(i) land requires_to_skip_bitmask <> 0 ||
  let nm = name.(i) in
  (String.length nm > 8 &&
   nm.[0] = 'r' && nm.[1] = 'p' && nm.[2] = 'm' && nm.[3] = 'l' &&
   nm.[4] = 'i' && nm.[5] = 'b' && nm.[6] = '(')

type vers = int option * string * string option * string option
type pack_ref = string * rel * vers option
type p =
  { num : int;
    name : string; version : string; release : string;
    epoch : int option; distepoch : string option;
    provide : pack_ref list;
    require : pack_ref list;
    conflict : pack_ref list }

type pool =
  { mutable size : int;
    files : (string * string, (file_info * p) list ref) Hashtbl.t;
    provides : (string, (rel * vers option * p) list ref) Hashtbl.t;
    mutable packages : p list;
    packages_by_num : (int, p) Hashtbl.t;
    packages_by_name : (string, p list ref) Hashtbl.t;
    }

let new_pool () =
  { size = 0;
    files = Hashtbl.create 300000;
    provides = Hashtbl.create 10000;
    packages = [];
    packages_by_num = Hashtbl.create 1000;
    packages_by_name = Hashtbl.create 1000 }

let add_file p f v =
  let l =
    try Hashtbl.find p.files f with Not_found ->
    let l = ref [] in Hashtbl.add p.files f l; l
  in
  l := v :: !l

let add_provide pool p (nm, rel, vers) =
  let l =
    try Hashtbl.find pool.provides nm with Not_found ->
    let l = ref [] in Hashtbl.add pool.provides nm l; l
  in
  l := (rel, vers, p) :: !l

(****)

let pr_version ch (epoch, version, release, distepoch) =
  begin match epoch with
    None   -> ()
  | Some e -> Format.fprintf ch "%d:" e
  end;
  Format.fprintf ch "%s" version;
  begin match release with
    Some r -> Format.fprintf ch "-%s" r
  | None   -> ()
  end;
  begin match distepoch with
    Some e -> Format.fprintf ch ":%s" e
  | None   -> ()
  end

let is_lower c = c >= 'a' && c <= 'z'
let is_upper c = c >= 'A' && c <= 'Z'
let is_digit c = c >= '0' && c <= '9'
let is_alpha c = is_lower c || is_upper c
let is_alnum c = is_alpha c || is_digit c


let check_version s = s <> "" && not (is_alnum s.[String.length s - 1])

(* _evr_tuple_match, rpmevr.c *)
let version_re_1 =
  Str.regexp
  "^\\(\\([0-9]*\\):\\)?\\([^:]+\\)\\(-\\([^:-]+\\)\\)\\(:\\([^:-]+\\)\\)?$"
let version_re_2 =
  Str.regexp
  "^\\(\\([0-9]*\\):\\)?\\([^:-]+\\)\\(-\\([^:-]+\\)\\)?\\(:\\([^:-]+\\)\\)?$"

let parse_version s =
  if s = "" then
    None
  else if not (Str.string_match version_re_1 s 0 ||
               Str.string_match version_re_2 s 0) then
    failwith ("Bad version " ^ s)
  else begin
    let epoch =
      try
        let s = Str.matched_group 2 s in
        Some (if s = "" then 0 else int_of_string s)
      with Not_found ->
        None
    in
    let version = Str.matched_group 3 s in
    let release = try Some (Str.matched_group 5 s) with Not_found -> None in
    let distepoch = try Some (Str.matched_group 7 s) with Not_found -> None in
    if
      check_version s ||
      match release with Some r -> check_version r | _ -> false
    then begin
      let b = Buffer.create 80 in
      Format.bprintf b
        "version '%a' not ending with an alphanumeric character@?"
        pr_version (epoch, version, release, distepoch);
      Util.print_warning (Buffer.contents b)
    end;
    Some (epoch, version, release, distepoch)
  end

let rec split_vers_rec s p l =
  let q = ref p in
  while !q < l && not (is_alnum s.[!q]) do incr q done;
  if !q = l then begin
    if p = !q then [] else [`Other]
  end else begin
    let p = !q in
    if is_digit s.[p] then begin
      let q = ref p in
      while !q < l && s.[!q] = '0' do incr q done;
      let p = !q in
      while !q < l && is_digit s.[!q] do incr q done;
      `Num (String.sub s p (!q - p)) :: split_vers_rec s !q l
    end else (* if is_alpha s.[p] then*) begin
      let q = ref (p + 1) in
      while !q < l && is_alpha s.[!q] do incr q done;
      `Alpha (String.sub s p (!q - p)) :: split_vers_rec s !q l
    end
  end

let split_vers s = split_vers_rec s 0 (String.length s)

let rec compare_vers_rec l1 l2 =
  match l1, l2 with
    `Alpha s1 :: r1, `Alpha s2 :: r2 ->
       let c = compare s1 s2 in
       if c <> 0 then c else
       compare_vers_rec r1 r2
  | `Num n1 :: r1, `Num n2 :: r2 ->
       let c = compare (String.length n1) (String.length n2) in
       if c <> 0 then c else
       let c = compare n1 n2 in
       if c <> 0 then c else
       compare_vers_rec r1 r2
  | `Num _ :: _, `Alpha _ :: _
  | `Num _ :: _, `Other :: _ ->
       1
  | `Alpha _ :: _, `Num _ :: _
  | `Alpha _ :: _, `Other :: _ ->
       -1
  | `Other :: _, `Alpha _ :: _
  | `Other :: _, `Num _ :: _      (* Should have been 1 *)
  | `Other :: _, `Other :: _ ->   (* Should have been 0 *)
       -1
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1

(*
Not stable by extension
    10 < 10a
    10a.5 < 10.5
Not a total order
    10. < 10,      10, < 10.
    10.a < 10.     10. < 10.a   (but   10.1 > 10.     10. < 10.1)
*)

(*rpmvercmp.c*)
let compare_vers s1 s2 =
  if s1 = s2 then 0 else
  compare_vers_rec (split_vers s1) (split_vers s2)

let promote = false

let compare_versions ver1 ver2 rel2 =
  match ver1, ver2 with
    Some (e1, v1, r1, d1), Some (e2, v2, r2, d2) ->
      let c2 =
        let c = compare_vers v1 v2 in
        if c <> 0 then c else
        let c =
          match r1, r2, rel2 with
            Some r1, Some r2, _        -> compare_vers r1 r2
          | _,       None,    (E|EQ|L) -> 0
          | None,    Some r2, _        -> compare_vers "" r2
          | Some r1, None,    _        -> compare_vers r1 ""
          | None,    None,    _        -> 0
        in
        if c <> 0 then c else
        match d1, d2, rel2 with
          Some d1, Some d2, _        -> compare_vers d1 d2
        | _,       None,    (EQ|L)   -> 0
        | None,    Some d2, _        -> compare_vers "" d2
        | Some d1, None,    _        -> compare_vers d1 ""
        | None,    None,    _        -> 0
      in
      let c1 =
        match e1, e2 with
          None, None | None, Some 0 | Some 0, None ->
            0
        | Some e1, Some e2 ->
            compare (e1 : int) e2
        | None, Some _ ->
            -1
        | Some _, None ->
            if promote then 0 else 1
      in
      if c1 <> 0 then c1 else c2
  | _ ->
      (* Checked in function validate_deps *)
      assert false

(* rpmdsCompare, rpmds.c *)
let compare_pack_refs (n1, r1, v1) (n2, r2, v2) =
  n1 = n2 &&
  match r1, r2 with
    ALL, _ | _, ALL | (SE | E), (SE | E) | (SL | L), (SL | L) -> true
  | (EQ | L | SL), SE | SL, (EQ | E) -> compare_versions v1 v2 r2 < 0
  | SE, (EQ | L | SL) | (EQ | E), SL -> compare_versions v1 v2 r2 > 0
  | EQ, E | L, E | L, EQ -> compare_versions v1 v2 r2 <= 0
  | E, EQ | E, L | EQ, L -> compare_versions v1 v2 r2 >= 0
  | EQ, EQ -> compare_versions v1 v2 r2 = 0

let pr_pack_ref ch (name, rel, ver) =
  Format.fprintf ch "%s" name;
  match ver with
    Some v -> Format.fprintf ch " (%a %a)" pr_rel rel pr_version v
  | None   -> ()

let pr_pack ch p =
  pr_pack_ref ch
    (p.name, EQ, Some (p.epoch, p.version, Some p.release, p.distepoch))

let resolve_file_dep p (nm, rel, ver) =
  if nm = "" || nm.[0] <> '/' then [] else begin
    let i = String.rindex nm '/' in
    let d = String.sub nm 0 (i + 1) in
    let f = String.sub nm (i + 1) (String.length nm - i - 1) in
    let l = try !(Hashtbl.find p.files (d, f)) with Not_found -> [] in
    List.map (fun (_, p) -> p) l
  end

let resolve_pack_ref p ((nm, rel, ver) as rf) =
  let l =
    try
      let l = !(Hashtbl.find p.provides nm) in
      List.filter
        (fun (rel, vers, p) ->
           (* The order here is important: the comparison is not symmetric! *)
          compare_pack_refs (nm, rel, vers) rf) l
    with Not_found ->
      []
  in
  resolve_file_dep p rf @ List.map (fun (rel, vers, p) -> p) l

let validate_deps l =
  List.iter
    (fun (nm, rel, ver) ->
       match rel, ver with
         ALL, Some _ -> assert false
       | ALL, None   -> ()
       |   _, None   -> assert false
       |   _, Some _ -> ())
    l

let parse_deps name flags version =
  let l = ref [] in
  for i = Array.length name - 1 downto 0 do
    if not (skipped_dep name flags i) then begin
      l := (name.(i), intern_flags flags.(i), parse_version version.(i)) :: !l
    end
  done;
  validate_deps !l;
  !l

let dump_fields = ref false

let parse_header pool ignored_packages ch =
  let h = substring ch 8 in
  if not (h.[0] = '\142' && h.[1] = '\173' && h.[2] = '\232') then
    Util.fail "Bad header";
  let entry_count = int ch in
  let sz = int ch in
(*Format.eprintf "%d %d@." entry_count sz;*)
  let entry = Array.make entry_count (0, NULL, 0, 0) in
  for i = 0 to entry_count - 1 do
    let tag = int ch in
    let typ = intern_typ (int ch) in
    let pos = int ch in
    let count = int ch in
(*    Format.eprintf "%d %a@." tag pr_typ typ;*)
    entry.(i) <- (tag, typ, pos, count)
  done;
  Array.sort (fun (tag1, _, _, _) (tag2, _, _, _) -> compare tag1 tag2) entry;
  let store = substring ch sz in
  try
    let i = move_to entry 0 _NAME in
    let name = estring store entry i _NAME in
    let version = estring store entry (i + 1) _VERSION in
    let release = estring store entry (i + 2) _RELEASE in
    assert (version <> ""); assert (release <> "");
    let epoch =
      if etag entry (i + 3) <> _EPOCH then None else
      Some (eint32 store entry (i + 3) _EPOCH)
    in
    Util.set_warning_location
      (match epoch with
         None   ->
           Format.sprintf "in package %s = %s-%s" name version release
       | Some e ->
           Format.sprintf "in package %s = %d:%s-%s" name e version release);
    if !dump_fields then pr_fields store entry;
    let i = move_to entry i _FILEMODES in
    let file_info = etag entry i = _FILEMODES in
    let filemodes =
      if file_info then eint16_array store entry i _FILEMODES else [||] in
    let i = move_to entry i _FILEMD5S in
    let filemd5s =
      if file_info then estring_array store entry i _FILEMD5S else [||] in
    let filelinktos =
      if file_info then estring_array store entry (i + 1) _FILELINKTOS else [||]
    in
    let fileflags =
      if file_info then eint32_array store entry (i + 2) _FILEFLAGS else [||]
    in
    let i = move_to entry i _PROVIDENAME in
    let providename = estring_array store entry i _PROVIDENAME in
    let requireflags = eint32_array store entry (i + 1) _REQUIREFLAGS in
    let requirename = estring_array store entry (i + 2) _REQUIRENAME in
    let requireversion = estring_array store entry (i + 3) _REQUIREVERSION in
    let i = move_to entry i _CONFLICTFLAGS in
    let has_confl = etag entry i = _CONFLICTFLAGS in
    let conflictflags =
      if has_confl then eint32_array store entry i _CONFLICTFLAGS else [||] in
    let conflictname =
      if has_confl then estring_array store entry (i + 1) _CONFLICTNAME else [||]
    in
    let conflictversion =
      if has_confl then estring_array store entry (i + 2) _CONFLICTVERSION
      else [||]
    in
    let i = move_to entry i _PROVIDEFLAGS in
    let provideflags = eint32_array store entry i _PROVIDEFLAGS in
    let provideversion = estring_array store entry (i + 1) _PROVIDEVERSION in
    let i = move_to entry i _DIRINDEXES in
    let non_empty = etag entry i = _DIRINDEXES in
    let dirindexes =
      if non_empty then eint32_array store entry i _DIRINDEXES else [||] in
    let basenames =
      if non_empty then estring_array store entry (i + 1) _BASENAMES else [||] in
    let dirnames =
      if non_empty then estring_array store entry (i + 2) _DIRNAMES else [||] in
    let i = move_to entry i _DISTEPOCH in
    let distepoch =
      if etag entry i <> _DISTEPOCH then None else
      Some (estring store entry i _DISTEPOCH)
    in

    if List.mem name ignored_packages then raise Skip;

    let p =
      { num = pool.size;
        name = name; version = version; release = release;
        epoch = epoch; distepoch = distepoch;
        provide = parse_deps providename provideflags provideversion;
        require = parse_deps requirename requireflags requireversion;
        conflict = parse_deps conflictname conflictflags conflictversion }
    in
    pool.packages <- p :: pool.packages;
    Hashtbl.add pool.packages_by_num pool.size p;
    add_to_package_list pool.packages_by_name p.name p;
    List.iter (fun pr -> add_provide pool p pr) p.provide;
    pool.size <- pool.size + 1;
    if file_info then begin
      Array.iteri
        (fun i f ->
           let d = dirnames.(dirindexes.(i)) in
           let is_ghost = fileflags.(i) land 0x40 <> 0 in
           if not is_ghost && keep_directory d then
             add_file pool (d, f)
               (intern_file filemodes filemd5s filelinktos i, p))
        basenames
    end else
      Array.iteri
        (fun i f ->
           let d = dirnames.(dirindexes.(i)) in
           add_file pool (d, f) (Dir (* Dummy value *), p))
        basenames;
    Util.reset_warning_location ()
  with Skip ->
    Util.reset_warning_location ()

let parse_packages pool ignored_packages ch =
  let st = Common.start_parsing (not !dump_fields) ch in
  begin try while true do
    parse_header pool ignored_packages ch;
    Common.parsing_tick st
  done with End_of_file -> () end;
  Common.stop_parsing st

(****)

let package_re = Str.regexp "^\\([^ (]+\\) *( *\\([<=>]+\\) *\\([^ )]+\\) *)$"

let parse_package_dependency pool s =
  if not (Str.string_match package_re s 0) then
    failwith (Format.sprintf "Bad package name '%s'" s);
  let name = Str.matched_group 1 s in
  let (rel, ver) =
    try
      let rel =
        match Str.matched_group 2 s with
          "<<"       -> SE
        | "<=" | "<" -> E
        | "="        -> EQ
        | ">=" | ">" -> L
        | ">>"       -> SL
        | s          -> failwith (Format.sprintf "Bad relation '%s'" s)
      in
      (rel, parse_version (Str.matched_group 3 s))
    with Not_found ->
      (ALL, None)
  in
  let l = resolve_pack_ref pool (name, rel, ver) in
  List.map (fun p -> p.num) l

let parse_package_name pool s =
  List.map (fun p -> p.num) (get_package_list pool.packages_by_name s)

(****)

type conflict_reason =
    R_File of string * string
  | R_Explicit of pack_ref

type reason =
    R_conflict of p * p * conflict_reason
  | R_depends of p * pack_ref

let print_pack p ch n =
  let p = Hashtbl.find p.packages_by_num n in
  Format.fprintf ch "%a" pr_pack p

let print_pack_name p ch n =
  let p = Hashtbl.find p.packages_by_num n in
  Format.fprintf ch "%s" p.name

module Solver = Solver.F (struct type t = reason type reason = t end)

let print_rules = ref false

let add_conflict st p1 p2 reason =
  let p = Solver.lit_of_var p1.num false in
  let p' = Solver.lit_of_var p2.num false in
  Solver.add_rule st [|p; p'|] [R_conflict (p1, p2, reason)]

let add_depend st n l r =
  let l = List.map (fun p -> p.num) l in
  Solver.add_rule st
    (Array.of_list
       (Solver.lit_of_var n.num false ::
        List.map (fun n' -> Solver.lit_of_var n' true) l))
    [R_depends (n, r)];
  match l with
    [] | [_] -> ()
  | _        -> Solver.associate_vars st (Solver.lit_of_var n.num true) l

let add_dependencies pool pr p dep kind =
(*
  if !print_rules then begin
    Format.printf "%d -> any-of (" n;
    List.iter (fun c -> Format.printf " %d" c) l;
    Format.printf ")@."
  end;
*)
  List.iter
    (fun r ->
       let l = resolve_pack_ref pool r in
       match kind with
         `Require ->
            add_depend pr p l r
       | `Conflict ->
            List.iter (fun p' -> add_conflict pr p p' (R_Explicit r)) l)
    dep

let generate_rules pool =
  let st = Common.start_generate (not !print_rules) pool.size in
  let pr = Solver.initialize_problem ~print_var:(print_pack pool) pool.size in
  (* File conflicts *)
  let h = Hashtbl.create 127 in
  Hashtbl.iter
    (fun (d, f) {contents = l} ->
       match l with
         [] | [_] -> ()
       | (inf, _) :: _ ->
          if not (List.for_all (fun (inf', _) -> inf = inf') l) then begin
            let a = Array.of_list l in
            let len = Array.length a in
            for i = 0 to len - 1 do
              for j = i + 1 to len - 1 do
                let (info1, p1) = a.(i) in
                let (info2, p2) = a.(j) in
                let pair = (min p1.num p2.num, max p1.num p2.num) in
                if
                  info1 <> info2 && not (Hashtbl.mem h pair)
                then begin
                  Hashtbl.add h pair ();
                  if !print_rules then begin
                    Format.printf "conflict between %a and %a on file %s%s (%a vs %a).@."
                      pr_pack p1 pr_pack p2 d f pr_info info1 pr_info info2
                  end;
                  add_conflict pr p1 p2 (R_File (d, f))
                end
              done
            done
          end)
    pool.files;
  List.iter
    (fun p ->
       Common.generate_next st;
       add_dependencies pool pr p p.require `Require;
       add_dependencies pool pr p p.conflict `Conflict)
    pool.packages;
  Common.stop_generate st;
  Solver.propagate pr;
  pr

(****)

let rec print_package_list_rec ch l =
  match l with
    []     -> Format.fprintf ch "NOT AVAILABLE"
  | [x]    -> pr_pack ch x
  | x :: r -> Format.fprintf ch "%a, %a" pr_pack x print_package_list_rec r

let print_package_list ch l =
  Format.fprintf ch "{%a}" print_package_list_rec l

let show_reasons pool l =
  if l <> [] then begin
    Format.printf "The following constraints cannot be satisfied:@.";
    List.iter
      (fun r ->
         match r with
           R_conflict (n1, n2, R_Explicit rf) ->
             Format.printf "  %a conflicts with %a {%a}@."
               pr_pack n1 pr_pack_ref rf pr_pack n2
         | R_conflict (n1, n2, R_File (d, f)) ->
             Format.printf "  %a conflicts with %a on file %s%s@."
               pr_pack n1 pr_pack n2 d f
         | R_depends (n, r) ->
             Format.printf "  %a depends on %a %a@."
               pr_pack n pr_pack_ref r
               print_package_list (resolve_pack_ref pool r))
      l
  end

let conflicts_in_reasons rl = List.fold_left (fun cl -> function R_conflict (i,j,r) -> (min i.num j.num, max i.num j.num)::cl | _ -> cl) [] rl

(****)

let compute_conflicts pool =
  let conflict_pairs = Hashtbl.create 1000 in
  let conflicts = Hashtbl.create 1000 in
  let add_conflict p1 p2 =
    let pair = (min p1.num p2.num, max p1.num p2.num) in
    if not (Hashtbl.mem conflict_pairs pair) then begin
      Hashtbl.add conflict_pairs pair ();
      add_to_package_list conflicts p1.num p2.num;
      add_to_package_list conflicts p2.num p1.num
    end
  in
  List.iter
    (fun p ->
       List.iter
         (fun r ->
            let l = resolve_pack_ref pool r in
            List.iter (fun p' -> add_conflict p p') l)
         p.conflict)
    pool.packages;
  let conflict_pairs' = Hashtbl.copy conflict_pairs in
  let has_conflict p1 p2 =
    let pair = (min p1.num p2.num, max p1.num p2.num) in
    Hashtbl.mem conflict_pairs' pair
  in
  (* File conflicts *)
  Hashtbl.iter
    (fun (d, f) {contents = l} ->
       match l with
         [] | [_] -> ()
       | (inf, _) :: _ ->
          if not (List.for_all (fun (inf', _) -> inf = inf') l) then begin
            let a = Array.of_list l in
            let len = Array.length a in
            for i = 0 to len - 1 do
              for j = i + 1 to len - 1 do
                let (info1, p1) = a.(i) in
                let (info2, p2) = a.(j) in
                if not (has_conflict p1 p2) && info1 <> info2 then begin
(*
                    Format.printf "conflict between %a and %a on file %s%s (%a vs %a).@."
                      pr_pack p1 pr_pack p2 d f pr_info info1 pr_info info2;
*)
                  add_conflict p1 p2
                end
              done
            done
          end)
    pool.files;
  Array.init pool.size (fun i -> get_package_list conflicts i)

let compute_deps dist =
  Array.init dist.size (fun i ->
    let p = Hashtbl.find dist.packages_by_num i in
    List.map (fun r -> List.map (fun p -> p.num) (resolve_pack_ref dist r))
      p.require)
(*
    List.map
      (fun l ->
         normalize_set
           (List.flatten
              (List.map (fun p -> resolve_package_dep dist p) l)))
      (p.depends @ p.pre_depends))
*)

(****)

let pool_size p = p.size
