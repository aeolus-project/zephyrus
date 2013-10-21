
let provide_arity x =
  match x with
  | "inf" | "infinite" | "INF" | "INFINITE" | "Infinite" -> true
  | _ -> let _ = int_of_string x in true