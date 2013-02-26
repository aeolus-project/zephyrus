
type st
val start_parsing : bool -> in_channel -> st
val parsing_tick : st -> unit
val stop_parsing : st -> unit

type st'
val start_generate : bool -> int -> st'
val generate_next : st' -> unit
val stop_generate : st' -> unit
