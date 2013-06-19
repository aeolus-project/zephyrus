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
(*    along with Zephyrus.  If not, see <http=//www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)

(* Depends on
    - Pervasives (standard library: files, opening and closing)
    - input/Settings (for configuration)
    - output/Zephyrus_log (for logging)
*)


val input_open : string option ref -> (string * in_channel) option

val parse_json : (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) -> string option ref -> 'a option

val parse_standard : ((Lexing.lexbuf -> 'b) -> Lexing.lexbuf -> 'a) -> (Lexing.lexbuf -> 'b) -> string option ref -> 'a option


