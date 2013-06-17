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


(* Depends on 
 - Sys (from ocaml standard library)
 - Lexing (from ocaml standard library)
 - input/Settings_parser
 - input/Settings_lexer
*)


(* for now, keep the same semantics as before *)
(* TODO: change everything *)

let usage =  "usage "
    ^ Sys.argv.(0)
    ^ "settings-file"
    ^ "[ssettings-file]*"


let load_settings _ = for i=1 to (Array.length Sys.argv) - 1 do
    let filename =  Sys.argv.(i) in
    let file = Pervasives.open_in filename in
      Settings_parser.main Settings_lexer.token (Lexing.from_channel file)
  done

