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


let string_of_printing_function printing_function argument =
  let (tmp_file, out_channel) = Filename.open_temp_file "tmp_string_of_" "" in
  printing_function out_channel argument;
  close_out out_channel;
  let in_channel = open_in tmp_file in
  let s = input_line in_channel in
  close_in in_channel;
  Sys.remove tmp_file; 
  s

let lines_of_strings strings = String.concat "\n" strings

let string_of_input_channel (in_channel : in_channel) =
  let in_channel_length = in_channel_length in_channel in
  if in_channel_length > Sys.max_string_length
  then failwith "The input channel contents do not fit in a single string!"
  else
    let s = String.create in_channel_length in
    really_input in_channel s 0 in_channel_length;
    close_in in_channel;
    s