
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