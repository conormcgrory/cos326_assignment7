open Str


let parse_line (line: string) (f: int * float * float -> 'a) : 'a = 
  let vals = Str.split (regexp ",") line in 
  let pop = int_of_string (List.nth vals 4) in 
  let lat = float_of_string (List.nth vals 5) in 
  let lon = float_of_string (List.nth vals 6) in 
  f (pop, lat, lon)


let parse (file_name: string) (f: int * float * float -> 'a) : 'a list = 
  let chan = open_in file_name in
  let groups = ref [] in 
  ignore (input_line chan); (* skip first line *)
  try
    while true do
      let line = input_line chan in
      groups := (parse_line line f) :: !groups
    done; !groups
  with End_of_file ->
    close_in chan; !groups
