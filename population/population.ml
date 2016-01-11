
open Sys
open Query

let (|>) v f = f v

let read_args () = 
  let input_file = Sys.argv.(1) in 
  let rows = int_of_string Sys.argv.(2) in 
  let cols = int_of_string Sys.argv.(3) in 
  let l = int_of_string Sys.argv.(4) in 
  let b = int_of_string Sys.argv.(5) in 
  let r = int_of_string Sys.argv.(6) in 
  let t = int_of_string Sys.argv.(7) in 
  (input_file, rows, cols, l, b, r, t)


let population groups (rows,cols) (l,b,r,t) : (int*float) * (int*float) = 
  let (spop,sper) = 
    try 
      let us_area = Query.encompassing_area groups in 
      let query = Query.latlon_of_rowcol us_area (rows,cols) (l,b,r,t) in
      let us_pop = Query.population_search groups us_area in 
      let spop = Query.population_search groups query in 
      let f1 = (float_of_int spop) in 
      let f2 = (float_of_int us_pop) in 
      (spop, (f1 /. f2) *. 100.0)
    with _ -> (0,0.0)
  in

  let (ppop, pper) = 
    try 
      let us_area = Query.encompassing_area groups in 
      let us_pop = Query.population_search groups us_area in 
      let summed_areas = Query.precompute groups us_area (rows,cols) in 
      let ppop = Query.population_lookup summed_areas (l,b,r,t) in

      let f1 = (float_of_int ppop) in 
      let f2 = (float_of_int us_pop) in 
      (ppop, (f1 /. f2) *. 100.0)
    with _ -> (0,0.0)
  in

  ((spop, sper), (ppop,pper))


let main () = 
  if Array.length Sys.argv < 8 then 
    (print_endline "Usage: ./population.native [file] [rows] [cols] [left] [bottom] [right] [top]";
     Pervasives.exit 0)
  else 
    let (input_file, rows, cols, l, b, r, t) = read_args () in
    assert (l >= 1 && r <= cols);
    assert (b >= 1 && t <= rows);
    assert (l <= r && b <= t);
    let groups = (Parse.parse input_file Query.group_of_data) 
                  |> Array.of_list 
                  |> Query.S.seq_of_array in 
    let ((spop,sper),(ppop,pper)) = population groups (rows,cols) (l,b,r,t) in
    Printf.printf "%d,%.1f\n%d,%.1f\n" spop sper ppop pper
;;

main ();;
