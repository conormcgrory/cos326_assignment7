
(* time a function *)
let time f arg = 
  let start = Unix.gettimeofday () in 
  let res = f arg in
  let finish = Unix.gettimeofday () in 
  (res, finish -. start)



(* get number of cores
 * taken from:
 * http://stackoverflow.com/questions/16269393/
 * how-to-get-the-number-of-cores-on-a-machine-with-ocaml
 *)
let cpu_count () = 
  try match Sys.os_type with 
  | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS") 
  | _ ->
      let i = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
      let close () = ignore (Unix.close_process_in i) in
      try Scanf.fscanf i "%d" (fun n -> close (); n) with e -> close (); raise e
  with
  | Not_found | Sys_error _ | Failure _ | Scanf.Scan_failure _ 
  | End_of_file | Unix.Unix_error (_, _, _) -> 2 (* default to 2 *)
