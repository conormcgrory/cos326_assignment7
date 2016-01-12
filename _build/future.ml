open Marshal
open Unix


(*******************************************************************************
 * Implementation of futures with true parallelism using separate processes
 * Note: copying large data structures between processes can be slow
 *******************************************************************************)

module type S = sig
  type 'a future 
  val force : 'a future -> 'a
  val future : ('a -> 'b) -> 'a -> 'b future
end


module SFuture : S = struct   
  type 'a future = 'a
  let force fut = fut 
  let future f x = f x
end


module PFuture : S = struct 
  type 'a future = {fd: file_descr; pid: int}

  let force (f: 'a future) : 'a =
    let ic = in_channel_of_descr f.fd in 
    let res = ((Marshal.from_channel ic) : 'a) in 
    close f.fd; 
    match waitpid [] f.pid with
    | (_,WEXITED 0) -> res
    | _ -> failwith "process failed to terminate in force"

  let future (f: 'a -> 'b) (x: 'a) : 'b future =
    let (fin, fout) = pipe () in
    match fork () with
    | 0 ->
        (close fin;
        let oc = out_channel_of_descr fout in
        Marshal.to_channel oc (f x) [Marshal.Closures];
        Pervasives.exit 0)
    | cid -> (close fout; {fd=fin; pid=cid})

end







