open Marshal
open Unix
open System


(*******************************************************************************
 * Simple message passing interface using channels
 * Note: the channel type is a phantom type used to track the 
 *       types of data sent and received throught the channel
 *******************************************************************************)

module type S = sig 
  type ('s, 'r) channel
  val spawn : (('r, 's) channel -> 'a -> unit) -> 'a -> ('s, 'r) channel
  val send : ('s, 'r) channel -> 's -> unit
  val receive : ('s, 'r) channel -> 'r
  val wait_die : ('s, 'r) channel -> unit
end


module Mpi : S = struct 
  type ('s, 'r) channel = {send: out_channel; rcv: in_channel; pid: int}

  let spawn f x = 
    let (pin, cout) = pipe () in 
    let (cin, pout) = pipe () in 
    let ppid = Unix.getpid () in 
    match fork () with
    | 0 ->
        (close pin; close pout;
        f {send = out_channel_of_descr cout; rcv = in_channel_of_descr cin; pid=ppid} x;
        Pervasives.exit 0)
    | cid -> 
        (close cin; close cout;
        {send=out_channel_of_descr pout; rcv=in_channel_of_descr pin; pid=cid})

  let send (a: ('s, 'r) channel) (msg: 's) : unit = 
    let res = Marshal.to_channel a.send msg [Marshal.Closures] in 
    flush a.send; res

  let receive (a: ('s, 'r) channel) : 'r = 
    ((Marshal.from_channel a.rcv) : 'r) 

  let wait_die (a: ('s, 'r) channel) : unit =
    close_out a.send;
    close_in a.rcv;
    match waitpid [] a.pid with
    | (_,WEXITED 0) -> ()
    | _ -> failwith "process failed to terminate in wait_die"
end








