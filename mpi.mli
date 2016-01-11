
module type S = sig 
  type ('s, 'r) channel
  val spawn : (('r, 's) channel -> 'a -> unit) -> 'a -> ('s, 'r) channel
  val send : ('s, 'r) channel -> 's -> unit
  val receive : ('s, 'r) channel -> 'r
  val wait_die : ('s, 'r) channel -> unit
end


module Mpi : S