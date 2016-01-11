
module type S = sig
  type 'a future 
  val force : 'a future -> 'a
  val future : ('a -> 'b) -> 'a -> 'b future
end


module SFuture : S 
module PFuture : S 