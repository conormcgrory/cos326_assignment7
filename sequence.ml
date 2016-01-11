open System
open Future 
open Mpi


module type S = sig
  type 'a t
  val tabulate : (int -> 'a) -> int -> 'a t
  val seq_of_array : 'a array -> 'a t
  val array_of_seq : 'a t -> 'a array
  val iter: ('a -> unit) -> 'a t -> unit
  val length : 'a t -> int
  val empty : unit  ->'a t
  val cons : 'a -> 'a t -> 'a t
  val singleton : 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val nth : 'a t -> int -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_reduce : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val reduce : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a
  val flatten : 'a t t -> 'a t
  val repeat : 'a -> int -> 'a t
  val zip : ('a t * 'b t) -> ('a * 'b) t
  val split : 'a t -> int -> 'a t * 'a t
  val scan: ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
end



(*******************************************************)
(* Sequential Sequences Based on a List Representation *)
(*******************************************************)

module ListSeq : S = struct

  type 'a t = 'a list

  let length = List.length

  let empty () = []

  let cons (x:'a) (s:'a t) = x::s

  let singleton x = [x]

  let append = List.append

  let tabulate f n =
    let rec helper acc x =
      if x = n then List.rev acc
      else helper ((f x)::acc) (x+1) in
    helper [] 0

  let nth = List.nth

  let filter = List.filter

  let map = List.map

  let reduce = List.fold_left

  let map_reduce m r b s = reduce r b (map m s)

  let repeat x n =
    let rec helper x n acc =
      if n = 0 then acc else helper x (n-1) (x::acc) in
    helper x n []

  let flatten = List.flatten

  let zip (s1,s2) = List.combine s1 s2

  let split s i =
    let rec helper s i acc =
      match s,i with
        | [],_ -> failwith "split"
        | _,0 -> (List.rev acc,s)
        | h::t,_ -> helper t (i-1) (h::acc) in
    helper s i []

  let iter = List.iter

  let array_of_seq = Array.of_list

  let seq_of_array = Array.to_list

  let scan f b s = 
    let (_,xs) = List.fold_left (fun (v,ls) e -> let r = f v e in (r,r::ls)) (b,[]) s in
    List.rev xs

end


(*******************************************************)
(* Parallel Sequences                                  *)
(*******************************************************)

module type SEQ_ARGS = sig 
  val use_mpi: bool
end


module Seq (Par : Future.S) (Arg : SEQ_ARGS) : S = struct

  type 'a t = 'a array

  let num_cores = System.cpu_count ()

  let chunks (f: int * int -> 'a) (n:int): 'a array = 
    let num_chunks = num_cores in 
    Array.init num_chunks (fun i -> 
      let lo = (n * i) / num_chunks in 
      let hi = (n * (i+1) / num_chunks) - 1 in 
      f (lo,hi) 
    )

  let seq_of_array a = a

  let array_of_seq seq = seq

  let empty () = [||]

  let singleton elem = [| elem |]

  let length seq = Array.length seq 

  let cons elem seq = Array.append (singleton elem) seq 

  let append seq1 seq2 = Array.append seq1 seq2

  let nth seq i = seq.(i)

  let iter f seq = Array.iter f seq

  let reduce r b seq = 
    if seq = [||] then b else
    let cs = chunks (fun (lo,hi) -> Par.future (Array.fold_left r seq.(lo)) 
                                               (Array.sub seq (lo + 1) (hi - lo))) 
                    (Array.length seq) in
    Array.fold_left r b (Array.map (Par.force) cs)

  let map f seq = 
    let cs = chunks (fun (lo,hi) -> Par.future (Array.map f) 
                                               (Array.sub seq lo (hi - lo + 1))) 
                    (Array.length seq) in
    Array.fold_left (fun base x -> append base (Par.force x)) (empty ()) cs

  let map_reduce m r b seq = 
    if seq = [||] then b else
    let cs = chunks (fun (lo,hi) -> Par.future (Array.fold_left r (m seq.(lo))) 
                                               (map m (Array.sub seq (lo + 1) (hi - lo)))) 
                    (Array.length seq) in
    Array.fold_left r b (Array.map (Par.force) cs)

  let flatten seqseq =
    let xs = Array.to_list seqseq in
    let xxs = List.map (fun x -> Array.to_list x) xs in
    Array.of_list (List.flatten xxs)
    

  let repeat elem num = Array.make num elem

  let zip (seq1,seq2) = 
    let l1 = Array.to_list seq1 in
    let l2 = Array.to_list seq2 in
    Array.of_list(List.combine l1 l2)

  let split seq x = 
    let seq1 = Array.sub seq 0 x in
    let seq2 = Array.sub seq x ((Array.length seq) - x) in
    (seq1, seq2)

  let tabulate f n = 
    let rec make_arr (lo:int) (hi:int) (arr:int t): int t =
      if lo = hi then (cons hi arr)
      else 
       make_arr lo (hi-1) (cons hi arr)
    in
        
    let cs = chunks (fun (lo,hi) -> Par.future (Array.map f) 
                                               (make_arr lo hi [||])) 
                    n in
    Array.fold_left (fun base x -> append base (Par.force x)) (empty ()) cs


  (*******************************************************)
  (* Parallel Prefix Sum                                 *)
  (*******************************************************)

  (* Here you will implement a version of the parallel prefix scan for a sequence 
   * [a0; a1; ...], the result of scan will be [f base a0; f (f base a0) a1; ...] *)
  let scan (f: 'a -> 'a -> 'a) (base: 'a) (seq: 'a t) : 'a t =
    failwith "implement me"
        
end
