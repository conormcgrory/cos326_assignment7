open Future

module PSeq = Sequence.Seq(PFuture)(struct let use_mpi = true end)


(* inverted_index computes an inverted index for the contents of
 * a given file. The filename is the given string.
 * The results are output to stdout. *)
let mkindex (args : string ) : unit = failwith "implement me"
	