
type group
type area

module S : Sequence.S

val group_of_data: int*float*float -> group 
val rowcol_of_latlon: area -> int*int -> float*float -> int*int
val latlon_of_rowcol: area -> int*int -> int*int*int*int -> area

val encompassing_area: group S.t -> area 
val population_search: group S.t -> area -> int 
val precompute: group S.t -> area -> int*int -> int S.t S.t
val population_lookup: int S.t S.t -> int*int*int*int -> int







