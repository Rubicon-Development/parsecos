open! Core

type t =
  { pr : float array
  ; jc : int array
  ; ir : int array
  }
[@@deriving sexp_of]

type triplet =
  { row : int
  ; col : int
  ; value : float
  }

val of_triplets : nrows:int -> ncols:int -> triplet list -> t
