open! Core

module Matrix : sig
  type t =
    { pr : Parsecos_formula.t array
    ; jc : int array
    ; ir : int array
    }
  [@@deriving sexp_of, yojson]

  type triplet =
    { row : int
    ; col : int
    ; value : Parsecos_formula.t
    }

  val of_triplets : nrows:int -> ncols:int -> triplet list -> t
end

type t =
  { params : Parsecos_param.t list
  ; n : int
  ; m : int
  ; p : int
  ; l : int
  ; q : int array
  ; e : int
  ; g : Matrix.t
  ; a : Matrix.t option
  ; c : Parsecos_formula.t array
  ; h : Parsecos_formula.t array
  ; b : Parsecos_formula.t array option
  ; bool_vars_idx : int array
  ; int_vars_idx : int array
  ; column_names : string array
  ; objective_offset : Parsecos_formula.t
  }
[@@deriving sexp_of, yojson]
