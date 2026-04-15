open! Core

type t =
  { n : int
  ; m : int
  ; p : int
  ; l : int
  ; q : int array
  ; e : int
  ; g : Parsecos_csc_matrix.t
  ; a : Parsecos_csc_matrix.t option
  ; c : float array
  ; h : float array
  ; b : float array option
  ; bool_vars_idx : int array
  ; int_vars_idx : int array
  ; column_names : string array
  ; objective_offset : float
  }
[@@deriving sexp_of]
