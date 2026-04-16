open! Core

type t =
  { constant : Parsecos_formula.t
  ; terms : Parsecos_formula.t Map.M(Parsecos_var.Scalar).t
  }

val zero : t
val constant : float -> t
val of_formula : Parsecos_formula.t -> t
val of_var : Parsecos_var.t -> t
val term : float -> Parsecos_var.t -> t
val term_formula : Parsecos_formula.t -> Parsecos_var.t -> t
val add : t -> t -> t
val sub : t -> t -> t
val scale : float -> t -> t
val scale_formula : Parsecos_formula.t -> t -> (t, Parsecos_error.t) result
val sum : t list -> t
val terms : t -> Parsecos_formula.t Map.M(Parsecos_var.Scalar).t
val constant_term : t -> Parsecos_formula.t
val vars : t -> (Parsecos_var.t, Parsecos_var.Scalar.comparator_witness) Set.t
val is_param_free : t -> bool
