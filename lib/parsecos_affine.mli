open! Core

type t =
  { constant : float
  ; terms : float Map.M(Parsecos_var.Scalar).t
  }

val zero : t
val constant : float -> t
val of_var : Parsecos_var.t -> t
val term : float -> Parsecos_var.t -> t
val add : t -> t -> t
val sub : t -> t -> t
val scale : float -> t -> t
val sum : t list -> t
val terms : t -> float Map.M(Parsecos_var.Scalar).t
val constant_term : t -> float
val vars : t -> (Parsecos_var.t, Parsecos_var.Scalar.comparator_witness) Set.t
