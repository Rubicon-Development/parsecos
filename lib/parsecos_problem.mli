open! Core

type t =
  { objective : Parsecos_affine.t
  ; constraints : Parsecos_constraint.t list
  ; declared_variables : Parsecos_var.t list option
  }

val minimize : Parsecos_affine.t -> t
val subject_to : t -> Parsecos_constraint.t list -> t
val with_declared_variables : t -> Parsecos_var.t list -> t
val to_ecos_data : t -> Parsecos_ecos_data.t
