open! Core

type t =
  { constant : float
  ; terms : float Map.M(Parsecos_var.Scalar).t
  }

let zero = { constant = 0.0; terms = Map.empty (module Parsecos_var.Scalar) }
let constant constant = { zero with constant }
let of_var var = { constant = 0.0; terms = Map.singleton (module Parsecos_var.Scalar) var 1.0 }

let term coefficient var =
  if Float.equal coefficient 0.0
  then zero
  else { constant = 0.0; terms = Map.singleton (module Parsecos_var.Scalar) var coefficient }

let add_term terms var coefficient =
  if Float.equal coefficient 0.0
  then terms
  else
    Map.change terms var ~f:(function
      | None -> Some coefficient
      | Some existing ->
        let updated = existing +. coefficient in
        if Float.equal updated 0.0 then None else Some updated)

let add left right =
  let terms =
    Map.fold right.terms ~init:left.terms ~f:(fun ~key ~data acc -> add_term acc key data)
  in
  { constant = left.constant +. right.constant; terms }

let sub left right = add left { constant = -.right.constant; terms = Map.map right.terms ~f:Float.neg }

let scale factor expr =
  if Float.equal factor 0.0
  then zero
  else
    { constant = factor *. expr.constant
    ; terms = Map.filter_map expr.terms ~f:(fun coefficient ->
        let scaled = factor *. coefficient in
        if Float.equal scaled 0.0 then None else Some scaled)
    }

let sum exprs = List.fold exprs ~init:zero ~f:add
let terms expr = expr.terms
let constant_term expr = expr.constant
let vars expr = Map.key_set expr.terms
