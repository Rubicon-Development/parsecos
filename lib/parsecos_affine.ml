open! Core

type t =
  { constant : Parsecos_formula.t
  ; terms : Parsecos_formula.t Map.M(Parsecos_var.Scalar).t
  }

let zero =
  { constant = Parsecos_formula.zero; terms = Map.empty (module Parsecos_var.Scalar) }
;;

let constant constant = { zero with constant = Parsecos_formula.constant constant }
let of_formula constant = { zero with constant }

let of_var var =
  { constant = Parsecos_formula.zero
  ; terms = Map.singleton (module Parsecos_var.Scalar) var (Parsecos_formula.constant 1.0)
  }
;;

let term_formula (coefficient : Parsecos_formula.t) var =
  if Float.equal coefficient.constant 0.0 && List.is_empty coefficient.terms
  then zero
  else
    { constant = Parsecos_formula.zero
    ; terms = Map.singleton (module Parsecos_var.Scalar) var coefficient
    }
;;

let term coefficient var =
  if Float.equal coefficient 0.0
  then zero
  else term_formula (Parsecos_formula.constant coefficient) var
;;

let add_term terms var coefficient =
  if
    Parsecos_formula.is_constant coefficient
    && Float.equal coefficient.Parsecos_formula.constant 0.0
  then terms
  else
    Map.change terms var ~f:(function
      | None -> Some coefficient
      | Some existing ->
        let updated = Parsecos_formula.add existing coefficient in
        if Parsecos_formula.is_constant updated && Float.equal updated.constant 0.0
        then None
        else Some updated)
;;

let add left right =
  let terms =
    Map.fold right.terms ~init:left.terms ~f:(fun ~key ~data acc -> add_term acc key data)
  in
  { constant = Parsecos_formula.add left.constant right.constant; terms }
;;

let sub left right =
  add
    left
    { constant = Parsecos_formula.scale (-1.0) right.constant
    ; terms = Map.map right.terms ~f:(Parsecos_formula.scale (-1.0))
    }
;;

let scale factor expr =
  if Float.equal factor 0.0
  then zero
  else
    { constant = Parsecos_formula.scale factor expr.constant
    ; terms =
        Map.filter_map expr.terms ~f:(fun coefficient ->
          let scaled = Parsecos_formula.scale factor coefficient in
          if Parsecos_formula.is_constant scaled && Float.equal scaled.constant 0.0
          then None
          else Some scaled)
    }
;;

let scale_formula factor expr =
  Result.bind (Parsecos_formula.multiply factor expr.constant) ~f:(fun constant ->
    Result.map
      (Map.fold
         expr.terms
         ~init:(Ok (Map.empty (module Parsecos_var.Scalar)))
         ~f:(fun ~key ~data acc ->
           Result.bind acc ~f:(fun terms ->
             Result.map (Parsecos_formula.multiply factor data) ~f:(fun coefficient ->
               if
                 Parsecos_formula.is_constant coefficient
                 && Float.equal coefficient.constant 0.0
               then terms
               else Map.set terms ~key ~data:coefficient))))
      ~f:(fun terms -> { constant; terms }))
;;

let sum exprs = List.fold exprs ~init:zero ~f:add
let terms expr = expr.terms
let constant_term expr = expr.constant
let vars expr = Map.key_set expr.terms

let is_param_free expr =
  Parsecos_formula.is_constant expr.constant
  && Map.for_all expr.terms ~f:Parsecos_formula.is_constant
;;
