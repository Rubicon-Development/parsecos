open! Core

type t =
  { objective : Parsecos_affine.t
  ; constraints : Parsecos_constraint.t list
  ; declared_variables : Parsecos_var.t list option
  ; declared_parameters : Parsecos_param.t list
  }

let minimize objective =
  { objective; constraints = []; declared_variables = None; declared_parameters = [] }
;;

let subject_to problem constraints =
  { problem with constraints = problem.constraints @ constraints }
;;

let with_declared_variables problem declared_variables =
  { problem with declared_variables = Some declared_variables }
;;

let with_declared_parameters problem declared_parameters =
  { problem with declared_parameters }
;;

module Row = struct
  type t =
    { coeffs : Parsecos_formula.t Map.M(Parsecos_var.Scalar).t
    ; rhs : Parsecos_formula.t
    }
end

let used_variables_from_terms problem =
  let objective_vars = Parsecos_affine.vars problem.objective in
  List.fold problem.constraints ~init:objective_vars ~f:(fun acc constraint_ ->
    Set.union acc (Parsecos_constraint.vars constraint_))
  |> Set.to_list
;;

let used_variables problem =
  match problem.declared_variables with
  | None -> used_variables_from_terms problem
  | Some declared_variables ->
    let declared_set = Set.of_list (module Parsecos_var.Scalar) declared_variables in
    let extras =
      used_variables_from_terms problem
      |> List.filter ~f:(fun variable -> not (Set.mem declared_set variable))
    in
    declared_variables @ extras
;;

let dense_vector ~columns coeffs =
  let vector = Array.create ~len:(Map.length columns) Parsecos_formula.zero in
  Map.iteri coeffs ~f:(fun ~key:var ~data:coefficient ->
    let column = Map.find_exn columns var in
    vector.(column) <- coefficient);
  vector
;;

let eq_row expr rhs =
  { Row.coeffs = Parsecos_affine.terms expr
  ; rhs =
      Parsecos_formula.sub
        (Parsecos_formula.constant rhs)
        (Parsecos_affine.constant_term expr)
  }
;;

let le_row expr rhs =
  { Row.coeffs = Parsecos_affine.terms expr
  ; rhs =
      Parsecos_formula.sub
        (Parsecos_formula.constant rhs)
        (Parsecos_affine.constant_term expr)
  }
;;

let soc_rows exprs =
  List.map exprs ~f:(fun expr ->
    { Row.coeffs = Map.map (Parsecos_affine.terms expr) ~f:(Parsecos_formula.scale (-1.0))
    ; rhs = Parsecos_affine.constant_term expr
    })
;;

let triplets_of_rows ~columns rows =
  List.concat_mapi rows ~f:(fun row_index row ->
    Map.to_alist row.Row.coeffs
    |> List.map ~f:(fun (var, coefficient) ->
      ({ row = row_index; col = Map.find_exn columns var; value = coefficient }
       : Parsecos_ecos_template.Matrix.triplet)))
;;

let to_ecos_template problem =
  let variables = used_variables problem in
  let columns =
    List.mapi variables ~f:(fun index var -> var, index)
    |> Map.of_alist_exn (module Parsecos_var.Scalar)
  in
  let equality_rows, linear_rows, soc_blocks =
    List.fold
      problem.constraints
      ~init:([], [], [])
      ~f:(fun (eqs, les, socs) constraint_ ->
        match constraint_ with
        | Parsecos_constraint.Eq { expr; rhs } -> eq_row expr rhs :: eqs, les, socs
        | Le { expr; rhs } -> eqs, le_row expr rhs :: les, socs
        | Soc { t; xs } -> eqs, les, soc_rows (t :: xs) :: socs)
  in
  let equality_rows = List.rev equality_rows in
  let linear_rows = List.rev linear_rows in
  let soc_blocks = List.rev soc_blocks in
  let g_rows = linear_rows @ List.concat soc_blocks in
  let q = soc_blocks |> List.map ~f:List.length |> Array.of_list in
  let column_names = variables |> List.map ~f:Parsecos_var.name |> Array.of_list in
  let c = dense_vector ~columns (Parsecos_affine.terms problem.objective) in
  let h = Array.of_list (List.map g_rows ~f:(fun row -> row.Row.rhs)) in
  let b = Array.of_list (List.map equality_rows ~f:(fun row -> row.Row.rhs)) in
  let a =
    if List.is_empty equality_rows
    then None
    else
      Some
        (Parsecos_ecos_template.Matrix.of_triplets
           ~nrows:(List.length equality_rows)
           ~ncols:(List.length variables)
           (triplets_of_rows ~columns equality_rows))
  in
  let g =
    Parsecos_ecos_template.Matrix.of_triplets
      ~nrows:(List.length g_rows)
      ~ncols:(List.length variables)
      (triplets_of_rows ~columns g_rows)
  in
  let bool_vars_idx, int_vars_idx =
    variables
    |> List.filter_mapi ~f:(fun index var ->
      match Parsecos_var.domain var with
      | Parsecos_var.Domain.Continuous -> None
      | Boolean -> Some (`Bool index)
      | Integer -> Some (`Int index))
    |> List.fold ~init:([], []) ~f:(fun (bools, ints) tag ->
      match tag with
      | `Bool index -> index :: bools, ints
      | `Int index -> bools, index :: ints)
    |> fun (bools, ints) -> Array.of_list (List.rev bools), Array.of_list (List.rev ints)
  in
  ({ params = problem.declared_parameters
   ; n = List.length variables
   ; m = List.length g_rows
   ; p = List.length equality_rows
   ; l = List.length linear_rows
   ; q
   ; e = 0
   ; g
   ; a
   ; c
   ; h
   ; b = (if List.is_empty equality_rows then None else Some b)
   ; bool_vars_idx
   ; int_vars_idx
   ; column_names
   ; objective_offset = Parsecos_affine.constant_term problem.objective
   }
   : Parsecos_ecos_template.t)
;;

let unresolved_params (template : Parsecos_ecos_template.t) =
  let formulas =
    [ Array.to_list template.c
    ; Array.to_list template.h
    ; Option.value_map template.b ~default:[] ~f:Array.to_list
    ; template.objective_offset :: []
    ; Array.to_list template.g.pr
    ; Option.value_map template.a ~default:[] ~f:(fun matrix -> Array.to_list matrix.pr)
    ]
    |> List.concat
  in
  List.concat_map formulas ~f:Parsecos_formula.unresolved_params
  |> List.dedup_and_sort ~compare:String.compare
;;

let to_float_array formulas =
  Array.map formulas ~f:(fun formula ->
    Option.value_exn (Parsecos_formula.to_float formula))
;;

let matrix_to_numeric (matrix : Parsecos_ecos_template.Matrix.t) =
  { Parsecos_csc_matrix.pr = to_float_array matrix.pr; jc = matrix.jc; ir = matrix.ir }
;;

let to_ecos_data_result problem =
  let template = to_ecos_template problem in
  match unresolved_params template with
  | [] ->
    Ok
      { Parsecos_ecos_data.n = template.n
      ; m = template.m
      ; p = template.p
      ; l = template.l
      ; q = template.q
      ; e = template.e
      ; g = matrix_to_numeric template.g
      ; a = Option.map template.a ~f:matrix_to_numeric
      ; c = to_float_array template.c
      ; h = to_float_array template.h
      ; b = Option.map template.b ~f:to_float_array
      ; bool_vars_idx = template.bool_vars_idx
      ; int_vars_idx = template.int_vars_idx
      ; column_names = template.column_names
      ; objective_offset =
          Option.value_exn (Parsecos_formula.to_float template.objective_offset)
      }
  | params -> Error (Parsecos_error.Unresolved_parameters params)
;;

let to_ecos_data problem =
  match to_ecos_data_result problem with
  | Ok data -> data
  | Error error -> failwith (Sexp.to_string_hum (Parsecos_error.sexp_of_t error))
;;
