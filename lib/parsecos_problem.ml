open! Core

type t =
  { objective : Parsecos_affine.t
  ; constraints : Parsecos_constraint.t list
  ; declared_variables : Parsecos_var.t list option
  }

let minimize objective = { objective; constraints = []; declared_variables = None }
let subject_to problem constraints = { problem with constraints = problem.constraints @ constraints }
let with_declared_variables problem declared_variables = { problem with declared_variables = Some declared_variables }

module Row = struct
  type t =
    { coeffs : float Map.M(Parsecos_var.Scalar).t
    ; rhs : float
    }
end

let used_variables_from_terms problem =
  let objective_vars = Parsecos_affine.vars problem.objective in
  List.fold problem.constraints ~init:objective_vars ~f:(fun acc constraint_ -> Set.union acc (Parsecos_constraint.vars constraint_))
  |> Set.to_list

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

let dense_vector ~columns coeffs =
  let vector = Array.create ~len:(Map.length columns) 0.0 in
  Map.iteri coeffs ~f:(fun ~key:var ~data:coefficient ->
    let column = Map.find_exn columns var in
    vector.(column) <- coefficient);
  vector

let eq_row expr rhs = { Row.coeffs = Parsecos_affine.terms expr; rhs = rhs -. Parsecos_affine.constant_term expr }
let le_row expr rhs = { Row.coeffs = Parsecos_affine.terms expr; rhs = rhs -. Parsecos_affine.constant_term expr }

let soc_rows exprs =
  List.map exprs ~f:(fun expr ->
    { Row.coeffs = Map.map (Parsecos_affine.terms expr) ~f:Float.neg; rhs = Parsecos_affine.constant_term expr })

let triplets_of_rows ~columns rows =
  List.concat_mapi rows ~f:(fun row_index row ->
    Map.to_alist row.Row.coeffs
    |> List.map ~f:(fun (var, coefficient) ->
      { Parsecos_csc_matrix.row = row_index; col = Map.find_exn columns var; value = coefficient }))

let to_ecos_data problem =
  let variables = used_variables problem in
  let columns =
    List.mapi variables ~f:(fun index var -> var, index)
    |> Map.of_alist_exn (module Parsecos_var.Scalar)
  in
  let equality_rows, linear_rows, soc_blocks =
    List.fold problem.constraints ~init:([], [], []) ~f:(fun (eqs, les, socs) constraint_ ->
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
    else Some (Parsecos_csc_matrix.of_triplets ~nrows:(List.length equality_rows) ~ncols:(List.length variables) (triplets_of_rows ~columns equality_rows))
  in
  let g =
    Parsecos_csc_matrix.of_triplets ~nrows:(List.length g_rows) ~ncols:(List.length variables) (triplets_of_rows ~columns g_rows)
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
  { Parsecos_ecos_data.n = List.length variables
  ; m = List.length g_rows
  ; p = List.length equality_rows
  ; l = List.length linear_rows
  ; q
  ; e = 0
  ; g
  ; a
  ; c
  ; h
  ; b = if List.is_empty equality_rows then None else Some b
  ; bool_vars_idx
  ; int_vars_idx
  ; column_names
  ; objective_offset = Parsecos_affine.constant_term problem.objective
  }
