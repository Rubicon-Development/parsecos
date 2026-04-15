open! Core

module Binding = struct
  type t =
    | Scalar of Parsecos_var.t
    | Array1 of Parsecos_var.array1
    | Array2 of Parsecos_var.array2
end

type env =
  { bindings : Binding.t String.Map.t
  ; declared_variables : Parsecos_var.t list
  }

let domain = function
  | Parsecos_parsed_ast.Continuous -> Parsecos_var.Domain.Continuous
  | Boolean -> Parsecos_var.Domain.Boolean
  | Integer -> Parsecos_var.Domain.Integer

let add_binding env name binding declared_variables =
  if Map.mem env.bindings name
  then Error (Parsecos_error.Duplicate_variable name)
  else
    Ok
      { bindings = Map.set env.bindings ~key:name ~data:binding
      ; declared_variables = env.declared_variables @ declared_variables
      }

let build_env vars =
  List.fold_result vars ~init:{ bindings = String.Map.empty; declared_variables = [] } ~f:(fun env decl ->
    match decl with
    | Parsecos_parsed_ast.Scalar { name; domain = var_domain } ->
      let variable = Parsecos_var.create ~name ~domain:(domain var_domain) in
      add_binding env name (Binding.Scalar variable) [ variable ]
    | Array1 { name; domain = var_domain; length } ->
      if length < 0
      then Error (Parsecos_error.Negative_length { name; length })
      else
        let family = Parsecos_var.array1 ~name ~domain:(domain var_domain) ~length in
        add_binding env name (Binding.Array1 family) (Array.to_list family.vars)
    | Array2 { name; domain = var_domain; dim1; dim2 } ->
      if dim1 < 0
      then Error (Parsecos_error.Negative_dimension { name; axis = 1; length = dim1 })
      else if dim2 < 0
      then Error (Parsecos_error.Negative_dimension { name; axis = 2; length = dim2 })
      else
        let family = Parsecos_var.array2 ~name ~domain:(domain var_domain) ~dim1 ~dim2 in
        let declared_variables =
          Array.to_list family.vars |> List.concat_map ~f:Array.to_list
        in
        add_binding env name (Binding.Array2 family) declared_variables)

let lookup env name = Map.find env.bindings name |> Result.of_option ~error:(Parsecos_error.Unknown_variable name)

let resolve_reference env = function
  | Parsecos_parsed_ast.Scalar_ref name ->
    Result.bind (lookup env name) ~f:(function
      | Binding.Scalar variable -> Ok variable
      | Array1 _ -> Error (Parsecos_error.Expected_scalar name)
      | Array2 _ -> Error (Parsecos_error.Expected_scalar name))
  | Array1_ref { name; index } ->
    Result.bind (lookup env name) ~f:(function
      | Scalar _ -> Error (Parsecos_error.Expected_array1 name)
      | Array2 _ -> Error (Parsecos_error.Expected_array1 name)
      | Array1 family ->
        let length = Array.length family.vars in
        if index < 0 || index >= length
        then Error (Parsecos_error.Index_out_of_bounds { name; index; length })
        else Ok family.vars.(index))
  | Array2_ref { name; index1; index2 } ->
    Result.bind (lookup env name) ~f:(function
      | Scalar _ -> Error (Parsecos_error.Expected_array2 name)
      | Array1 _ -> Error (Parsecos_error.Expected_array2 name)
      | Array2 family ->
        let dim1 = Array.length family.vars in
        if index1 < 0 || index1 >= dim1
        then Error (Parsecos_error.Index2_out_of_bounds { name; axis = 1; index = index1; length = dim1 })
        else
          let row = family.vars.(index1) in
          let dim2 = Array.length row in
          if index2 < 0 || index2 >= dim2
          then Error (Parsecos_error.Index2_out_of_bounds { name; axis = 2; index = index2; length = dim2 })
          else Ok row.(index2))

let rec affine env = function
  | Parsecos_parsed_ast.Constant value -> Ok (Parsecos_affine.constant value)
  | Var reference -> Result.map (resolve_reference env reference) ~f:Parsecos_affine.of_var
  | Sum exprs ->
    List.fold_result exprs ~init:Parsecos_affine.zero ~f:(fun acc expr ->
      Result.map (affine env expr) ~f:(Parsecos_affine.add acc))
  | Sub (left, right) -> Result.bind (affine env left) ~f:(fun left -> Result.map (affine env right) ~f:(Parsecos_affine.sub left))
  | Scale { coefficient; expr } -> Result.map (affine env expr) ~f:(Parsecos_affine.scale coefficient)

let constraint_ env = function
  | Parsecos_parsed_ast.Eq { lhs; rhs } ->
    Result.bind (affine env lhs) ~f:(fun lhs ->
      Result.map (affine env rhs) ~f:(fun rhs -> Parsecos_constraint.eq (Parsecos_affine.sub lhs rhs) 0.0))
  | Le { lhs; rhs } ->
    Result.bind (affine env lhs) ~f:(fun lhs ->
      Result.map (affine env rhs) ~f:(fun rhs -> Parsecos_constraint.le (Parsecos_affine.sub lhs rhs) 0.0))
  | Ge { lhs; rhs } ->
    Result.bind (affine env lhs) ~f:(fun lhs ->
      Result.map (affine env rhs) ~f:(fun rhs -> Parsecos_constraint.ge (Parsecos_affine.sub lhs rhs) 0.0))
  | Soc { t; xs } ->
    Result.bind (affine env t) ~f:(fun t ->
      Result.bind (List.map xs ~f:(affine env) |> Result.all) ~f:(fun xs ->
        if List.is_empty xs
        then Error (Parsecos_error.Dsl_parse "soc constraints require at least one component")
        else Ok (Parsecos_constraint.soc ~t ~xs)))

let problem model =
  Result.bind (build_env model.Parsecos_parsed_ast.vars) ~f:(fun env ->
    Result.bind
      (match model.objective with
       | Minimize expr -> affine env expr)
      ~f:(fun objective ->
        Result.map (List.map model.constraints ~f:(constraint_ env) |> Result.all) ~f:(fun constraints ->
          Parsecos_problem.minimize objective
          |> fun problem -> Parsecos_problem.subject_to problem constraints
          |> fun problem -> Parsecos_problem.with_declared_variables problem env.declared_variables)))
