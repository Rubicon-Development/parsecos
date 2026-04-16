open! Core
open Parsecos

let print_sexp sexp_of value = print_s (sexp_of value)

let print_metadata data =
  let payload =
    [%sexp
      { column_names = (data.Ecos_data.column_names : string array)
      ; c = (data.c : float array)
      ; bool_vars_idx = (data.bool_vars_idx : int array)
      ; int_vars_idx = (data.int_vars_idx : int array)
      }]
  in
  print_s payload
;;

let print_linear_data data =
  let payload =
    [%sexp
      { p = (data.Ecos_data.p : int)
      ; m = (data.m : int)
      ; l = (data.l : int)
      ; a = (data.a : Csc_matrix.t option)
      ; b = (data.b : float array option)
      ; g = (data.g : Csc_matrix.t)
      ; h = (data.h : float array)
      }]
  in
  print_s payload
;;

let print_soc_data data =
  let payload =
    [%sexp
      { l = (data.Ecos_data.l : int)
      ; q = (data.q : int array)
      ; g = (data.g : Csc_matrix.t)
      ; h = (data.h : float array)
      }]
  in
  print_s payload
;;

let print_full_data data =
  let payload =
    [%sexp
      { column_names = (data.Ecos_data.column_names : string array)
      ; c = (data.c : float array)
      ; p = (data.p : int)
      ; m = (data.m : int)
      ; l = (data.l : int)
      ; q = (data.q : int array)
      ; a = (data.a : Csc_matrix.t option)
      ; b = (data.b : float array option)
      ; g = (data.g : Csc_matrix.t)
      ; h = (data.h : float array)
      ; bool_vars_idx = (data.bool_vars_idx : int array)
      ; int_vars_idx = (data.int_vars_idx : int array)
      }]
  in
  print_s payload
;;

let print_parse_result = function
  | Ok data ->
    print_s
      [%sexp
        { column_names = (data.Ecos_data.column_names : string array)
        ; c = (data.c : float array)
        ; p = (data.p : int)
        ; m = (data.m : int)
        ; l = (data.l : int)
        ; q = (data.q : int array)
        ; a = (data.a : Csc_matrix.t option)
        ; b = (data.b : float array option)
        ; g = (data.g : Csc_matrix.t)
        ; h = (data.h : float array)
        ; bool_vars_idx = (data.bool_vars_idx : int array)
        ; int_vars_idx = (data.int_vars_idx : int array)
        ; objective_offset = (data.objective_offset : float)
        }]
  | Error error -> print_s [%sexp (error : Parsecos.Error.t)]
;;

let print_template_result = function
  | Ok template ->
    Parsecos.Ecos_template.yojson_of_t template
    |> Yojson.Safe.pretty_to_string
    |> print_endline
  | Error error -> print_s [%sexp (error : Parsecos.Error.t)]
;;

let%expect_test "variable ordering and integrality metadata" =
  let x = Var.continuous "x" in
  let b = Var.boolean "b" in
  let u = Var.array1 ~name:"u" ~domain:Var.Domain.Integer ~length:3 in
  let y = Var.array2 ~name:"y" ~domain:Var.Domain.Continuous ~dim1:2 ~dim2:2 in
  let objective =
    Affine.sum
      [ Affine.term 1.0 x
      ; Affine.term 2.0 b
      ; Affine.term 10.0 (Var.get1 u 0)
      ; Affine.term 11.0 (Var.get1 u 1)
      ; Affine.term 12.0 (Var.get1 u 2)
      ; Affine.term 20.0 (Var.get2 y 0 0)
      ; Affine.term 21.0 (Var.get2 y 0 1)
      ; Affine.term 22.0 (Var.get2 y 1 0)
      ; Affine.term 23.0 (Var.get2 y 1 1)
      ]
  in
  let data = Problem.minimize objective |> Problem.to_ecos_data in
  print_metadata data;
  [%expect
    {|
    ((column_names (x b u[0] u[1] u[2] y[0,0] y[0,1] y[1,0] y[1,1]))
     (c (1 2 10 11 12 20 21 22 23)) (bool_vars_idx (1)) (int_vars_idx (2 3 4)))
    |}]
;;

let%expect_test "objective constants and coefficient merging" =
  let x = Var.continuous "x_merge" in
  let y = Var.continuous "y_cancel" in
  let objective =
    Affine.sum
      [ Affine.term 2.0 x
      ; Affine.term 3.0 x
      ; Affine.constant 7.0
      ; Affine.term 4.0 y
      ; Affine.scale (-1.0) (Affine.term 4.0 y)
      ]
  in
  let data = Problem.minimize objective |> Problem.to_ecos_data in
  print_s
    [%sexp
      { column_names = (data.column_names : string array)
      ; c = (data.c : float array)
      ; objective_offset = (data.objective_offset : float)
      }];
  [%expect
    {|
    ((column_names (x_merge)) (c (5)) (objective_offset 7))
    |}]
;;

let%expect_test "linear equalities and inequalities canonicalize into A b G h" =
  let x = Var.continuous "x_lin" in
  let y = Var.continuous "y_lin" in
  let constraints =
    [ Constraint.eq
        (Affine.sum [ Affine.term 1.0 x; Affine.term 2.0 y; Affine.constant 3.0 ])
        10.0
    ; Constraint.le
        (Affine.sum [ Affine.term 1.0 x; Affine.term (-1.0) y; Affine.constant 1.0 ])
        4.0
    ; Constraint.ge (Affine.sum [ Affine.term 2.0 x; Affine.constant 5.0 ]) 1.0
    ]
  in
  let data =
    Problem.minimize Affine.zero
    |> fun problem -> Problem.subject_to problem constraints |> Problem.to_ecos_data
  in
  print_linear_data data;
  [%expect
    {|
    ((p 1) (m 2) (l 2) (a (((pr (1 2)) (jc (0 1 2)) (ir (0 0))))) (b ((7)))
     (g ((pr (1 -2 -1)) (jc (0 2 3)) (ir (0 1 0)))) (h (3 4)))
    |}]
;;

let%expect_test "soc constraints become cone blocks in G and h" =
  let t = Var.continuous "t_soc" in
  let x = Var.continuous "x_soc" in
  let y = Var.continuous "y_soc" in
  let constraint_ =
    Constraint.soc
      ~t:(Affine.sum [ Affine.of_var t; Affine.constant 1.0 ])
      ~xs:[ Affine.sum [ Affine.of_var x; Affine.constant (-2.0) ]; Affine.term 3.0 y ]
  in
  let data =
    Problem.minimize Affine.zero
    |> fun problem -> Problem.subject_to problem [ constraint_ ] |> Problem.to_ecos_data
  in
  print_soc_data data;
  [%expect
    {|
    ((l 0) (q (3)) (g ((pr (-1 -1 -3)) (jc (0 1 2 3)) (ir (0 1 2))))
     (h (1 -2 0)))
    |}]
;;

let%expect_test "mixed integer metadata includes indexed families" =
  let x = Var.continuous "x_meta" in
  let b = Var.array1 ~name:"b_meta" ~domain:Var.Domain.Boolean ~length:2 in
  let k = Var.array2 ~name:"k_meta" ~domain:Var.Domain.Integer ~dim1:2 ~dim2:2 in
  let objective =
    Affine.sum
      [ Affine.of_var x
      ; Affine.of_var (Var.get1 b 0)
      ; Affine.of_var (Var.get1 b 1)
      ; Affine.of_var (Var.get2 k 0 0)
      ; Affine.of_var (Var.get2 k 0 1)
      ; Affine.of_var (Var.get2 k 1 0)
      ; Affine.of_var (Var.get2 k 1 1)
      ]
  in
  let data = Problem.minimize objective |> Problem.to_ecos_data in
  print_metadata data;
  [%expect
    {|
    ((column_names
      (x_meta b_meta[0] b_meta[1] k_meta[0,0] k_meta[0,1] k_meta[1,0]
       k_meta[1,1]))
     (c (1 1 1 1 1 1 1)) (bool_vars_idx (1 2)) (int_vars_idx (3 4 5 6)))
    |}]
;;

let%expect_test "time indexed equalities flatten deterministically" =
  let x = Var.array1 ~name:"x_dyn" ~domain:Var.Domain.Continuous ~length:3 in
  let u = Var.array1 ~name:"u_dyn" ~domain:Var.Domain.Boolean ~length:2 in
  let dynamics =
    List.init 2 ~f:(fun step ->
      Constraint.eq
        (Affine.sum
           [ Affine.of_var (Var.get1 x (step + 1))
           ; Affine.scale (-1.0) (Affine.of_var (Var.get1 x step))
           ; Affine.scale (-2.0) (Affine.of_var (Var.get1 u step))
           ])
        0.0)
  in
  let data =
    Problem.minimize Affine.zero
    |> fun problem -> Problem.subject_to problem dynamics |> Problem.to_ecos_data
  in
  print_s
    [%sexp
      { column_names = (data.column_names : string array)
      ; a = (data.a : Csc_matrix.t option)
      ; b = (data.b : float array option)
      ; bool_vars_idx = (data.bool_vars_idx : int array)
      }];
  [%expect
    {|
    ((column_names (x_dyn[0] x_dyn[1] x_dyn[2] u_dyn[0] u_dyn[1]))
     (a (((pr (-1 1 -1 1 -2 -2)) (jc (0 1 3 4 5 6)) (ir (0 0 1 1 0 1)))))
     (b ((0 0))) (bool_vars_idx (3 4)))
    |}]
;;

let%expect_test "time indexed mixed integer socp example" =
  let x = Var.array1 ~name:"x_misocp" ~domain:Var.Domain.Continuous ~length:3 in
  let on = Var.array1 ~name:"on_misocp" ~domain:Var.Domain.Boolean ~length:2 in
  let level = Var.array1 ~name:"level_misocp" ~domain:Var.Domain.Integer ~length:2 in
  let objective =
    Affine.sum
      [ Affine.term 1.0 (Var.get1 x 2)
      ; Affine.term 1.0 (Var.get1 on 0)
      ; Affine.term 1.0 (Var.get1 on 1)
      ; Affine.term 2.0 (Var.get1 level 0)
      ; Affine.term 2.0 (Var.get1 level 1)
      ]
  in
  let dynamics =
    List.init 2 ~f:(fun step ->
      Constraint.eq
        (Affine.sum
           [ Affine.of_var (Var.get1 x (step + 1))
           ; Affine.scale (-1.0) (Affine.of_var (Var.get1 x step))
           ; Affine.scale (-2.0) (Affine.of_var (Var.get1 on step))
           ; Affine.scale (-1.0) (Affine.of_var (Var.get1 level step))
           ])
        0.0)
  in
  let bounds = [ Constraint.le (Affine.of_var (Var.get1 x 0)) 10.0 ] in
  let cones =
    [ Constraint.soc
        ~t:(Affine.sum [ Affine.of_var (Var.get1 x 1); Affine.constant 3.0 ])
        ~xs:
          [ Affine.sum [ Affine.of_var (Var.get1 x 0); Affine.constant (-1.0) ]
          ; Affine.of_var (Var.get1 level 0)
          ]
    ; Constraint.soc
        ~t:(Affine.sum [ Affine.of_var (Var.get1 x 2); Affine.constant 3.0 ])
        ~xs:
          [ Affine.sum [ Affine.of_var (Var.get1 x 1); Affine.constant (-1.0) ]
          ; Affine.of_var (Var.get1 level 1)
          ]
    ]
  in
  let data =
    Problem.minimize objective
    |> fun problem ->
    Problem.subject_to problem (dynamics @ bounds @ cones) |> Problem.to_ecos_data
  in
  print_full_data data;
  [%expect
    {|
     ((column_names
       (x_misocp[0] x_misocp[1] x_misocp[2] on_misocp[0] on_misocp[1]
        level_misocp[0] level_misocp[1]))
      (c (0 0 1 1 1 2 2)) (p 2) (m 7) (l 1) (q (3 3))
      (a
       (((pr (-1 1 -1 1 -2 -2 -1 -1)) (jc (0 1 3 4 5 6 7 8))
         (ir (0 0 1 1 0 1 0 1)))))
      (b ((0 0)))
      (g ((pr (1 -1 -1 -1 -1 -1 -1)) (jc (0 2 4 5 5 5 6 7)) (ir (0 2 1 5 4 3 6))))
      (h (10 3 -1 0 3 -1 0)) (bool_vars_idx (3 4)) (int_vars_idx (5 6)))
     |}]
;;

let%expect_test "json input elaborates with declared unused variables preserved" =
  let model : Parsecos.Parsed_ast.model =
    { params = []
    ; vars =
        [ Scalar { name = "x_json"; domain = Continuous }
        ; Array1 { name = "on_json"; domain = Boolean; length = 2 }
        ; Scalar { name = "unused_json"; domain = Integer }
        ]
    ; objective =
        Minimize
          (Sum
             [ Var (Scalar_ref "x_json")
             ; Scale
                 { coefficient = 2.0
                 ; expr = Var (Array1_ref { name = "on_json"; index = 0 })
                 }
             ])
    ; constraints =
        [ Le
            { lhs = Var (Array1_ref { name = "on_json"; index = 1 }); rhs = Constant 1.0 }
        ]
    }
  in
  let data =
    model
    |> Parsecos.Parsed_ast.yojson_of_model
    |> Yojson.Safe.to_string
    |> Parsecos.ecos_data_of_json_string
  in
  print_parse_result data;
  [%expect
    {|
    ((column_names (x_json on_json[0] on_json[1] unused_json)) (c (1 2 0 0))
     (p 0) (m 1) (l 1) (q ()) (a ()) (b ())
     (g ((pr (1)) (jc (0 0 0 1 1)) (ir (0)))) (h (1)) (bool_vars_idx (1 2))
     (int_vars_idx (3)) (objective_offset 0))
    |}]
;;

let%expect_test "dsl template preserves affine parameter coefficients" =
  let text =
    {|
params:
  alpha
  price[2]
  demand[2]
  cap[1]

vars:
  continuous x[2]

minimize:
  (2 * price[0] - alpha + 3) * x[0] + price[1] * x[1] + demand[0]

subject to:
  x[0] + x[1] = demand[1]
  price[0] * x[0] <= cap[0]
  x[1] >= 0
|}
  in
  print_template_result (Parsecos.ecos_template_of_text text);
  [%expect
    {|
    {
      "params": [
        { "name": "alpha", "dimensions": [] },
        { "name": "price", "dimensions": [ 2 ] },
        { "name": "demand", "dimensions": [ 2 ] },
        { "name": "cap", "dimensions": [ 1 ] }
      ],
      "n": 2,
      "m": 2,
      "p": 1,
      "l": 2,
      "q": [],
      "e": 0,
      "g": {
        "pr": [
          {
            "constant": 0.0,
            "terms": [
              { "param": "price", "indices": [ 0 ], "coefficient": 1.0 }
            ]
          },
          { "constant": -1.0, "terms": [] }
        ],
        "jc": [ 0, 1, 2 ],
        "ir": [ 0, 1 ]
      },
      "a": {
        "pr": [
          { "constant": 1.0, "terms": [] }, { "constant": 1.0, "terms": [] }
        ],
        "jc": [ 0, 1, 2 ],
        "ir": [ 0, 0 ]
      },
      "c": [
        {
          "constant": 3.0,
          "terms": [
            { "param": "alpha", "indices": [], "coefficient": -1.0 },
            { "param": "price", "indices": [ 0 ], "coefficient": 2.0 }
          ]
        },
        {
          "constant": 0.0,
          "terms": [ { "param": "price", "indices": [ 1 ], "coefficient": 1.0 } ]
        }
      ],
      "h": [
        {
          "constant": 0.0,
          "terms": [ { "param": "cap", "indices": [ 0 ], "coefficient": 1.0 } ]
        },
        { "constant": 0.0, "terms": [] }
      ],
      "b": [
        {
          "constant": 0.0,
          "terms": [
            { "param": "demand", "indices": [ 1 ], "coefficient": 1.0 }
          ]
        }
      ],
      "bool_vars_idx": [],
      "int_vars_idx": [],
      "column_names": [ "x[0]", "x[1]" ],
      "objective_offset": {
        "constant": 0.0,
        "terms": [ { "param": "demand", "indices": [ 0 ], "coefficient": 1.0 } ]
      }
    }
    |}]
;;

let%expect_test "dsl template rejects parameter products" =
  let text =
    {|
params:
  alpha
  beta

vars:
  continuous x

minimize:
  alpha * beta * x

subject to:
  x >= 0
|}
  in
  print_template_result (Parsecos.ecos_template_of_text text);
  [%expect
    {|
    (Nonlinear_expression
     "products may only multiply a parameter-affine scalar by a parameter-free affine expression")
    |}]
;;

let%expect_test "dsl input elaborates with linear and soc constraints" =
  let text =
    {|
vars:
  continuous x_dsl
  boolean on_dsl[2]
  integer unused_dsl

minimize:
  x_dsl + on_dsl[0] + 3

subject to:
  x_dsl - on_dsl[1] = 2
  norm(x_dsl - 1, on_dsl[0]) <= x_dsl + 4
|}
  in
  let data = Parsecos.ecos_data_of_text text in
  print_parse_result data;
  [%expect
    {|
    ((column_names (x_dsl on_dsl[0] on_dsl[1] unused_dsl)) (c (1 1 0 0))
     (p 1) (m 3) (l 0) (q (3)) (a (((pr (1 -1)) (jc (0 1 1 2 2)) (ir (0 0)))))
     (b ((2))) (g ((pr (-1 -1 -1)) (jc (0 2 3 3 3)) (ir (0 1 2)))) (h (4 -1 0))
     (bool_vars_idx (1 2)) (int_vars_idx (3)) (objective_offset 3))
    |}]
;;

let%expect_test "dsl params and for loops expand across time" =
  let text =
    {|params:
  rhs = [2, 4]

vars:
  continuous x[2]
  continuous y[2]

minimize:
  for t in 0..1:
    x[t]
    2 * y[t]
  end

subject to:
  for t in 0..1:
    x[t] + y[t] = rhs[t]
    x[t] >= 0
  end
|}
  in
  print_endline text;
  let data = Parsecos.ecos_data_of_text text in
  print_parse_result data;
  [%expect
    {|
    params:
      rhs = [2, 4]

    vars:
      continuous x[2]
      continuous y[2]

    minimize:
      for t in 0..1:
        x[t]
        2 * y[t]
      end

    subject to:
      for t in 0..1:
        x[t] + y[t] = rhs[t]
        x[t] >= 0
      end

    ((column_names (x[0] x[1] y[0] y[1])) (c (1 1 2 2)) (p 2) (m 2) (l 2)
     (q ()) (a (((pr (1 1 1 1)) (jc (0 1 2 3 4)) (ir (0 1 0 1))))) (b ((2 4)))
     (g ((pr (-1 -1)) (jc (0 1 2 2 2)) (ir (0 1)))) (h (0 0)) (bool_vars_idx ())
     (int_vars_idx ()) (objective_offset 0))
    |}]
;;

let%expect_test "dsl example for a day-scale bes site" =
  let text =
    {|
params:
  load = [28, 26, 24, 23, 22, 24, 30, 38, 44, 48, 52, 55, 57, 56, 54, 50, 46, 44, 42, 40, 38, 36, 34, 31]
  pv_cap = [0, 0, 0, 0, 0, 2, 8, 16, 24, 32, 36, 38, 40, 36, 30, 22, 12, 4, 0, 0, 0, 0, 0, 0]

vars:
  continuous grid[24]
  continuous pv[24]
  continuous charge[24]
  continuous discharge[24]
  continuous soc[25]
  continuous gen[24]
  boolean gen_on[24]

minimize:
  for t in 0..23:
    grid[t] + 5 * gen[t] + 2 * gen_on[t]
  end

subject to:
  # Serve fixed site load with grid, PV, battery, and generator
  for t in 0..23:
    grid[t] + pv[t] + discharge[t] + gen[t] - charge[t] = load[t]
  end

  # Battery state dynamics and end-of-day return
  for t in 0..23:
    soc[t+1] - soc[t] - charge[t] + discharge[t] = 0
  end
  soc[0] = 40
  soc[24] = 40

  # Device limits
  for t in 0..23:
    grid[t] >= 0
    pv[t] >= 0
    pv[t] <= pv_cap[t]
    charge[t] >= 0
    charge[t] <= 20
    discharge[t] >= 0
    discharge[t] <= 20
    gen[t] >= 0
    gen[t] <= 40 * gen_on[t]
  end

  for t in 0..24:
    soc[t] >= 0
    soc[t] <= 80
  end
|}
  in
  let data = Parsecos.ecos_data_of_text text in
  let () =
    match data with
    | Ok d -> Ecos_data.yojson_of_t d |> Yojson.Safe.pretty_to_string |> print_endline
    | Error _ -> ()
  in
  [%expect
    {|
    {
      "n": 169,
      "m": 266,
      "p": 50,
      "l": 266,
      "q": [],
      "e": 0,
      "g": {
        "pr": [
          -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
          -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -40.0, -40.0, -40.0, -40.0,
          -40.0, -40.0, -40.0, -40.0, -40.0, -40.0, -40.0, -40.0, -40.0, -40.0,
          -40.0, -40.0, -40.0, -40.0, -40.0, -40.0, -40.0, -40.0, -40.0, -40.0
        ],
        "jc": [
          0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
          20, 21, 22, 23, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50,
          52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86,
          88, 90, 92, 94, 96, 98, 100, 102, 104, 106, 108, 110, 112, 114, 116,
          118, 120, 122, 124, 126, 128, 130, 132, 134, 136, 138, 140, 142, 144,
          146, 148, 150, 152, 154, 156, 158, 160, 162, 164, 166, 168, 170, 172,
          174, 176, 178, 180, 182, 184, 186, 188, 190, 192, 194, 196, 198, 200,
          202, 204, 206, 208, 210, 212, 214, 216, 218, 220, 222, 224, 226, 228,
          230, 232, 234, 236, 238, 240, 242, 244, 246, 248, 250, 252, 254, 256,
          258, 260, 262, 264, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275,
          276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289,
          290
        ],
        "ir": [
          0, 9, 18, 27, 36, 45, 54, 63, 72, 81, 90, 99, 108, 117, 126, 135, 144,
          153, 162, 171, 180, 189, 198, 207, 1, 2, 10, 11, 19, 20, 28, 29, 37,
          38, 46, 47, 55, 56, 64, 65, 73, 74, 82, 83, 91, 92, 100, 101, 109, 110,
          118, 119, 127, 128, 136, 137, 145, 146, 154, 155, 163, 164, 172, 173,
          181, 182, 190, 191, 199, 200, 208, 209, 3, 4, 12, 13, 21, 22, 30, 31,
          39, 40, 48, 49, 57, 58, 66, 67, 75, 76, 84, 85, 93, 94, 102, 103, 111,
          112, 120, 121, 129, 130, 138, 139, 147, 148, 156, 157, 165, 166, 174,
          175, 183, 184, 192, 193, 201, 202, 210, 211, 5, 6, 14, 15, 23, 24, 32,
          33, 41, 42, 50, 51, 59, 60, 68, 69, 77, 78, 86, 87, 95, 96, 104, 105,
          113, 114, 122, 123, 131, 132, 140, 141, 149, 150, 158, 159, 167, 168,
          176, 177, 185, 186, 194, 195, 203, 204, 212, 213, 216, 217, 218, 219,
          220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233,
          234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247,
          248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261,
          262, 263, 264, 265, 7, 8, 16, 17, 25, 26, 34, 35, 43, 44, 52, 53, 61,
          62, 70, 71, 79, 80, 88, 89, 97, 98, 106, 107, 115, 116, 124, 125, 133,
          134, 142, 143, 151, 152, 160, 161, 169, 170, 178, 179, 187, 188, 196,
          197, 205, 206, 214, 215, 8, 17, 26, 35, 44, 53, 62, 71, 80, 89, 98,
          107, 116, 125, 134, 143, 152, 161, 170, 179, 188, 197, 206, 215
        ]
      },
      "a": {
        "pr": [
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
          -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
          -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
          -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
          -1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, -1.0, 1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
          1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,
          -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0
        ],
        "jc": [
          0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
          20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,
          38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 50, 52, 54, 56, 58, 60, 62,
          64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98,
          100, 102, 104, 106, 108, 110, 112, 114, 116, 118, 120, 122, 124, 126,
          128, 130, 132, 134, 136, 138, 140, 142, 144, 146, 148, 150, 152, 154,
          156, 158, 160, 162, 164, 166, 168, 170, 172, 174, 176, 178, 180, 182,
          184, 186, 188, 190, 192, 194, 195, 196, 197, 198, 199, 200, 201, 202,
          203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216,
          217, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218,
          218, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218, 218
        ],
        "ir": [
          0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
          20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
          16, 17, 18, 19, 20, 21, 22, 23, 0, 24, 1, 25, 2, 26, 3, 27, 4, 28, 5,
          29, 6, 30, 7, 31, 8, 32, 9, 33, 10, 34, 11, 35, 12, 36, 13, 37, 14, 38,
          15, 39, 16, 40, 17, 41, 18, 42, 19, 43, 20, 44, 21, 45, 22, 46, 23, 47,
          0, 24, 1, 25, 2, 26, 3, 27, 4, 28, 5, 29, 6, 30, 7, 31, 8, 32, 9, 33,
          10, 34, 11, 35, 12, 36, 13, 37, 14, 38, 15, 39, 16, 40, 17, 41, 18, 42,
          19, 43, 20, 44, 21, 45, 22, 46, 23, 47, 24, 48, 24, 25, 25, 26, 26, 27,
          27, 28, 28, 29, 29, 30, 30, 31, 31, 32, 32, 33, 33, 34, 34, 35, 35, 36,
          36, 37, 37, 38, 38, 39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 44, 45,
          45, 46, 46, 47, 47, 49, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
          14, 15, 16, 17, 18, 19, 20, 21, 22, 23
        ]
      },
      "c": [
        1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
        1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0, 5.0, 5.0, 5.0, 5.0,
        5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0,
        5.0, 5.0, 5.0, 5.0, 5.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
        2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0
      ],
      "h": [
        0.0, 0.0, 0.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 20.0,
        0.0, 20.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 20.0, 0.0,
        20.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0,
        8.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0, 16.0, 0.0, 20.0, 0.0,
        20.0, 0.0, 0.0, 0.0, 0.0, 24.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0,
        32.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0, 36.0, 0.0, 20.0, 0.0,
        20.0, 0.0, 0.0, 0.0, 0.0, 38.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0,
        40.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0, 36.0, 0.0, 20.0, 0.0,
        20.0, 0.0, 0.0, 0.0, 0.0, 30.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0,
        22.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0, 12.0, 0.0, 20.0, 0.0,
        20.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 20.0, 0.0, 20.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 20.0, 0.0, 20.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 20.0, 0.0, 20.0, 0.0, 0.0, 0.0, 80.0, 0.0, 80.0,
        0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0,
        80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0,
        0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0, 0.0,
        80.0, 0.0, 80.0, 0.0, 80.0, 0.0, 80.0
      ],
      "b": [
        28.0, 26.0, 24.0, 23.0, 22.0, 24.0, 30.0, 38.0, 44.0, 48.0, 52.0, 55.0,
        57.0, 56.0, 54.0, 50.0, 46.0, 44.0, 42.0, 40.0, 38.0, 36.0, 34.0, 31.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 40.0, 40.0
      ],
      "bool_vars_idx": [
        145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158,
        159, 160, 161, 162, 163, 164, 165, 166, 167, 168
      ],
      "int_vars_idx": [],
      "column_names": [
        "grid[0]", "grid[1]", "grid[2]", "grid[3]", "grid[4]", "grid[5]",
        "grid[6]", "grid[7]", "grid[8]", "grid[9]", "grid[10]", "grid[11]",
        "grid[12]", "grid[13]", "grid[14]", "grid[15]", "grid[16]", "grid[17]",
        "grid[18]", "grid[19]", "grid[20]", "grid[21]", "grid[22]", "grid[23]",
        "pv[0]", "pv[1]", "pv[2]", "pv[3]", "pv[4]", "pv[5]", "pv[6]", "pv[7]",
        "pv[8]", "pv[9]", "pv[10]", "pv[11]", "pv[12]", "pv[13]", "pv[14]",
        "pv[15]", "pv[16]", "pv[17]", "pv[18]", "pv[19]", "pv[20]", "pv[21]",
        "pv[22]", "pv[23]", "charge[0]", "charge[1]", "charge[2]", "charge[3]",
        "charge[4]", "charge[5]", "charge[6]", "charge[7]", "charge[8]",
        "charge[9]", "charge[10]", "charge[11]", "charge[12]", "charge[13]",
        "charge[14]", "charge[15]", "charge[16]", "charge[17]", "charge[18]",
        "charge[19]", "charge[20]", "charge[21]", "charge[22]", "charge[23]",
        "discharge[0]", "discharge[1]", "discharge[2]", "discharge[3]",
        "discharge[4]", "discharge[5]", "discharge[6]", "discharge[7]",
        "discharge[8]", "discharge[9]", "discharge[10]", "discharge[11]",
        "discharge[12]", "discharge[13]", "discharge[14]", "discharge[15]",
        "discharge[16]", "discharge[17]", "discharge[18]", "discharge[19]",
        "discharge[20]", "discharge[21]", "discharge[22]", "discharge[23]",
        "soc[0]", "soc[1]", "soc[2]", "soc[3]", "soc[4]", "soc[5]", "soc[6]",
        "soc[7]", "soc[8]", "soc[9]", "soc[10]", "soc[11]", "soc[12]", "soc[13]",
        "soc[14]", "soc[15]", "soc[16]", "soc[17]", "soc[18]", "soc[19]",
        "soc[20]", "soc[21]", "soc[22]", "soc[23]", "soc[24]", "gen[0]",
        "gen[1]", "gen[2]", "gen[3]", "gen[4]", "gen[5]", "gen[6]", "gen[7]",
        "gen[8]", "gen[9]", "gen[10]", "gen[11]", "gen[12]", "gen[13]",
        "gen[14]", "gen[15]", "gen[16]", "gen[17]", "gen[18]", "gen[19]",
        "gen[20]", "gen[21]", "gen[22]", "gen[23]", "gen_on[0]", "gen_on[1]",
        "gen_on[2]", "gen_on[3]", "gen_on[4]", "gen_on[5]", "gen_on[6]",
        "gen_on[7]", "gen_on[8]", "gen_on[9]", "gen_on[10]", "gen_on[11]",
        "gen_on[12]", "gen_on[13]", "gen_on[14]", "gen_on[15]", "gen_on[16]",
        "gen_on[17]", "gen_on[18]", "gen_on[19]", "gen_on[20]", "gen_on[21]",
        "gen_on[22]", "gen_on[23]"
      ],
      "objective_offset": 0.0
    }
    |}]
;;
