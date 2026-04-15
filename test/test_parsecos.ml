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
  print_s [%sexp { column_names = (data.column_names : string array); c = (data.c : float array); objective_offset = (data.objective_offset : float) }];
  [%expect
    {|
    ((column_names (x_merge)) (c (5)) (objective_offset 7))
    |}]

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
    ; Constraint.ge
        (Affine.sum [ Affine.term 2.0 x; Affine.constant 5.0 ])
        1.0
    ]
  in
  let data =
    Problem.minimize Affine.zero
    |> fun problem -> Problem.subject_to problem constraints
    |> Problem.to_ecos_data
  in
  print_linear_data data;
  [%expect
    {|
    ((p 1) (m 2) (l 2) (a (((pr (1 2)) (jc (0 1 2)) (ir (0 0))))) (b ((7)))
     (g ((pr (1 -2 -1)) (jc (0 2 3)) (ir (0 1 0)))) (h (3 4)))
    |}]

let%expect_test "soc constraints become cone blocks in G and h" =
  let t = Var.continuous "t_soc" in
  let x = Var.continuous "x_soc" in
  let y = Var.continuous "y_soc" in
  let constraint_ =
    Constraint.soc
      ~t:(Affine.sum [ Affine.of_var t; Affine.constant 1.0 ])
      ~xs:
        [ Affine.sum [ Affine.of_var x; Affine.constant (-2.0) ]
        ; Affine.term 3.0 y
        ]
  in
  let data =
    Problem.minimize Affine.zero
    |> fun problem -> Problem.subject_to problem [ constraint_ ]
    |> Problem.to_ecos_data
  in
  print_soc_data data;
  [%expect
    {|
    ((l 0) (q (3)) (g ((pr (-1 -1 -3)) (jc (0 1 2 3)) (ir (0 1 2))))
     (h (1 -2 0)))
    |}]

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
    |> fun problem -> Problem.subject_to problem dynamics
    |> Problem.to_ecos_data
  in
  print_s [%sexp { column_names = (data.column_names : string array); a = (data.a : Csc_matrix.t option); b = (data.b : float array option); bool_vars_idx = (data.bool_vars_idx : int array) }];
  [%expect
    {|
    ((column_names (x_dyn[0] x_dyn[1] x_dyn[2] u_dyn[0] u_dyn[1]))
     (a (((pr (-1 1 -1 1 -2 -2)) (jc (0 1 3 4 5 6)) (ir (0 0 1 1 0 1)))))
     (b ((0 0))) (bool_vars_idx (3 4)))
    |}]

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
    |> fun problem -> Problem.subject_to problem (dynamics @ bounds @ cones)
    |> Problem.to_ecos_data
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
