open! Core_kernel
open! Import

module Ml_z_logand = struct
  let%expect_test "print (x, y, x logand y)" =
    Static.quickcheck_pair
      ~f:(fun x y -> [%message (x : t) (y : t) (Z.logand x y : t)])
      ();
    [%expect "((hash edb3907ae89e9a054a3a3468ec4fa41e) (uniqueness_rate 96.529814))"]
  ;;
end

module Ml_z_logor = struct
  let%expect_test "print (x, y, x logor y)" =
    Static.quickcheck_pair
      ~f:(fun x y -> [%message (x : t) (y : t) (Z.logor x y : t)])
      ();
    [%expect "((hash 924dc08ed08d44cc6daa895e9cb15c1c) (uniqueness_rate 96.529814))"]
  ;;
end

module Ml_z_logxor = struct
  let%expect_test "print (x, y, x logxor y)" =
    Static.quickcheck_pair
      ~f:(fun x y -> [%message (x : t) (y : t) (Z.logxor x y : t)])
      ();
    [%expect "((hash b11b218de81f69b7985e08f02df67b1f) (uniqueness_rate 96.529814))"]
  ;;
end

module Ml_z_lognot = struct
  let%expect_test "print (x, lognot x)" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (Z.lognot x : t)]) ();
    [%expect "((hash b131739e5dd70cb57dd7a7250e4e4b1f) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_shift_left = struct
  let%expect_test "print x << y" =
    Static.quickcheck
      ~f:(fun x ->
        List.range 0 100
        |> List.map ~f:(fun y -> [%message (x : t) (y : int) (Z.shift_left x y : t)])
        |> Sexp.List)
      ();
    [%expect {| ((hash ec0fcbe27e42ce2098a5644515678069) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_shift_right = struct
  let%expect_test "print x >> y" =
    Static.quickcheck
      ~f:(fun x ->
        List.range 0 100
        |> List.map ~f:(fun y -> [%message (x : t) (y : int) (Z.shift_right x y : t)])
        |> Sexp.List)
      ();
    [%expect {| ((hash f7642afef5845a19faa1e2a1ff320b84) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_shift_right_trunc = struct
  let%expect_test "print x >> y" =
    Static.quickcheck
      ~f:(fun x ->
        List.range 0 100
        |> List.map ~f:(fun y ->
          [%message (x : t) (y : int) (Z.shift_right_trunc x y : t)])
        |> Sexp.List)
      ();
    [%expect {| ((hash 798c8f514381c4521966e3dd3ea93c1d) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_testbit = struct
  let%expect_test "testbit" =
    Static.quickcheck
      ~f:(fun x ->
        Sexp.List
          (List.init 100 ~f:(fun i ->
             [%message (x : t) (i : int) (Z.testbit x i : bool)])))
      ();
    [%expect
      {|
      ((hash a0f6bad9471e135b64764ab691a06b02) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_popcount = struct
  let%expect_test "print x, popcnt x" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (popcount x : int)]) ();
    (* Compression rate is low because our quickcheck implementation generates
       integers with a bounded bitcount. *)
    [%expect {| ((hash 1e429706c701b111d98b6e6e858bbea4) (uniqueness_rate 42.96875)) |}]
  ;;
end

module Ml_z_hamdist = struct
  let%expect_test "print x, y, hamdist x y" =
    let fits_int31 x = fits_int32 (Z.shift_left x 1) in
    Static.quickcheck_pair
      ~f:(fun x y ->
        (* hamdist handle negative argument weirdly *)
        if geq x zero || geq y zero || (fits_int31 x && fits_int31 y)
        then [%message (x : t) (y : t) (hamdist x y : int)]
        else [%message "special"])
      ();
    (* Compression rate is low because our quickcheck implementation generates
       integers with a bounded bitcount. *)
    [%expect
      {|
      ((hash 0a270232628736ee7d47c8b403250989) (uniqueness_rate 33.284457)) |}]
  ;;
end

module Ml_z_extract = struct
  let%expect_test "extract" =
    let test a o l =
      let result = extract (of_int a) o l in
      print_s [%message (a : int) (o : int) (l : int) (result : t)]
    in
    test 0b0101 1 1;
    [%expect {| ((a 5) (o 1) (l 1) (result 0)) |}];
    test 0b0101 1 2;
    [%expect {| ((a 5) (o 1) (l 2) (result 2)) |}];
    test 0b1101 1 3;
    [%expect {| ((a 13) (o 1) (l 3) (result 6)) |}];
    test (-4) 1 3;
    [%expect {| ((a -4) (o 1) (l 3) (result 6)) |}]
  ;;

  let%expect_test "print x, extract x 2 8" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (extract x 2 8 : t)]) ();
    [%expect
      {|
      ((hash 20b6f322ff762ded3d569d57b7f18135) (uniqueness_rate 85.742188))|}]
  ;;
end
