open! Core_kernel
open! Import

module Ml_z_gcd = struct
  let%expect_test "print x, y, gcd x y" =
    Static.quickcheck_pair ~f:(fun x y -> [%message (x : t) (y : t) (gcd x y : t)]) ();
    [%expect "((hash 92f8df73c82a7f926d5783455913a5fb) (uniqueness_rate 96.529814))"]
  ;;
end

module Ml_z_gcdext_intern = struct
  let%expect_test "gcdext 12 17" =
    let test a b =
      let a = of_int a
      and b = of_int b in
      print_s [%message (a : t) (b : t) (gcdext a b : t * t * t)]
    in
    test 12 27;
    [%expect {| ((a 12) (b 27) ("gcdext a b" (3 -2 1))) |}];
    test 27 12;
    [%expect {| ((a 27) (b 12) ("gcdext a b" (3 1 -2))) |}];
    test (-12) 27;
    [%expect {| ((a -12) (b 27) ("gcdext a b" (3 2 1))) |}];
    test 27 (-12);
    [%expect {| ((a 27) (b -12) ("gcdext a b" (3 1 2))) |}];
    test 12 (-27);
    [%expect {| ((a 12) (b -27) ("gcdext a b" (3 -2 -1))) |}];
    test (-27) 12;
    [%expect {| ((a -27) (b 12) ("gcdext a b" (3 -1 -2))) |}];
    test (-12) (-27);
    [%expect {| ((a -12) (b -27) ("gcdext a b" (3 2 -1))) |}];
    test (-27) (-12);
    [%expect {| ((a -27) (b -12) ("gcdext a b" (3 -1 2))) |}]
  ;;

  let%expect_test "print x, y, gcdext x y" =
    Static.quickcheck_pair
      ~f:(fun x y -> [%message (x : t) (y : t) (gcdext x y : t * t * t)])
      ();
    [%expect {| ((hash 053b7e844ff37e7111c238d84e8f5c42) (uniqueness_rate 96.529814))|}]
  ;;
end

module Ml_z_invert = struct
  let%expect_test "" =
    format "%d" (invert (of_int (-437423)) (of_int (-50))) |> print_endline;
    [%expect {| 13 |}]
  ;;

  let%expect_test "" =
    format "%d" (invert (of_int (-437423)) (of_int 48)) |> print_endline;
    [%expect {| 1 |}]
  ;;

  let%expect_test "print x, y, invert x y" =
    Static.quickcheck_pair ~f:(fun x y -> [%message (x : t) (y : t) (invert x y : t)]) ();
    (* Low uniqueness rate because not many pairs of numbers are invertable. *)
    [%expect {| ((hash c792a46ebce6ab1538e70bbf2d463465) (uniqueness_rate 54.203324)) |}]
  ;;
end
