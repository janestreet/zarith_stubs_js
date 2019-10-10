open! Core_kernel
open! Import

module Ml_z_gcd = struct
  (* Testing with zero is unpredictable when negative numbers come into play.
     https://github.com/ocaml/Zarith/issues/58 *)
  let%expect_test "print x, y, gcd x y" =
    Static.quickcheck_pair
      ~filter:Filter.not_zero
      ~f:(fun x y -> [%message (x : t) (y : t) (gcd x y : t)])
      ();
    [%expect "((hash d68d643cd169e881bf1fcd382659cbf6) (uniqueness_rate 99.780702))"]
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
    [%expect
      {|
      ((hash dbf22dbfdb8397782f4069c588c27ed7) (uniqueness_rate 80.30303))|}]
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
    Static.quickcheck_pair
      (* According to gmp docs: "behavior is undefined when y is zero" *)
      ~filter:Filter.(combine [ not_zero ])
      ~f:(fun x y -> [%message (x : t) (y : t) (invert x y : t)])
      ();
    (* Low uniqueness rate because not many pairs of numbers are invertable. *)
    [%expect
      {|
      ((hash d49fc2f192aad76966e1cb98f2afac79) (uniqueness_rate 66.502193)) |}]
  ;;
end
