open! Core_kernel
open! Import

module Ml_z_neg = struct
  let%test_unit "neg neg x = x" =
    Dynamic.quickcheck () ~f:(fun x -> [%test_eq: t] x (neg (neg x)))
  ;;

  let%test_unit "neg x = 0 - x" =
    Dynamic.quickcheck () ~f:(fun x -> [%test_eq: t] (zero - x) (neg x))
  ;;

  let%expect_test "print (x, neg x)" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (neg x : t)]) ();
    [%expect "((hash 7b0a8e898b48b6e4ef3a08b8879fc3a9) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_add = struct
  let add_test_helper a b = of_string a + of_string b |> print

  let%expect_test "small_integer_addition" =
    add_test_helper "1" "2";
    [%expect "3"]
  ;;

  let%expect_test "addition_with_a_negative" =
    add_test_helper "1" "-2";
    [%expect "-1"]
  ;;

  let%expect_test "addition_with_0" =
    add_test_helper "1" "0";
    [%expect "1"];
    add_test_helper "0" "1";
    [%expect "1"]
  ;;

  let%test_unit "a + 0 = a" =
    Dynamic.quickcheck () ~f:(fun a -> [%test_eq: t] (a + zero) a)
  ;;

  let%test_unit "a + b = b + a" =
    Dynamic.quickcheck_pair () ~f:(fun a b -> [%test_eq: t] (a + b) (b + a))
  ;;

  let%test_unit "(a + b) + c = a + (b + c)" =
    Dynamic.quickcheck_tripple () ~f:(fun a b c ->
      [%test_eq: t] (a + b + c) (a + (b + c)))
  ;;

  let%test_unit "a + -a = 0" =
    Dynamic.quickcheck () ~f:(fun a -> [%test_eq: t] (a + neg a) zero)
  ;;

  let%expect_test "print (x, y, x + y)" =
    Static.quickcheck_pair ~f:(fun x y -> [%message (x : t) (y : t) (x + y : t)]) ();
    [%expect "((hash 9763ccb282897020828d44e81572fd92) (uniqueness_rate 96.529814))"]
  ;;
end

module Ml_z_sub = struct
  let%test_unit "a - b = - (b - a)" =
    Dynamic.quickcheck_pair () ~f:(fun a b -> [%test_eq: t] (a - b) (neg (b - a)))
  ;;

  let%test_unit "a - a = 0" =
    Dynamic.quickcheck () ~f:(fun a -> [%test_eq: t] (a - a) zero)
  ;;

  let%test_unit "a - (neg a) = a + a" =
    Dynamic.quickcheck () ~f:(fun a -> [%test_eq: t] (a - neg a) (a + a))
  ;;

  let%expect_test "print (x, y, x - y)" =
    Static.quickcheck_pair ~f:(fun x y -> [%message (x : t) (y : t) (x - y : t)]) ();
    [%expect "((hash d2c310975ededc4cc2125e5f2f04f269) (uniqueness_rate 96.529814))"]
  ;;
end

module Ml_z_mul = struct
  let%test_unit "a * 0 = 0" =
    Dynamic.quickcheck () ~f:(fun a -> [%test_eq: t] (a * zero) zero)
  ;;

  let%test_unit "a * 2 = a + a" =
    Dynamic.quickcheck () ~f:(fun a -> [%test_eq: t] (a * of_int 2) (a + a))
  ;;

  let%test_unit "a * b = b * a" =
    Dynamic.quickcheck_pair () ~f:(fun a b -> [%test_eq: t] (a * b) (b * a))
  ;;

  let%test_unit "(a * b) * c = a * (b * c)" =
    Dynamic.quickcheck_tripple () ~f:(fun a b c ->
      [%test_eq: t] (a * b * c) (a * (b * c)))
  ;;

  let%test_unit "a * -a = neg (a * a)" =
    Dynamic.quickcheck () ~f:(fun a -> [%test_eq: t] (a * neg a) (neg (a * a)))
  ;;

  let%expect_test "print (x, y, x * y)" =
    Static.quickcheck_pair ~f:(fun x y -> [%message (x : t) (y : t) (x * y : t)]) ();
    [%expect "((hash 39c63570aefd9c9c0285c15dc3d5970e) (uniqueness_rate 96.529814))"]
  ;;
end

module Ml_z_succ = struct
  let%expect_test "succ 1 = 2" =
    print (succ (of_string "1"));
    [%expect "2"]
  ;;

  let%expect_test "succ 0 = 1" =
    print (succ (of_string "0"));
    [%expect "1"]
  ;;

  let%expect_test "succ 23092345681102982356239087190819801239109812287639021 = \
                   23092345681102982356239087190819801239109812287639022"
    =
    print (succ (of_string "23092345681102982356239087190819801239109812287639021"));
    [%expect "23092345681102982356239087190819801239109812287639022"]
  ;;

  let%expect_test "succ -1 = 0" =
    print (succ (of_string "-1"));
    [%expect "0"]
  ;;

  let%test_unit "succ x = x + 1" =
    Dynamic.quickcheck () ~f:(fun x -> [%test_eq: t] (succ x) (x + of_string "1"))
  ;;

  let%test_unit "succ x > x" = Dynamic.quickcheck () ~f:(fun x -> assert (succ x > x))

  let%expect_test "print (x, succ x)" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (succ x : t)]) ();
    [%expect "((hash 3b86d6cc93b6c79a143979a4fa00a7da) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_pred = struct
  let%expect_test "pred 2 = 1" =
    print (pred (of_string "2"));
    [%expect "1"]
  ;;

  let%expect_test "pred 1 = 0" =
    print (pred (of_string "1"));
    [%expect "0"]
  ;;

  let%expect_test "succ 23092345681102982356239087190819801239109812287639021 = \
                   23092345681102982356239087190819801239109812287639020"
    =
    print (pred (of_string "23092345681102982356239087190819801239109812287639021"));
    [%expect "23092345681102982356239087190819801239109812287639020"]
  ;;

  let%expect_test "pred -1 = -2" =
    print (pred (of_string "-1"));
    [%expect "-2"]
  ;;

  let%test_unit "pred x = x - 1" =
    Dynamic.quickcheck () ~f:(fun x -> [%test_eq: t] (pred x) (x - of_string "1"))
  ;;

  let%test_unit "succ x < x" = Dynamic.quickcheck () ~f:(fun x -> assert (pred x < x))

  let%test_unit "x |> succ |> pred = x" =
    Dynamic.quickcheck () ~f:(fun x -> [%test_eq: t] (x |> succ |> pred) x)
  ;;

  let%test_unit "x |> pred |> succ = x" =
    Dynamic.quickcheck () ~f:(fun x -> [%test_eq: t] (x |> pred |> succ) x)
  ;;

  let%expect_test "print (x, pred x)" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (pred x : t)]) ();
    [%expect "((hash 9f99db74aa88e98daa9e3a8c3d2bf45b) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_abs = struct
  let%expect_test "abs 0 = 0" =
    print (abs (of_string "0"));
    [%expect "0"]
  ;;

  let%expect_test "abs 1 = 1" =
    print (abs (of_string "1"));
    [%expect "1"]
  ;;

  let%expect_test "abs -1 = 1" =
    print (abs (of_string "-1"));
    [%expect "1"]
  ;;

  let%expect_test "abs -2 = 2" =
    print (abs (of_string "-2"));
    [%expect "2"]
  ;;

  let%test_unit "abs x >= 0" =
    Dynamic.quickcheck () ~f:(fun x -> assert (geq (abs x) zero))
  ;;

  let%test_unit "abs x = -1 * x when x < 0" =
    Dynamic.quickcheck () ~f:(fun x ->
      if x < of_string "0"
      then [%test_eq: t] (abs x) (x * of_string "-1")
      else [%test_eq: t] (abs x) x)
  ;;

  let%expect_test "print (x, abs x)" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (abs x : t)]) ();
    [%expect "((hash fd1abbf43e359f12fcd47315f3167c8d) (uniqueness_rate 85.742188))"]
  ;;
end
