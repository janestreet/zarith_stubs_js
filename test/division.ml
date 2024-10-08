open! Core
open! Import

module Ml_z_div = struct
  let%test_unit "a / a = 1" =
    Dynamic.quickcheck () ~include_zero:false ~f:(fun a -> [%test_eq: t] (a / a) one)
  ;;

  let%test_unit "a / 0 = throws" =
    Dynamic.quickcheck () ~f:(fun a ->
      try
        let (_ : Z.t) = a / zero in
        failwith "did not throw"
      with
      | Division_by_zero -> ())
  ;;

  let%expect_test "-844387025997697699501 / -588" =
    Z.print (Z.of_string "-844387025997697699501" / Z.of_string "-588");
    [%expect "1436032357138941665"]
  ;;

  let%test_unit "a / b = 1 / (b / a)" =
    Dynamic.quickcheck_pair () ~include_zero:false ~f:(fun a b ->
      let div1 = a / b in
      let div2 = b / a in
      if equal div1 zero || equal div2 zero then () else [%test_eq: t] div1 (one / div2))
  ;;

  let%test_unit "(a * b) / b = a" =
    Dynamic.quickcheck_pair () ~include_zero:false ~f:(fun a b ->
      [%test_eq: t] (a * b / b) a)
  ;;

  let%expect_test "print (x, y, x / y)" =
    Static.quickcheck_pair ~f:(fun x y -> [%message (x : t) (y : t) (x / y : t)]) ();
    [%expect "((hash 9d89273bc7184f22fd83e7308beb398b) (uniqueness_rate 88.416422))"]
  ;;
end

module Ml_z_cdiv = struct
  (* cdiv rounds towards positive infinity. *)
  let%expect_test "5 /> 3" =
    Z.print (Z.of_string "5" /> Z.of_string "3");
    [%expect "2"]
  ;;

  let%expect_test "-5 /> 3" =
    Z.print (Z.of_string "-5" /> Z.of_string "3");
    [%expect "-1"]
  ;;

  let%expect_test "5 /> -3" =
    Z.print (Z.of_string "5" /> Z.of_string "-3");
    [%expect "-1"]
  ;;

  let%expect_test "4 /> 4" =
    Z.print (Z.of_string "4" /> Z.of_string "4");
    [%expect "1"]
  ;;

  let%expect_test "1 /> 1" =
    Z.print (Z.of_string "1" /> Z.of_string "1");
    [%expect "1"]
  ;;

  let%expect_test "-1 /> 1" =
    Z.print (Z.of_string "-1" /> Z.of_string "1");
    [%expect "-1"]
  ;;

  let%expect_test "-844387025997697699501 /> -588" =
    Z.print (Z.of_string "-844387025997697699501" /> Z.of_string "-588");
    [%expect "1436032357138941666"]
  ;;

  let%expect_test "1 /> -1" =
    Z.print (Z.of_string "1" /> Z.of_string "-1");
    [%expect "-1"]
  ;;

  let%test_unit "(a * b) /> b = a" =
    Dynamic.quickcheck_pair () ~include_zero:false ~f:(fun a b ->
      assert (a /> b >= a / b))
  ;;

  let%expect_test "print (x, y, x /> y)" =
    Static.quickcheck_pair ~f:(fun x y -> [%message (x : t) (y : t) (x /> y : t)]) ();
    [%expect "((hash 98fa01b1831c668a6a76933f11ceb54b) (uniqueness_rate 88.416422))"]
  ;;
end

module Ml_z_fdiv = struct
  (* Fdiv rounds towards negative infinity *)
  let%expect_test "5 /< 3" =
    Z.print (Z.of_string "5" /< Z.of_string "3");
    [%expect "1"]
  ;;

  let%expect_test "-5 /< 3" =
    Z.print (Z.of_string "-5" /< Z.of_string "3");
    [%expect "-2"]
  ;;

  let%expect_test "5 /< -3" =
    Z.print (Z.of_string "5" /< Z.of_string "-3");
    [%expect "-2"]
  ;;

  let%expect_test "4 /< 4" =
    Z.print (Z.of_string "4" /< Z.of_string "4");
    [%expect "1"]
  ;;

  let%expect_test "1 /< 1" =
    Z.print (Z.of_string "1" /< Z.of_string "1");
    [%expect "1"]
  ;;

  let%expect_test "-1 /< 1" =
    Z.print (Z.of_string "-1" /< Z.of_string "1");
    [%expect "-1"]
  ;;

  let%expect_test "-844387025997697699501 /< -588" =
    Z.print (Z.of_string "-844387025997697699501" /< Z.of_string "-588");
    [%expect "1436032357138941665"]
  ;;

  let%expect_test "1 /< -1" =
    Z.print (Z.of_string "1" /< Z.of_string "-1");
    [%expect "-1"]
  ;;

  let%expect_test "print (x, y, x /< y)" =
    Static.quickcheck_pair ~f:(fun x y -> [%message (x : t) (y : t) (x /< y : t)]) ();
    [%expect "((hash d77a9337f3926e4659a3d7e4e9e3f196) (uniqueness_rate 88.416422))"]
  ;;
end

module Ml_z_rem = struct
  let%expect_test "print (x, y, rem x y)" =
    Static.quickcheck_pair ~f:(fun x y -> [%message (x : t) (y : t) (Z.rem x y : t)]) ();
    [%expect "((hash a6d99e4b3b3bd7f59e7954dd820f5b04) (uniqueness_rate 88.416422))"]
  ;;
end

module Ml_z_div_rem = struct
  let%expect_test "print (x, y, divrem x y)" =
    Static.quickcheck_pair
      ~f:(fun x y -> [%message (x : t) (y : t) (Z.div_rem x y : t * t)])
      ();
    [%expect "((hash 237975ffcd477be34a47b0a26c28593e) (uniqueness_rate 88.416422))"]
  ;;
end

module Ml_z_divexact = struct
  (* divexact divides the integers correctly iff the division is exact. *)
  let%expect_test "print x, y, divexact x y" =
    Static.quickcheck_pair
      ~f:(fun x y ->
        let prod = x * y in
        Sexp.List
          [ [%message (x : t) (y : t) (prod : t) (divexact prod x : t)]
          ; [%message (x : t) (y : t) (prod : t) (divexact prod y : t)]
          ])
      ();
    [%expect {| ((hash 01098278b1918be5cc873b9061bd3bd5) (uniqueness_rate 80.30303)) |}]
  ;;
end

module Ml_z_divisible = struct
  let%expect_test "print x, y, divisible x y" =
    Static.quickcheck_pair
      ~f:(fun x y ->
        Sexp.List
          [ [%message (x : t) (y : t) (divisible x y : bool)]
          ; [%message (x : t) (y : t) (divisible y x : bool)]
          ])
      ();
    [%expect {| ((hash 6505140a50f6be8c19d753b5bc9d0078) (uniqueness_rate 96.529814)) |}]
  ;;
end

module Ml_z_congruent = struct
  let%expect_test "congruent(_,_,0)" =
    printf "%b\n" Zarith.(Z.congruent Z.zero Z.zero Z.zero);
    printf "%b\n" Zarith.(Z.congruent (Z.of_int 1) (Z.of_int 1) Z.zero);
    printf "%b\n" Zarith.(Z.congruent (Z.of_int 2) (Z.of_int 4) Z.zero);
    [%expect
      {|
      true
      true
      false
      |}]
  ;;

  let%expect_test "print x, y, z, congruent x y z" =
    Static.quickcheck_tripple
      ~f:(fun x y z ->
        Sexp.List
          [ [%message (x : t) (y : t) (z : t) (congruent x y z : bool)]
          ; [%message (x : t) (y : t) (z : t) (congruent x z y : bool)]
          ; [%message (x : t) (y : t) (z : t) (congruent z y x : bool)]
          ])
      ();
    [%expect {| ((hash f7b829847eeeab5c6bada69e8ec6905d) (uniqueness_rate 96.934116)) |}]
  ;;
end

module Ml_z_remove = struct
  let%expect_test "remove" =
    let test a b =
      let open Zarith.Z in
      print_s [%message (a : t) (b : t) (remove a b : t * int)]
    in
    test (Z.of_int 0) (Z.of_int 1);
    test (Z.of_int 0) (Z.of_int 10);
    test (Z.of_int 1) (Z.of_int 1);
    test (Z.of_int 10) (Z.of_int 1);
    test (Z.of_int 1) (Z.of_int (-1));
    test (Z.of_int 10) (Z.of_int (-1));
    test (Z.of_int 2) (Z.of_int 4);
    test (Z.of_int 4) (Z.of_int 2);
    [%expect
      {|
      ((a 0) (b 1) ("remove a b" (0 0)))
      ((a 0) (b 10) ("remove a b" (0 0)))
      ((a 1) (b 1) ("remove a b" (1 0)))
      ((a 10) (b 1) ("remove a b" (10 0)))
      ((a 1) (b -1) ("remove a b" (1 0)))
      ((a 10) (b -1) ("remove a b" (10 0)))
      ((a 2) (b 4) ("remove a b" (2 0)))
      ((a 4) (b 2) ("remove a b" (1 2)))
      |}]
  ;;

  let%expect_test "print x, y, remove x y" =
    Static.quickcheck_pair
      ~f:(fun x y ->
        Sexp.List
          (List.filter_opt
             [ (if Z.equal Z.zero y
                then None
                else Some [%message (x : t) (y : t) (remove x y : t * int)])
             ; (if Z.equal Z.zero x
                then None
                else Some [%message (x : t) (y : t) (remove y x : t * int)])
             ]))
      ();
    [%expect {| ((hash 31785be785de93844f221901a25a0b5f) (uniqueness_rate 96.529814)) |}]
  ;;
end
