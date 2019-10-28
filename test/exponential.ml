open! Core_kernel
open! Import

module Ml_z_powm = struct
  let%expect_test "print x y z, powm x y z" =
    Static.quickcheck_tripple
      ~f:(fun x y z -> [%message (x : t) (y : t) (z : t) (powm x y z : t)])
      ();
    [%expect
      {|
      ((hash 9d5d2749d3ebde755be8b18a7420c365) (uniqueness_rate 71.705806)) |}]
  ;;
end

module Ml_z_root = struct
  let root_helper n s = Z.root (Z.of_string s) n |> Z.print

  let%expect_test "root 2 -1 = invalid_argument" =
    (try root_helper 2 "-1" with
     | exn -> Exn.sexp_of_t exn |> print_s);
    [%expect {|(Invalid_argument "Z.root: even root of a negative number")|}]
  ;;

  let%expect_test "root 2 0 = 0" =
    root_helper 2 "0";
    [%expect "0"]
  ;;

  let%expect_test "root 2 1 = 1" =
    root_helper 2 "1";
    [%expect "1"]
  ;;

  let%expect_test "root 2 2 = 1" =
    root_helper 2 "2";
    [%expect "1"]
  ;;

  let%expect_test "root 2 4 = 2" =
    root_helper 2 "4";
    [%expect "2"]
  ;;

  let%expect_test "print i, x, (root x i)" =
    Static.quickcheck
      ~f:(fun x ->
        List.range 2 16
        |> List.map ~f:(fun i -> [%message (x : t) (root x i : t)])
        |> Sexp.List)
      ();
    [%expect "((hash b2af96bdd453c921ceb81ae36c8d8dcd) (uniqueness_rate 42.96875))"]
  ;;
end

module Ml_z_perfect_square = struct
  let%expect_test "print x, perfect_square x" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (perfect_square x : bool)]) ();
    [%expect "((hash 8a842a620afa024c8415a5c82a2b3652) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_sqrt = struct
  let%expect_test "print x, sqrt x" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (sqrt x : t)]) ();
    (* Low comppression rate because there are so many "no sqrt of negative number" errors. *)
    [%expect {| ((hash 9cd6d29511aaaa02fb96ce90dc2bb0d8) (uniqueness_rate 42.96875)) |}]
  ;;
end

module Ml_z_sqrt_rem = struct
  let%expect_test "print x, sqrt x" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (sqrt_rem x : t * t)]) ();
    (* Low comppression rate because there are so many "no sqrt of negative number" errors. *)
    [%expect {| ((hash 9c54b9ca1f1401e7eb5b8dca2d80e8fd) (uniqueness_rate 42.96875)) |}]
  ;;
end

module Ml_z_powm_sec = struct
  let%expect_test "print x y z, powm_sec x y z" =
    Static.quickcheck_tripple
      ~f:(fun x y z -> [%message (x : t) (y : t) (z : t) (powm_sec x y z : t)])
      ();
    (* Low comppression rate because there are so many "modulus must be odd" and "exponent
       must be positive" errors. *)
    [%expect {|
((hash a3fab8ed7394c3c05d62041aaae68b0f) (uniqueness_rate 20.890411))|}]
  ;;

  let%expect_test "FILTERED print x y z, powm_sec x y z" =
    Static.quickcheck_tripple
      ~filter:
        Filter.(combine [ odd; positive ]) (* Filter to get a better uniqueness rate*)
      ~f:(fun x y z -> [%message (x : t) (y : t) (z : t) (powm_sec x y z : t)])
      ();
    [%expect {|
((hash 571382f34276e42f2a65e217aee6e69e) (uniqueness_rate 99.790795))|}]
  ;;
end

module Ml_z_perfect_power = struct
  (* These are the tests that gmp have for their perfect_power implementation *)

  let test_perfect_power_helper s v =
    let res = if Z.(perfect_power (of_string s)) then 1 else 0 in
    [%test_eq: int] res v;
    true
  ;;

  let%test "0" = test_perfect_power_helper "0" 1
  let%test "1" = test_perfect_power_helper "1" 1
  let%test "-1" = test_perfect_power_helper "-1" 1
  let%test "2" = test_perfect_power_helper "2" 0
  let%test "-2" = test_perfect_power_helper "-2" 0
  let%test "3" = test_perfect_power_helper "3" 0
  let%test "-3" = test_perfect_power_helper "-3" 0
  let%test "4" = test_perfect_power_helper "4" 1
  let%test "-4" = test_perfect_power_helper "-4" 0
  let%test "64" = test_perfect_power_helper "64" 1
  let%test "-64" = test_perfect_power_helper "-64" 1
  let%test "128" = test_perfect_power_helper "128" 1
  let%test "-128" = test_perfect_power_helper "-128" 1
  let%test "256" = test_perfect_power_helper "256" 1
  let%test "-256" = test_perfect_power_helper "-256" 0
  let%test "512" = test_perfect_power_helper "512" 1
  let%test "-512" = test_perfect_power_helper "-512" 1
  let%test "0x4000000" = test_perfect_power_helper "0x4000000" 1
  let%test "-0x4000000" = test_perfect_power_helper "-0x4000000" 1
  let%test "0x3cab640" = test_perfect_power_helper "0x3cab640" 1
  let%test "-0x3cab640" = test_perfect_power_helper "-0x3cab640" 0
  let%test "0x3e23840" = test_perfect_power_helper "0x3e23840" 1
  let%test "-0x3e23840" = test_perfect_power_helper "-0x3e23840" 0
  let%test "0x3d3a7ed1" = test_perfect_power_helper "0x3d3a7ed1" 1
  let%test "-0x3d3a7ed1" = test_perfect_power_helper "-0x3d3a7ed1" 1
  let%test "0x30a7a6000" = test_perfect_power_helper "0x30a7a6000" 1
  let%test "-0x30a7a6000" = test_perfect_power_helper "-0x30a7a6000" 1
  let%test "0xf33e5a5a59" = test_perfect_power_helper "0xf33e5a5a59" 1
  let%test "-0xf33e5a5a59" = test_perfect_power_helper "-0xf33e5a5a59" 0
  let%test "0xed1b1182118135d" = test_perfect_power_helper "0xed1b1182118135d" 1
  let%test "-0xed1b1182118135d" = test_perfect_power_helper "-0xed1b1182118135d" 1

  let%test "0xe71f6eb7689cc276b2f1" =
    test_perfect_power_helper "0xe71f6eb7689cc276b2f1" 1
  ;;

  let%test "-0xe71f6eb7689cc276b2f1" =
    test_perfect_power_helper "-0xe71f6eb7689cc276b2f1" 0
  ;;

  let%test "0x12644507fe78cf563a4b342c92e7da9fe5e99cb75a01" =
    test_perfect_power_helper "0x12644507fe78cf563a4b342c92e7da9fe5e99cb75a01" 1
  ;;

  let%test "-0x12644507fe78cf563a4b342c92e7da9fe5e99cb75a01" =
    test_perfect_power_helper "-0x12644507fe78cf563a4b342c92e7da9fe5e99cb75a01" 0
  ;;

  let%test "0x1ff2e7c581bb0951df644885bd33f50e472b0b73a204e13cbe98fdb424d66561e4000000" =
    test_perfect_power_helper
      "0x1ff2e7c581bb0951df644885bd33f50e472b0b73a204e13cbe98fdb424d66561e4000000"
      1
  ;;

  let%test "-0x1ff2e7c581bb0951df644885bd33f50e472b0b73a204e13cbe98fdb424d66561e4000000" =
    test_perfect_power_helper
      "-0x1ff2e7c581bb0951df644885bd33f50e472b0b73a204e13cbe98fdb424d66561e4000000"
      1
  ;;

  let%test "0x2b9b44db2d91a6f8165c8c7339ef73633228ea29e388592e80354e4380004aad84000000" =
    test_perfect_power_helper
      "0x2b9b44db2d91a6f8165c8c7339ef73633228ea29e388592e80354e4380004aad84000000"
      1
  ;;

  let%test "-0x2b9b44db2d91a6f8165c8c7339ef73633228ea29e388592e80354e4380004aad84000000" =
    test_perfect_power_helper
      "-0x2b9b44db2d91a6f8165c8c7339ef73633228ea29e388592e80354e4380004aad84000000"
      1
  ;;

  let%test "0x28d5a2b8f330910a9d3cda06036ae0546442e5b1a83b26a436efea5b727bf1bcbe7e12b47d81"
    =
    test_perfect_power_helper
      "0x28d5a2b8f330910a9d3cda06036ae0546442e5b1a83b26a436efea5b727bf1bcbe7e12b47d81"
      1
  ;;

  let%test "-0x28d5a2b8f330910a9d3cda06036ae0546442e5b1a83b26a436efea5b727bf1bcbe7e12b47d81"
    =
    test_perfect_power_helper
      "-0x28d5a2b8f330910a9d3cda06036ae0546442e5b1a83b26a436efea5b727bf1bcbe7e12b47d81"
      1
  ;;

  (* Disable this test in javascript as it takes more than 2 minutes to complete. *)
  let%expect_test ("print (x, perfect_power x)"[@tags "no-js"]) =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (perfect_power x : bool)]) ();
    [%expect "((hash ec1b6a78eea1217d5ec7ea0e85eafaef) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_pow = struct
  let%expect_test "print x y , pow x y" =
    Static.quickcheck_pair
      ~f:(fun x y ->
        let y = y mod of_int 1024 in
        [%message (x : t) (y : t) (pow x y : t)])
      ();
    (* Low uniqueness rate because there are so many "no pow of a negative number" errors. *)
    [%expect "((hash 39a530e2309b08c3b598d4a8525d110c) (uniqueness_rate 52.297165))"]
  ;;

  let%expect_test "FILTERED print x y , pow x y" =
    Static.quickcheck_pair
      ~filter:
        Filter.(combine [ positive ])
      (* Filter for better uniqueness compared to the above. *)
      ~f:(fun x y ->
        let y = y mod of_int 1024 in
        [%message (x : t) (y : t) (pow x y : t)])
      ();
    (* Low uniqueness rate because there are so many "no pow of a negative number" errors. *)
    [%expect "((hash e298785b52497454a125dd95c691dec3) (uniqueness_rate 99.778761))"]
  ;;
end
