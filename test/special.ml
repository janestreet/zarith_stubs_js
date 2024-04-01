open! Core
open! Import

module Ml_z_fib = struct
  let%expect_test "print x, fib x" =
    let open Zarith.Z in
    Sexp.List (List.init 10 ~f:(fun i -> [%message (i : int) (fib i : t)])) |> print_s;
    [%expect
      {|
      (((i 0) ("fib i" 0)) ((i 1) ("fib i" 1)) ((i 2) ("fib i" 1))
       ((i 3) ("fib i" 2)) ((i 4) ("fib i" 3)) ((i 5) ("fib i" 5))
       ((i 6) ("fib i" 8)) ((i 7) ("fib i" 13)) ((i 8) ("fib i" 21))
       ((i 9) ("fib i" 34)))
      |}]
  ;;
end

module Ml_z_lucnum = struct
  let%expect_test "print x, lucnum x" =
    let open Zarith.Z in
    Sexp.List (List.init 10 ~f:(fun i -> [%message (i : int) (lucnum i : t)])) |> print_s;
    [%expect
      {|
      (((i 0) ("lucnum i" 2)) ((i 1) ("lucnum i" 1)) ((i 2) ("lucnum i" 3))
       ((i 3) ("lucnum i" 4)) ((i 4) ("lucnum i" 7)) ((i 5) ("lucnum i" 11))
       ((i 6) ("lucnum i" 18)) ((i 7) ("lucnum i" 29)) ((i 8) ("lucnum i" 47))
       ((i 9) ("lucnum i" 76)))
      |}]
  ;;
end

module Ml_z_jacobi = struct
  let%expect_test "print x, y, jacobi x y" =
    let open Zarith.Z in
    Static.quickcheck_pair
      ~f:(fun x y ->
        let y = Z.abs (Z.add (Z.mul y (Z.of_int 2)) Z.one) in
        [%message (x : t) (y : t) (jacobi x y : int)])
      ();
    [%expect {| ((hash cd1cadf3e1014c37df027be2d6cfdc37) (uniqueness_rate 96.089932)) |}]
  ;;
end

module Ml_z_legendre = struct
  let%expect_test "print x, y, legendre x y" =
    let open Zarith.Z in
    Static.quickcheck_pair
      ~f:(fun x y ->
        let y = Z.abs (Z.add (Z.mul y (Z.of_int 2)) Z.one) in
        [%message (x : t) (y : t) (legendre x y : int)])
      ();
    [%expect {| ((hash 00f3ba8c7b418e70c4311a972509d7d5) (uniqueness_rate 96.089932)) |}]
  ;;
end

module Ml_z_kronecker = struct
  let%expect_test ("print x, y, kronecker x y" [@tags "no-js"]) =
    let open Zarith.Z in
    Static.quickcheck_pair
      ~f:(fun x y -> [%message (x : t) (y : t) (kronecker x y : int)])
      ();
    [%expect {| ((hash 5e20979fff7d0cdacebdaf949ad2a9cf) (uniqueness_rate 96.529814)) |}]
  ;;
end

module Ml_z_bin = struct
  let%expect_test "print x, y, bin x y" =
    let open Zarith.Z in
    Static.quickcheck
      ~f:(fun n ->
        Sexp.List (List.init 10 ~f:(fun i -> [%message (n : t) (i : int) (bin n i : t)])))
      ();
    [%expect {| ((hash e9550b72915b41a5e8e84150d82c3cda) (uniqueness_rate 85.742188)) |}]
  ;;
end
