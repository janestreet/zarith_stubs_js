open! Core
open! Import

module Ml_z_fac = struct
  let%expect_test "print x, fact x" =
    let open Zarith.Z in
    Sexp.List
      (List.init 10 ~f:(fun i ->
         let i = Int.succ i in
         [%message (i : int) (fac i : t)]))
    |> print_s;
    [%expect
      {|
      (((i 1) ("fac i" 1)) ((i 2) ("fac i" 2)) ((i 3) ("fac i" 6))
       ((i 4) ("fac i" 24)) ((i 5) ("fac i" 120)) ((i 6) ("fac i" 720))
       ((i 7) ("fac i" 5040)) ((i 8) ("fac i" 40320)) ((i 9) ("fac i" 362880))
       ((i 10) ("fac i" 3628800)))
      |}]
  ;;
end

module Ml_z_fac2 = struct
  let%expect_test "print x, fact2 x" =
    let open Zarith.Z in
    Sexp.List
      (List.init 10 ~f:(fun i ->
         let i = Int.succ i in
         [%message (i : int) (fac2 i : t)]))
    |> print_s;
    [%expect
      {|
      (((i 1) ("fac2 i" 1)) ((i 2) ("fac2 i" 2)) ((i 3) ("fac2 i" 3))
       ((i 4) ("fac2 i" 8)) ((i 5) ("fac2 i" 15)) ((i 6) ("fac2 i" 48))
       ((i 7) ("fac2 i" 105)) ((i 8) ("fac2 i" 384)) ((i 9) ("fac2 i" 945))
       ((i 10) ("fac2 i" 3840)))
      |}]
  ;;
end

module Ml_z_facM = struct
  let%expect_test "print x, fact2 x" =
    let open Zarith.Z in
    Sexp.List
      (List.init 10 ~f:(fun i ->
         let i = Int.succ i in
         Sexp.List
           (List.init 5 ~f:(fun m ->
              let m = Int.succ m in
              [%message (i : int) (m : int) (facM i m : t)]))))
    |> print_s;
    [%expect
      {|
      ((((i 1) (m 1) ("facM i m" 1)) ((i 1) (m 2) ("facM i m" 1))
        ((i 1) (m 3) ("facM i m" 1)) ((i 1) (m 4) ("facM i m" 1))
        ((i 1) (m 5) ("facM i m" 1)))
       (((i 2) (m 1) ("facM i m" 2)) ((i 2) (m 2) ("facM i m" 2))
        ((i 2) (m 3) ("facM i m" 2)) ((i 2) (m 4) ("facM i m" 2))
        ((i 2) (m 5) ("facM i m" 2)))
       (((i 3) (m 1) ("facM i m" 6)) ((i 3) (m 2) ("facM i m" 3))
        ((i 3) (m 3) ("facM i m" 3)) ((i 3) (m 4) ("facM i m" 3))
        ((i 3) (m 5) ("facM i m" 3)))
       (((i 4) (m 1) ("facM i m" 24)) ((i 4) (m 2) ("facM i m" 8))
        ((i 4) (m 3) ("facM i m" 4)) ((i 4) (m 4) ("facM i m" 4))
        ((i 4) (m 5) ("facM i m" 4)))
       (((i 5) (m 1) ("facM i m" 120)) ((i 5) (m 2) ("facM i m" 15))
        ((i 5) (m 3) ("facM i m" 10)) ((i 5) (m 4) ("facM i m" 5))
        ((i 5) (m 5) ("facM i m" 5)))
       (((i 6) (m 1) ("facM i m" 720)) ((i 6) (m 2) ("facM i m" 48))
        ((i 6) (m 3) ("facM i m" 18)) ((i 6) (m 4) ("facM i m" 12))
        ((i 6) (m 5) ("facM i m" 6)))
       (((i 7) (m 1) ("facM i m" 5040)) ((i 7) (m 2) ("facM i m" 105))
        ((i 7) (m 3) ("facM i m" 28)) ((i 7) (m 4) ("facM i m" 21))
        ((i 7) (m 5) ("facM i m" 14)))
       (((i 8) (m 1) ("facM i m" 40320)) ((i 8) (m 2) ("facM i m" 384))
        ((i 8) (m 3) ("facM i m" 80)) ((i 8) (m 4) ("facM i m" 32))
        ((i 8) (m 5) ("facM i m" 24)))
       (((i 9) (m 1) ("facM i m" 362880)) ((i 9) (m 2) ("facM i m" 945))
        ((i 9) (m 3) ("facM i m" 162)) ((i 9) (m 4) ("facM i m" 45))
        ((i 9) (m 5) ("facM i m" 36)))
       (((i 10) (m 1) ("facM i m" 3628800)) ((i 10) (m 2) ("facM i m" 3840))
        ((i 10) (m 3) ("facM i m" 280)) ((i 10) (m 4) ("facM i m" 120))
        ((i 10) (m 5) ("facM i m" 50))))
      |}]
  ;;
end
