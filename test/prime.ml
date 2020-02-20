open! Core_kernel
open! Import

module Ml_z_probab_prime = struct
  let%expect_test "print x, probab_prime x" =
    Static.quickcheck
      ~f:(fun x ->
        List.range 22 27
        |> List.map ~f:(fun i ->
          let probab_prime x i = not (Int.equal (Z.probab_prime x i) 0) in
          [%message (x : t) (i : int) (probab_prime x i : bool)])
        |> Sexp.List)
      ();
    [%expect
      {|
      ((hash 841bf3c0441e2a8f411a38399b51b11e) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_nextprime = struct
  let%expect_test "print x, nextprime x" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (nextprime x : t)]) ();
    [%expect "((hash e4324c97c8ef012678595c7c8203b8c8) (uniqueness_rate 85.742188))"]
  ;;
end
