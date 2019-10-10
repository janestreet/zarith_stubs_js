open! Core_kernel
open! Import

module Ml_z_probab_prime = struct
  let%expect_test "print x, probab_prime x" =
    Static.quickcheck
      ~f:(fun x ->
        List.range 2 7
        |> List.map ~f:(fun i ->
          [%message (x : t) (i : int) (Int.equal 0 (probab_prime x i) : bool)])
        |> Sexp.List)
      ();
    [%expect {| ((hash c18a1924b9cd5dc81139c67a1b40950d) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_nextprime = struct
  let%expect_test "print x, nextprime x" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (nextprime x : t)]) ();
    [%expect "((hash e4324c97c8ef012678595c7c8203b8c8) (uniqueness_rate 85.742188))"]
  ;;
end
