open! Core_kernel
open! Import

module Ml_z_sign = struct
  let%expect_test "print (a, sign a)" =
    Static.quickcheck ~f:(fun a -> [%message (a : t) (sign a : int)]) ();
    [%expect "((hash 6a50fc55f309b06e235a58279d95ab7c) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_numbits = struct
  let%expect_test "print (a, numbits a)" =
    Static.quickcheck ~f:(fun a -> [%message (a : t) (numbits a : int)]) ();
    [%expect "((hash 8b57321da6ff80f190ac44f5b0ccd514) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_size = struct
  let%expect_test "print x, size x" =
    let size64 =
      match Sys.word_size with
      | 64 -> fun x -> size x
      | 32 -> fun x -> Int.((size x + 1) / 2)
      | _ -> assert false
    in
    Static.quickcheck ~f:(fun x -> [%message (x : t) (size64 x : int)]) ();
    [%expect
      {|
      ((hash 0ce1a8f087b3d87b3d852b046f0f3c6d) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_trailing_zeros = struct
  let%expect_test "print x, trailing_zeros x" =
    Static.quickcheck
      (* The "zero" behavior for zarith is really weird, it returns Int.max_value. *)
      ~f:(fun x ->
        let trailing_zeros = trailing_zeros x in
        if equal zero x && Int.equal trailing_zeros Int.max_value
        then [%message (x : t) ("Int.max_value" : string)]
        else [%message (x : t) (trailing_zeros : int)])
      ();
    [%expect
      {|
      ((hash d14ebff09189ec485d47fdb192b84435) (uniqueness_rate 85.742188)) |}]
  ;;
end
