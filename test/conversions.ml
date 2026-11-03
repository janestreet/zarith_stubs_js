open! Core
open! Import

module Ml__z_of_int = struct
  let%test "a = a |> z_of_int |> int_of_z" =
    Core.Quickcheck.test Int.quickcheck_generator ~f:(fun i ->
      assert (Int.equal i (i |> Z.of_int_exn |> Z.to_int_exn)));
    true
  ;;
end

module Ml_z_of_nativeint = struct
  let%test "a = a |> z_of_nativeint |> int_of_z" =
    Core.Quickcheck.test Nativeint.quickcheck_generator ~f:(fun i ->
      assert (Nativeint.equal i (i |> Z.of_nativeint_exn |> Z.to_nativeint_exn)));
    true
  ;;
end

module Ml_z_of_int32 = struct
  let%test "a = a |> z_of_int32 |> int32_of_z" =
    Core.Quickcheck.test Int32.quickcheck_generator ~f:(fun i ->
      assert (Int32.equal i (i |> Z.of_int32_exn |> Z.to_int32_exn)));
    true
  ;;
end

module Ml_z_of_int64 = struct
  let max = Z.of_string "9223372036854775807"
  let min = Z.of_string "-9223372036854775808"

  let%expect_test _ =
    Z.to_int64 max |> [%sexp_of: Int64.t option] |> print_s;
    [%expect {| (9223372036854775807) |}];
    Z.to_int64 (Z.add max Z.one) |> [%sexp_of: Int64.t option] |> print_s;
    [%expect {| () |}];
    Z.to_int64 min |> [%sexp_of: Int64.t option] |> print_s;
    [%expect {| (-9223372036854775808) |}];
    Z.to_int64 (Z.sub min Z.one) |> [%sexp_of: Int64.t option] |> print_s;
    [%expect {| () |}]
  ;;

  let%test "a = a |> z_of_int_64 |> int64_of_z" =
    Core.Quickcheck.test Int64.quickcheck_generator ~f:(fun i ->
      assert (Int64.equal i (i |> Z.of_int64_exn |> Z.to_int64_exn)));
    true
  ;;
end

module Ml_z_to_int = struct
  let%test "i = i |> z_of_int |> z_to_int" =
    Core.Quickcheck.test Int.quickcheck_generator ~f:(fun x ->
      assert (Int.equal x (x |> Z.of_int |> Z.to_int_exn)));
    true
  ;;

  let%test "if fitsint i then i = i |> to_z |> of_z" =
    Static.quickcheck
      ~quiet:true
      ~f:(fun x ->
        if Z.fits_int x then assert (Z.equal x (x |> Z.to_int_exn |> Z.of_int)) else ();
        Sexp.Atom "")
      ();
    true
  ;;
end

module Ml_z_to_int32 = struct
  let%expect_test "print (Z.to_int_32 x)" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (Z.to_int32 x : int32 option)]) ();
    [%expect {| ((hash 2b8919d2f03f90fd1928acdea3fe9049) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_to_int64 = struct
  let%expect_test "print (Z.to_int_64 x)" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (Z.to_int64 x : int64 option)]) ();
    [%expect {| ((hash 2d4c79adf4965bbbcd2bda4c83610b41) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_to_nativeint = struct
  let%test "i = i |> z_of_int |> z_to_int" =
    Core.Quickcheck.test Nativeint.quickcheck_generator ~f:(fun x ->
      assert (Nativeint.equal x (x |> Z.of_nativeint |> Z.to_nativeint_exn)));
    true
  ;;

  let%test "if fitsint i then i = i |> to_z |> of_z" =
    Static.quickcheck
      ~quiet:true
      ~f:(fun x ->
        if Z.fits_nativeint x
        then assert (Z.equal x (x |> Z.to_nativeint_exn |> Z.of_nativeint))
        else ();
        Sexp.Atom "")
      ();
    true
  ;;
end

let unsigned_max bits = Z.(pred (shift_left one bits))

(* Reference semantics, from [external/zarith/src/z.mli] and [ml_to_int32_unsigned] in
   [external/zarith/src/caml_z.c]: [fits_*_unsigned] is "non-negative {i and} fits", and
   [to_*_unsigned] raises [Overflow] on anything that does not fit -- explicitly including
   every negative value.

   Both properties have to hold for small (immediate) and large (boxed) representations
   alike, because the JS and Wasm runtimes special-case immediates: on Wasm a small [Z.t]
   is an [i31], which is where the signed [fits_*] shortcut of "any immediate fits" stops
   being valid once the target type is unsigned. *)
let test_fits_unsigned ~bits ~fits =
  let max = unsigned_max bits in
  let check z ~expect = [%test_result: bool] ~message:(Z.to_string z) ~expect (fits z) in
  (* Non-negative and in range. [zero]/[one] are immediates; [2^(bits-1)] and [max] are
     boxed, so this covers both representations. *)
  check Z.zero ~expect:true;
  check Z.one ~expect:true;
  check (Z.shift_left Z.one (Int.pred bits)) ~expect:true;
  check (Z.pred max) ~expect:true;
  check max ~expect:true;
  (* Non-negative but out of range. *)
  check (Z.succ max) ~expect:false;
  check (Z.mul max max) ~expect:false;
  (* Negative: never fits an unsigned type, whether immediate or boxed. *)
  check Z.minus_one ~expect:false;
  check (Z.of_int (-42)) ~expect:false;
  check (Z.neg max) ~expect:false;
  check (Z.neg (Z.succ max)) ~expect:false
;;

(* [to_] is applied only for effect here, so this stays polymorphic in its result type and
   can be shared by the [int32] / [int64] / [nativeint] conversions. *)
let test_to_unsigned_raises ~bits ~to_ =
  let max = unsigned_max bits in
  let raises z =
    match to_ z with
    | _ -> false
    | exception Z.Overflow -> true
  in
  let check z ~expect =
    [%test_result: bool] ~message:(Z.to_string z) ~expect (raises z)
  in
  (* In range: no exception. *)
  check Z.zero ~expect:false;
  check Z.one ~expect:false;
  check max ~expect:false;
  (* Out of range, and negative. A negative immediate is the case the Wasm runtime used to
     return unchanged instead of raising. *)
  check (Z.succ max) ~expect:true;
  check (Z.mul max max) ~expect:true;
  check Z.minus_one ~expect:true;
  check (Z.of_int (-42)) ~expect:true;
  check (Z.neg max) ~expect:true
;;

module Ml_z_to_int32_unsigned = struct
  let%expect_test "maximum unsigned value" =
    print_s [%sexp (Z.to_int32_unsigned (unsigned_max 32) : int32)];
    [%expect {| -1 |}]
  ;;

  let%expect_test "overflow" =
    test_to_unsigned_raises ~bits:32 ~to_:Z.to_int32_unsigned;
    print_endline "ok";
    [%expect {| ok |}]
  ;;

  (* Widths are fixed for [int32], so unlike [nativeint] these values are the same in
     every backend and can be checked exactly. *)
  let%expect_test "boundaries" =
    List.iter [ "0"; "1"; "2147483647"; "2147483648"; "4294967295" ] ~f:(fun s ->
      let z = Z.of_string s in
      print_s [%message "" ~_:(z : t) ~_:(Z.to_int32_unsigned z : int32)]);
    [%expect
      {|
      (0 0)
      (1 1)
      (2147483647 2147483647)
      (2147483648 -2147483648)
      (4294967295 -1)
      |}]
  ;;
end

module Ml_z_to_int64_unsigned = struct
  let%expect_test "maximum unsigned value" =
    print_s [%sexp (Z.to_int64_unsigned (unsigned_max 64) : int64)];
    [%expect {| -1 |}]
  ;;

  let%expect_test "overflow" =
    test_to_unsigned_raises ~bits:64 ~to_:Z.to_int64_unsigned;
    print_endline "ok";
    [%expect {| ok |}]
  ;;

  let%expect_test "boundaries" =
    List.iter
      [ "0"; "1"; "9223372036854775807"; "9223372036854775808"; "18446744073709551615" ]
      ~f:(fun s ->
        let z = Z.of_string s in
        print_s [%message "" ~_:(z : t) ~_:(Z.to_int64_unsigned z : int64)]);
    [%expect
      {|
      (0 0)
      (1 1)
      (9223372036854775807 9223372036854775807)
      (9223372036854775808 -9223372036854775808)
      (18446744073709551615 -1)
      |}]
  ;;
end

module Ml_z_to_nativeint_unsigned = struct
  (* The width varies by backend, but the all-ones result does not: it is [-1] read as a
     signed [nativeint] whether that is 32 or 64 bits wide. So this one can still be
     printed explicitly. *)
  let%expect_test "maximum unsigned value" =
    print_s
      [%sexp (Z.to_nativeint_unsigned (unsigned_max Stdlib.Nativeint.size) : nativeint)];
    [%expect {| -1 |}]
  ;;

  (* Driven off [Nativeint.size] rather than a hard-coded width: [nativeint] is 32 bits
     for js_of_ocaml but can be 64 bits on Wasm. This is what stops the runtime's choice
     of width from drifting away from the width OCaml sees. *)
  let%expect_test "overflow" =
    test_to_unsigned_raises ~bits:Stdlib.Nativeint.size ~to_:Z.to_nativeint_unsigned;
    print_endline "ok";
    [%expect {| ok |}]
  ;;
end

module Ml_z_fits_int32_unsigned = struct
  let%expect_test "unsigned range" =
    test_fits_unsigned ~bits:32 ~fits:Z.fits_int32_unsigned;
    print_endline "ok";
    [%expect {| ok |}]
  ;;
end

module Ml_z_fits_int64_unsigned = struct
  let%expect_test "unsigned range" =
    test_fits_unsigned ~bits:64 ~fits:Z.fits_int64_unsigned;
    print_endline "ok";
    [%expect {| ok |}]
  ;;
end

module Ml_z_fits_nativeint_unsigned = struct
  let%expect_test "unsigned range" =
    test_fits_unsigned ~bits:Stdlib.Nativeint.size ~fits:Z.fits_nativeint_unsigned;
    print_endline "ok";
    [%expect {| ok |}]
  ;;

  (* [fits_nativeint_unsigned] must agree with [fits_int32_unsigned] or
     [fits_int64_unsigned] according to [Nativeint.size], so that the delegation the JS
     runtime performs is checked rather than assumed. *)
  let%expect_test "width agrees with [Nativeint.size]" =
    let equivalent_to =
      match Stdlib.Nativeint.size with
      | 32 -> Z.fits_int32_unsigned
      | 64 -> Z.fits_int64_unsigned
      | other -> raise_s [%message "unexpected [Nativeint.size]" (other : int)]
    in
    List.iter
      [ Z.zero
      ; Z.one
      ; Z.minus_one
      ; unsigned_max Stdlib.Nativeint.size
      ; Z.succ (unsigned_max Stdlib.Nativeint.size)
      ; Z.neg (unsigned_max Stdlib.Nativeint.size)
      ]
      ~f:(fun z ->
        [%test_result: bool]
          ~message:(Z.to_string z)
          ~expect:(equivalent_to z)
          (Z.fits_nativeint_unsigned z));
    print_endline "ok";
    [%expect {| ok |}]
  ;;
end

module To_float = struct
  let%expect_test "print (Z.to_float x)" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (Z.to_float x : float)]) ();
    [%expect {| ((hash f9142dbb58637f3d8a7f34d5164e2fef) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_fits_int = struct
  let fits_test_helper a = Z.fits_int (Z.of_string a) |> printf "%b"

  let%expect_test "fits_int" =
    fits_test_helper "1";
    [%expect "true"]
  ;;

  (* 'int' in js_of_ocaml is 32 bits, so the fits_int32 tests are sufficient. *)
end

module Ml_z_fits_int32 = struct
  let fits_test_helper a = Z.fits_int32 (Z.of_string a) |> printf "%b"

  let%expect_test "fits_int" =
    fits_test_helper "1";
    [%expect "true"]
  ;;

  let%expect_test "print x, fits_int32 x" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (fits_int32 x : bool)]) ();
    [%expect "((hash db765bf400e9c6d1e2b418f6891e51a8) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_fits_int64 = struct
  let fits_test_helper a = Z.fits_int64 (Z.of_string a) |> printf "%b"

  let%expect_test "fits_int" =
    fits_test_helper "1";
    [%expect "true"];
    fits_test_helper "9223372036854775807";
    [%expect "true"];
    fits_test_helper "9223372036854775808";
    [%expect "false"];
    fits_test_helper "-9223372036854775808";
    [%expect "true"];
    fits_test_helper "-9223372036854775809";
    [%expect "false"]
  ;;

  let%expect_test "print x, fits_int64 x" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (fits_int64 x : bool)]) ();
    [%expect "((hash 6e86cc7603c6284295d7ac5111d575d7) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_fits_nativeint = struct
  let fits_test_helper a = Z.fits_nativeint (Z.of_string a) |> printf "%b"

  let%expect_test "fits_int" =
    fits_test_helper "1";
    [%expect "true"]
  ;;

  let%expect_test ("fits_nativeint and to_nativeint agree at 2^63-1"
    [@tags "64-bits-only"])
    =
    let max_64b_signed = Z.of_int64_exn Int64.((one lsl 63) - one) in
    assert (Z.fits_nativeint max_64b_signed);
    print_endline (Z.to_nativeint_exn max_64b_signed |> Nativeint.to_string);
    [%expect {| 9223372036854775807 |}]
  ;;
end

module Ml_z_of_float = struct
  let%expect_test "of_float" =
    let of_float_test_helper a = Z.of_float a |> Z.print in
    of_float_test_helper 3.3;
    [%expect "3"];
    of_float_test_helper (-3.3);
    [%expect "-3"]
  ;;

  let%expect_test "float conversions" =
    Core.Quickcheck.test Core.Float.quickcheck_generator ~f:(fun f ->
      if Float.is_finite f
      then
        [%test_eq: float] (f |> Float.round_towards_zero) (f |> Z.of_float |> Z.to_float)
      else (
        try
          let (_ : t) = Z.of_float f in
          failwith "failed to fail"
        with
        | Overflow -> ()
        | _ -> failwith "failed unexpectedly"))
  ;;

  let print_or_print_exn x =
    try Z.of_float x |> Z.to_string |> print_endline with
    | Overflow -> print_endline "overflow"
  ;;

  let%expect_test "of_float nan" =
    print_or_print_exn Float.nan;
    [%expect {| overflow |}]
  ;;

  let%expect_test "of_float infinity" =
    print_or_print_exn Float.infinity;
    [%expect {| overflow |}];
    print_or_print_exn Float.neg_infinity;
    [%expect {| overflow |}]
  ;;

  let%expect_test "of_float x" =
    print_or_print_exn 1.0;
    [%expect {| 1 |}];
    print_or_print_exn 1.01;
    [%expect {| 1 |}];
    print_or_print_exn 1.99;
    [%expect {| 1 |}];
    print_or_print_exn (-1.0);
    [%expect {| -1 |}];
    print_or_print_exn (-1.01);
    [%expect {| -1 |}];
    print_or_print_exn (-1.99);
    [%expect {| -1 |}];
    print_or_print_exn 1e30;
    [%expect {| 1000000000000000019884624838656 |}];
    print_or_print_exn 1e308;
    [%expect
      {| 100000000000000001097906362944045541740492309677311846336810682903157585404911491537163328978494688899061249669721172515611590283743140088328307009198146046031271664502933027185697489699588559043338384466165001178426897626212945177628091195786707458122783970171784415105291802893207873272974885715430223118336 |}];
    print_or_print_exn 1e309;
    [%expect {| overflow |}];
    print_or_print_exn (-1e30);
    [%expect {| -1000000000000000019884624838656 |}];
    print_or_print_exn (-1e308);
    [%expect
      {| -100000000000000001097906362944045541740492309677311846336810682903157585404911491537163328978494688899061249669721172515611590283743140088328307009198146046031271664502933027185697489699588559043338384466165001178426897626212945177628091195786707458122783970171784415105291802893207873272974885715430223118336 |}];
    print_or_print_exn (-1e309);
    [%expect {| overflow |}]
  ;;
end
