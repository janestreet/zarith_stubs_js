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

  (* 'nativeint' in js_of_ocaml is 32 bits, so the fits_int32 tests are sufficient. *)
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
