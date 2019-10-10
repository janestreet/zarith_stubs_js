open! Core_kernel
open! Import

module Ml_z_of_substring_base = struct
  let%expect_test "print of_base (format x)" =
    Static.quickcheck
      ~f:(fun x ->
        [ 2, "b"; 8, "o"; 10, "d"; 16, "x" ]
        |> List.map ~f:(fun (i, f) ->
          let string = Z.format f x in
          let string_dropped n =
            let n = Int.min n (String.length string) in
            String.sub string ~pos:n ~len:(Int.( - ) (String.length string) n)
          in
          try
            [%message
              (i : int)
                (string : string)
                (string_dropped 2 : string)
                (Z.of_substring_base i (string_dropped 2) ~pos:1 ~len:4 : t)
                (Z.of_substring_base 0 string ~pos:0 ~len:4 : t)]
          with
          | exn -> Sexp.Atom (Exn.to_string exn))
        |> Sexp.List)
      ();
    [%expect
      {|
      ((hash 6e19ad54e13932775b7dcb252f2460c6) (uniqueness_rate 81.25))|}]
  ;;

  let%expect_test "print of_base (format x)" =
    Static.quickcheck
      ~f:(fun x ->
        [ 2, "b"; 8, "o"; 10, "d"; 16, "x" ]
        |> List.map ~f:(fun (i, f) ->
          let formatted = Z.format f x in
          let parsed = Z.of_string_base i formatted in
          [%message
            (x : t) (i : int) (f : string) (formatted : string) (parsed : t)])
        |> Sexp.List)
      ();
    [%expect
      {|
      ((hash 5accf29c4669d527bd5779447b54dab1) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_to_bits = struct
  let to_bits a =
    let r = Z.to_bits a in
    match Sys.word_size with
    | 64 -> Option.value ~default:r (String.chop_suffix r ~suffix:"\000\000\000\000")
    | _ -> r
  ;;

  let to_bits_test_helper x = print_string (to_bits (Z.of_string x))

  let%expect_test "to_bits" =
    to_bits_test_helper "1234567";
    [%expect "\135\214\018\000"]
  ;;

  let%expect_test "to_bits" =
    to_bits_test_helper "-1234567";
    [%expect "\135\214\018\000"]
  ;;

  let%expect_test "to_bits" =
    to_bits_test_helper "316049152";
    [%expect "\000\135\214\018"]
  ;;

  let%expect_test "print x, (to_bits x)" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (to_bits x : string)]) ();
    [%expect
      {|
      ((hash 8941f3eca65e7d039f6d987683e3803b) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_of_bits = struct
  let of_bits_test_helper a = Z.of_bits a |> Z.print

  let%expect_test "of_bits" =
    of_bits_test_helper "\135\214\018\000\000\000\000\000";
    [%expect "1234567"]
  ;;

  let%test "assert (abs x) = (of_bits (to_bits x))" =
    Dynamic.quickcheck () ~f:(fun x -> [%test_eq: t] (abs x) (of_bits (to_bits x)));
    true
  ;;
end

module Ml_z_format = struct
  let%expect_test "format '%d' x" =
    Static.quickcheck
      ~f:(fun x ->
        let str = Z.format "%d" x in
        [%message str])
      ();
    [%expect
      {|
      ((hash cf890da293ccebc1e11c9ff4eec88058) (uniqueness_rate 85.742188))|}]
  ;;

  let%expect_test "format '%x' x" =
    Static.quickcheck
      ~f:(fun x ->
        let str = Z.format "%x" x in
        [%message str])
      ();
    [%expect
      {|
      ((hash 0db4620a008e5958554fff85f7bc950a) (uniqueness_rate 85.742188))|}]
  ;;

  let%expect_test "permute formats" =
    let combinations =
      let open List.Let_syntax in
      let%bind flag = [ ""; "+"; " "; "0"; "#" ] in
      let%bind width = [ ""; "0"; "10"; "100" ] in
      let%bind typ = [ "i"; "d"; "u"; "b"; "o"; "x"; "X" ] in
      return (sprintf "%%%s%s%s" flag width typ)
    in
    Static.quickcheck
      ~f:(fun x ->
        combinations
        |> List.map ~f:(fun f -> f, Z.format f x)
        |> List.map ~f:[%sexp_of: string * string]
        |> Sexp.List)
      ();
    [%expect "((hash d8e5286d828a22da8141249fc86185be) (uniqueness_rate 85.742188))"]
  ;;
end

module Marshal = struct
  let m x =
    let y = of_string x in
    let marshal = Marshal.to_string y [] in
    print_endline
      (String.to_list marshal
       |> List.map ~f:(fun x -> sprintf "%02X" (Char.to_int x))
       |> List.chunks_of ~length:8
       |> List.map ~f:(String.concat ~sep:",")
       |> String.concat ~sep:"\n");
    let y' : Z.t = Marshal.from_string marshal 0 in
    if not (equal y y') then print_endline "(roundtrip failed)"
  ;;

  let%expect_test "marshal large" =
    m "8432103214690897934401335661952849215774309719238";
    [%expect
      {|
        84,95,A6,BE,00,00,00,2D
        00,00,00,01,00,00,00,09
        00,00,00,06,18,5F,7A,00
        00,00,00,1C,00,00,00,00
        00,00,00,20,00,00,00,00
        18,C6,9C,63,3B,6A,F2,2A
        E0,41,A2,BE,0F,2B,5F,8B
        0C,CC,95,FC,C4,05,00,00
        00 |}];
    m "-107549090292258971570605440155";
    [%expect
      {|
        84,95,A6,BE,00,00,00,25
        00,00,00,01,00,00,00,07
        00,00,00,05,18,5F,7A,00
        00,00,00,14,00,00,00,00
        00,00,00,18,01,00,00,00
        10,9B,BC,2C,E8,CF,9C,72
        2F,BB,85,82,5B,01,00,00
        00 |}];
    m "107549090292258971570605440155";
    [%expect
      {|
        84,95,A6,BE,00,00,00,25
        00,00,00,01,00,00,00,07
        00,00,00,05,18,5F,7A,00
        00,00,00,14,00,00,00,00
        00,00,00,18,00,00,00,00
        10,9B,BC,2C,E8,CF,9C,72
        2F,BB,85,82,5B,01,00,00
        00 |}]
  ;;

  let%expect_test ("marshal mix len"[@tags "64-bits-only"]) =
    m "-549389047489539543158";
    [%expect
      {|
        84,95,A6,BE,00,00,00,25
        00,00,00,01,00,00,00,07
        00,00,00,05,18,5F,7A,00
        00,00,00,14,00,00,00,00
        00,00,00,18,01,00,00,00
        10,76,10,37,60,E7,FB,4D
        C8,1D,00,00,00,00,00,00
        00 |}]
  ;;

  let%expect_test ("marshal mix len"[@tags "32-bits-only"]) =
    m "-549389047489539543158";
    [%expect
      {|
        84,95,A6,BE,00,00,00,21
        00,00,00,01,00,00,00,06
        00,00,00,05,18,5F,7A,00
        00,00,00,10,00,00,00,00
        00,00,00,18,01,00,00,00
        0C,76,10,37,60,E7,FB,4D
        C8,1D,00,00,00 |}]
  ;;

  let%expect_test ("marshal mix len"[@tags "js-only"]) =
    m "-549389047489539543158";
    [%expect
      {|
        84,95,A6,BE,00,00,00,21
        00,00,00,01,00,00,00,06
        00,00,00,05,18,5F,7A,00
        00,00,00,10,00,00,00,00
        00,00,00,18,01,00,00,00
        0C,76,10,37,60,E7,FB,4D
        C8,1D,00,00,00 |}]
  ;;

  let%expect_test "marshal small" =
    m "0";
    [%expect
      {|
        84,95,A6,BE,00,00,00,01
        00,00,00,00,00,00,00,00
        00,00,00,00,40 |}]
  ;;

  let%expect_test "marshal roundtrip" =
    Static.quickcheck
      ~verbose:false
      ~f:(fun x ->
        let str = Marshal.to_string x [] in
        let y = Marshal.from_string str 0 in
        [%message (x : t) (y : t) (equal x y : bool)])
      ();
    [%expect
      {|
      ((hash c9b9d5441e3470cb51b8114f77ea91d8) (uniqueness_rate 85.742188))|}]
  ;;
end
