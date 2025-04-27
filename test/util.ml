open! Core
open! Z

let sexp_of_t t = Sexp.Atom (Z.to_string t)
let is_zero a = equal zero a

module Dynamic = struct
  let sexp_of = sexp_of_t
  let trials = 100

  let get_generator_and_shrinker ~include_zero =
    let generator = Z.For_quickcheck.quickcheck_generator in
    let shrinker = Z.For_quickcheck.quickcheck_shrinker in
    if include_zero
    then generator, shrinker
    else (
      let generator =
        Quickcheck.Generator.filter generator ~f:(fun a -> not (is_zero a))
      in
      generator, shrinker)
  ;;

  let quickcheck ?(include_zero = true) ?(trials = trials) ~f () =
    let generator, shrinker = get_generator_and_shrinker ~include_zero in
    Quickcheck.test generator ~trials ~sexp_of ~shrinker ~f
  ;;

  let quickcheck_pair ?(include_zero = false) ~f () =
    let generator, shrinker = get_generator_and_shrinker ~include_zero in
    let generator = Quickcheck.Generator.tuple2 generator generator in
    let shrinker = Quickcheck.Shrinker.tuple2 shrinker shrinker in
    let sexp_of = Tuple2.sexp_of_t sexp_of sexp_of in
    let f (a, b) = f a b in
    Quickcheck.test generator ~trials ~sexp_of ~shrinker ~f
  ;;

  let quickcheck_tripple ?(include_zero = false) ~f () =
    let generator, shrinker = get_generator_and_shrinker ~include_zero in
    let generator = Quickcheck.Generator.tuple3 generator generator generator in
    let shrinker = Quickcheck.Shrinker.tuple3 shrinker shrinker shrinker in
    let sexp_of = Tuple3.sexp_of_t sexp_of sexp_of sexp_of in
    let f (a, b, c) = f a b c in
    Quickcheck.test generator ~trials ~sexp_of ~shrinker ~f
  ;;
end

module Static = struct
  (** For non-random input, the functions in this module read from the
      "stable-bigints.txt" file, run the requested computation, and then sexpify the
      results and print the hash of that result. This is here solely to verify that the
      Javascript and C implementations produce the same (hashed) output.

      If a test fails, toggle the [?verbose] argument to [true] and diff the output. *)

  let () =
    let regenerate_stable_quickcheck_file =
      match Sys.getenv "zarith_test_generate_sample" with
      | Some "true" -> true
      | Some _ ->
        failwith "zarith_test_generate_sample should be set to true or not set at all"
      | None -> false
    in
    if regenerate_stable_quickcheck_file
    then (
      let q = Queue.create ~capacity:1024 () in
      while Int.( < ) (Queue.length q) 1024 do
        Dynamic.quickcheck ~trials:1024 () ~f:(fun x -> Queue.enqueue q x)
      done;
      Out_channel.with_file "stable_bigints.ml" ~f:(fun oc ->
        Out_channel.output_string oc "let all =\n";
        Queue.iteri q ~f:(fun i z ->
          if Int.equal i 0
          then Out_channel.output_string oc (sprintf "  [| %S\n" (Z.to_string z))
          else Out_channel.output_string oc (sprintf "   ; %S\n" (Z.to_string z)));
        Out_channel.output_string oc "  |]\n;;\n"))
    else ()
  ;;

  let static_bigints = Stable_bigints.all |> Array.to_list |> List.map ~f:Z.of_string
  let bigint_seq_1 ~filter = List.filter static_bigints ~f:filter

  let bigint_seq_2 ~filter =
    let bigints = bigint_seq_1 ~filter in
    let a = List.drop bigints 0 in
    let b = List.drop bigints 1 in
    let zipped, _ = List.zip_with_remainder b a in
    zipped
  ;;

  let bigint_seq_3 ~filter =
    let bigints = bigint_seq_1 ~filter in
    let a = List.drop bigints 0 in
    let b = List.drop bigints 1 in
    let c = List.drop bigints 2 in
    let zipped, _ = List.zip_with_remainder a b in
    let zipped, _ = List.zip_with_remainder zipped c in
    List.map zipped ~f:(fun ((a, b), c) -> a, b, c)
  ;;

  let[@warning "-16"] qc ?(quiet = false) ~target ~verbose ~f =
    let q = Queue.create () in
    let target = target in
    List.iter target ~f:(fun a ->
      let result =
        List.map f ~f:(fun f ->
          try f a with
          | Division_by_zero -> Sexp.Atom "division-by-zero"
          | ex -> Sexp.Atom (Core.Exn.to_string ex))
      in
      Queue.enqueue_all q result);
    let all_sexps = Queue.to_list q in
    let big_sexp =
      all_sexps |> List.map ~f:Sexp.to_string_hum |> String.concat ~sep:"\n"
    in
    let set_of_sexps = Sexp.Set.of_list all_sexps in
    let uniqueness_rate =
      (set_of_sexps |> Set.length |> Float.of_int)
      /. (all_sexps |> List.length |> Float.of_int)
      *. 100.0
    in
    if verbose then print_endline big_sexp else ();
    let hash = big_sexp |> Md5_lib.string |> Md5_lib.to_hex in
    if not quiet then print_s [%message (hash : string) (uniqueness_rate : Float.Terse.t)]
  ;;

  let quickcheck ?quiet ?(verbose = false) ?(filter = Fn.const true) ~f () =
    qc ?quiet ~target:(bigint_seq_1 ~filter) ~verbose ~f:[ (fun a -> f a) ]
  ;;

  let quickcheck_pair ?quiet ?(verbose = false) ?(filter = Fn.const true) ~f () =
    let g m x =
      let a, b = m x in
      f a b
    in
    let permutation = [ Fn.id; Tuple2.swap ] in
    qc
      ?quiet
      ~target:(bigint_seq_2 ~filter)
      ~verbose
      ~f:(List.map permutation ~f:(fun p -> g p))
  ;;

  let quickcheck_tripple ?quiet ?(verbose = false) ?(filter = Fn.const true) ~f () =
    let g m x =
      let a, b, c = m x in
      f a b c
    in
    let permutation =
      [ (fun (a, b, c) -> a, b, c)
      ; (fun (a, b, c) -> a, c, b)
      ; (fun (a, b, c) -> b, a, c)
      ; (fun (a, b, c) -> b, c, a)
      ; (fun (a, b, c) -> c, a, b)
      ; (fun (a, b, c) -> c, b, a)
      ]
    in
    qc
      ?quiet
      ~target:(bigint_seq_3 ~filter)
      ~verbose
      ~f:(List.map permutation ~f:(fun p -> g p))
  ;;
end

module Filter = struct
  type nonrec t = t -> bool

  let not_zero = Fn.non is_zero
  let positive x = x > zero
  let small x = abs x < of_string "1024"
  let combine fs a = List.for_all fs ~f:(fun f -> f a)
  let odd x = equal one (bit_and x one)
end
