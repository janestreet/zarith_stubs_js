open Core
open Zarith
include Zarith.Z

module Redefinitions = struct
  let ( - ) = Z.( - )
  let ( + ) = Z.( + )
  let ( * ) = Z.( * )
  let ( / ) = Z.( / )
  let rem = Z.rem
  let ( ~- ) = Z.( ~- )
  let neg = Z.neg
  let abs = Z.abs
  let succ = Z.succ
  let pred = Z.pred
  let equal = Z.equal
  let ( = ) = Z.equal
  let ( < ) = Z.lt
  let ( > ) = Z.gt
  let ( <= ) = Z.leq
  let ( >= ) = Z.geq
  let max = Z.max
  let min = Z.min
  let ascending = compare
  let shift_right = Z.shift_right
  let shift_left = Z.shift_left
  let bit_not = Z.lognot
  let bit_xor = Z.logxor
  let bit_or = Z.logor
  let bit_and = Z.logand
  let ( land ) = bit_and
  let ( lor ) = bit_or
  let ( lxor ) = bit_xor
  let lnot = bit_not
  let ( lsl ) = shift_left
  let ( asr ) = shift_right
  let of_int = Z.of_int
  let of_int32 = Z.of_int32
  let of_int64 = Z.of_int64
  let of_nativeint = Z.of_nativeint
  let of_float_unchecked = Z.of_float
  let of_float = Z.of_float
  let of_int_exn = of_int
  let of_int32_exn = of_int32
  let of_int64_exn = of_int64
  let of_nativeint_exn = of_nativeint
  let to_int_exn = Z.to_int
  let to_int32_exn = Z.to_int32
  let to_int64_exn = Z.to_int64
  let to_nativeint_exn = Z.to_nativeint
  let to_float = Z.to_float
  let zero = Z.zero
  let one = Z.one
  let minus_one = Z.minus_one
  let to_int t = if Z.fits_int t then Some (Z.to_int t) else None
  let to_int32 t = if Z.fits_int32 t then Some (Z.to_int32 t) else None
  let to_int64 t = if Z.fits_int64 t then Some (Z.to_int64 t) else None
  let to_nativeint t = if Z.fits_nativeint t then Some (Z.to_nativeint t) else None
  let ( <> ) x y = not (equal x y)
  let incr cell = cell := succ !cell
  let decr cell = cell := pred !cell
  let pow x y = Z.pow x (to_int_exn y)
  let ( ** ) x y = pow x y
  let popcount x = Z.popcount x
  let to_string = Z.to_string

  let ( /% ) x y =
    if Int.( >= ) (Z.sign y) 0
    then Z.ediv x y
    else
      failwithf
        "%s.(%s /%% %s) : divisor must be positive"
        "Zarith_kernel"
        (to_string x)
        (to_string y)
        ()
  ;;

  let ( % ) x y =
    if Int.( >= ) (Z.sign y) 0
    then Z.erem x y
    else
      failwithf
        "%s.(%s %% %s) : divisor must be positive"
        "Zarith_kernel"
        (to_string x)
        (to_string y)
        ()
  ;;

  let hash_fold_t state t = Int.hash_fold_t state (Z.hash t)
end

include Redefinitions

module Make_random (State : sig
    type t

    val bits : t -> int
    val int : t -> int -> int
  end) : sig
  val random : state:State.t -> t -> t
end = struct
  (* Uniform random generation of Bigint values.

     [random ~state range] chooses a [depth] and generates random values using
     [Random.State.bits state], called [1 lsl depth] times and concatenated. The
     preliminary result [n] therefore satisfies [0 <= n < 1 lsl (30 lsl depth)].

     In order for the random choice to be uniform between [0] and [range-1], there must
     exist [k > 0] such that [n < k * range <= 1 lsl (30 lsl depth)]. If so, [n % range]
     is returned. Otherwise the random choice process is repeated from scratch.

     The [depth] value is chosen so that repeating is uncommon (1 in 1,000 or less). *)

  let bits_at_depth ~depth = Int.shift_left 30 depth
  let range_at_depth ~depth = shift_left one (bits_at_depth ~depth)

  let rec choose_bit_depth_for_range_from ~range ~depth =
    if range_at_depth ~depth >= range
    then depth
    else choose_bit_depth_for_range_from ~range ~depth:(Int.succ depth)
  ;;

  let choose_bit_depth_for_range ~range = choose_bit_depth_for_range_from ~range ~depth:0

  let rec random_bigint_at_depth ~state ~depth =
    if Int.equal depth 0
    then of_int (State.bits state)
    else (
      let prev_depth = Int.pred depth in
      let prefix = random_bigint_at_depth ~state ~depth:prev_depth in
      let suffix = random_bigint_at_depth ~state ~depth:prev_depth in
      bit_or (shift_left prefix (bits_at_depth ~depth:prev_depth)) suffix)
  ;;

  let random_value_is_uniform_in_range ~range ~depth n =
    let k = range_at_depth ~depth / range in
    n < k * range
  ;;

  let rec large_random_at_depth ~state ~range ~depth =
    let result = random_bigint_at_depth ~state ~depth in
    if random_value_is_uniform_in_range ~range ~depth result
    then result % range
    else large_random_at_depth ~state ~range ~depth
  ;;

  let large_random ~state ~range =
    let tolerance_factor = of_int 1_000 in
    let depth = choose_bit_depth_for_range ~range:(range * tolerance_factor) in
    large_random_at_depth ~state ~range ~depth
  ;;

  let random ~state range =
    if range <= zero
    then
      failwithf "Bigint.random: argument %s <= 0" (to_string range) ()
      (* Note that it's not safe to do [1 lsl 30] on a 32-bit machine (with 31-bit signed
         integers) *)
    else if range < shift_left one 30
    then of_int (State.int state (to_int_exn range))
    else large_random ~state ~range
  ;;
end

module Random_internal = Make_random (Random.State)

module For_quickcheck : sig
  include Quickcheckable.S_int with type t := t

  val gen_negative : t Quickcheck.Generator.t
  val gen_positive : t Quickcheck.Generator.t
end = struct
  module Generator = Quickcheck.Generator
  open Generator.Let_syntax

  module Uniform = Make_random (struct
      type t = Splittable_random.t

      let int t range = Splittable_random.int t ~lo:0 ~hi:(Int.pred range)
      let bits t = int t (Int.shift_left 1 30)
    end)

  let random_uniform ~state lo hi = lo + Uniform.random ~state (succ (hi - lo))

  let gen_uniform_incl lower_bound upper_bound =
    if lower_bound > upper_bound
    then (
      let lower_bound = to_string lower_bound in
      let upper_bound = to_string upper_bound in
      raise_s
        [%message
          "Bigint.gen_uniform_incl: bounds are crossed"
            (lower_bound : string)
            (upper_bound : string)]);
    Generator.create (fun ~size:_ ~random:state ->
      random_uniform ~state lower_bound upper_bound)
  ;;

  let gen_incl lower_bound upper_bound =
    Generator.weighted_union
      [ 0.05, Generator.return lower_bound
      ; 0.05, Generator.return upper_bound
      ; 0.9, gen_uniform_incl lower_bound upper_bound
      ]
  ;;

  let min_represented_by_n_bits n =
    if Int.equal n 0 then zero else shift_left one (Int.pred n)
  ;;

  let max_represented_by_n_bits n = pred (shift_left one n)

  let gen_log_uniform_incl lower_bound upper_bound =
    if lower_bound < zero || lower_bound > upper_bound
    then (
      let lower_bound = to_string lower_bound in
      let upper_bound = to_string upper_bound in
      raise_s
        [%message
          "Bigint.gen_log_incl: invalid bounds"
            (lower_bound : string)
            (upper_bound : string)]);
    let min_bits = Z.numbits lower_bound in
    let max_bits = Z.numbits upper_bound in
    let%bind bits = Int.gen_uniform_incl min_bits max_bits in
    gen_uniform_incl
      (max lower_bound (min_represented_by_n_bits bits))
      (min upper_bound (max_represented_by_n_bits bits))
  ;;

  let gen_log_incl lower_bound upper_bound =
    Generator.weighted_union
      [ 0.05, Generator.return lower_bound
      ; 0.05, Generator.return upper_bound
      ; 0.9, gen_log_uniform_incl lower_bound upper_bound
      ]
  ;;

  let gen_positive =
    let%bind extra_bytes = Generator.size in
    let num_bytes = Int.succ extra_bytes in
    let num_bits = Int.( * ) num_bytes 8 in
    gen_log_uniform_incl one (pred (shift_left one num_bits))
  ;;

  let gen_negative = Generator.map gen_positive ~f:neg

  let quickcheck_generator =
    Generator.weighted_union
      [ 0.45, gen_positive; 0.1, Generator.return zero; 0.45, gen_negative ]
  ;;

  let quickcheck_observer =
    Quickcheck.Observer.create (fun t ~size:_ ~hash -> hash_fold_t hash t)
  ;;

  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
end
