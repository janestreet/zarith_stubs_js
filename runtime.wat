(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "js" "wasm_z_neg"
      (func $neg (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_add"
      (func $add (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_sub"
      (func $sub (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_mul"
      (func $mul (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_div"
      (func $div (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_cdiv"
      (func $cdiv (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_fdiv"
      (func $fdiv (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_rem"
      (func $rem (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_size"
      (func $size (param (ref any)) (result i32)))
   (import "js" "wasm_z_numbits"
      (func $numbits (param (ref any)) (result i32)))
   (import "js" "wasm_z_abs"
      (func $abs (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_logand"
      (func $logand (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_logor"
      (func $logor (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_logxor"
      (func $logxor (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_lognot"
      (func $lognot (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_shift_left"
      (func $shift_left (param (ref any)) (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_shift_right"
      (func $shift_right (param (ref any)) (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_shift_right_trunc"
      (func $shift_right_trunc
         (param (ref any)) (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_of_float"
      (func $of_float (param f64) (result (ref any))))
   (import "js" "wasm_z_of_int32"
      (func $of_int32 (param i32) (result (ref any))))
   (import "js" "wasm_z_normalize"
      (func $of_int64 (param i64) (result (ref any))))
   (import "js" "wasm_z_to_int32"
      (func $to_int32 (param (ref any)) (result i32)))
   (import "js" "wasm_z_to_int64"
      (func $to_int64 (param (ref any)) (result i64)))
   (import "js" "wasm_z_testbit"
      (func $testbit (param (ref any)) (param (ref eq)) (result i32)))
   (import "js" "wasm_z_compare"
      (func $compare (param (ref any)) (param (ref any)) (result i32)))
   (import "js" "wasm_z_equal"
      (func $equal (param (ref any)) (param (ref any)) (result i32)))
   (import "js" "wasm_z_positive"
      (func $positive (param (ref any)) (result i32)))
   (import "js" "wasm_z_gcd"
      (func $gcd (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_fits_int32"
      (func $fits_int32 (param (ref any)) (result i32)))
   (import "js" "wasm_z_fits_int64"
      (func $fits_int64 (param (ref any)) (result i32)))
   (import "js" "wasm_z_powm"
      (func $powm
         (param (ref any)) (param (ref any)) (param (ref any))
         (result (ref any))))
   (import "js" "wasm_z_pow"
      (func $pow (param (ref any)) (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_format"
      (func $format (param anyref) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_of_js_string_base"
      (func $of_js_string_base
         (param (ref eq)) (param anyref) (result anyref)))
   (import "js" "wasm_z_hash"
      (func $hash (param (ref func)) (param (ref any)) (result i32)))
   (import "js" "wasm_z_to_bits"
      (func $to_bits (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_of_bits"
      (func $of_bits (param anyref) (result (ref any))))
   (import "js" "wasm_z_root"
      (func $root (param (ref any)) (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_invert"
      (func $invert (param (ref any)) (param (ref any)) (result anyref)))
   (import "js" "wasm_z_perfect_power"
      (func $perfect_power (param (ref any)) (result i32)))
   (import "js" "wasm_z_perfect_square"
      (func $perfect_square (param (ref any)) (result i32)))
   (import "js" "wasm_z_probab_prime"
      (func $probab_prime (param (ref any)) (param (ref eq)) (result i32)))
   (import "js" "wasm_z_nextprime"
      (func $next_prime (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_extract"
      (func $extract
         (param (ref any)) (param (ref eq)) (param (ref eq))
         (result (ref any))))
   (import "js" "wasm_z_gcdext_intern"
      (func $gcdext_intern
         (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_trailing_zeros"
      (func $trailing_zeroes (param (ref any)) (result i32)))
   (import "js" "wasm_z_popcount"
      (func $popcount (param (ref any)) (result i32)))
   (import "js" "wasm_z_hamdist"
      (func $hamdist (param (ref any)) (param (ref any)) (result i32)))
   (import "js" "wasm_z_divisible"
      (func $divisible (param (ref any)) (param (ref any)) (result i32)))
   (import "js" "wasm_z_remove"
      (func $remove (param (ref any)) (param (ref any)) (result (ref any))))
   (import "js" "wasm_z_fac" (func $fac (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_fac2" (func $fac2 (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_facM"
      (func $facM (param (ref eq)) (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_fib" (func $fib (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_lucnum"
      (func $lucnum (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_jacobi"
      (func $jacobi (param (ref any)) (param (ref any)) (result i32)))
   (import "js" "wasm_z_primorial"
      (func $primorial (param i32) (result (ref any))))
   (import "js" "wasm_z_bin"
      (func $bin (param (ref any)) (param (ref eq)) (result (ref any))))
   (import "js" "wasm_z_serialize"
      (func $serialize (param (ref func)) (param (ref eq)) (param (ref any))))
   (import "js" "wasm_z_deserialize"
      (func $deserialize
         (param (ref func)) (param (ref eq)) (param i32) (param i32)
         (result (ref any))))
   (import "env" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "env" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "env" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "env" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "env" "jsstring_of_substring"
      (func $jsstring_of_substring
         (param (ref $string)) (param i32) (param i32) (result anyref)))
   (import "env" "caml_string_of_jsbytes"
      (func $caml_string_of_jsbytes (param (ref eq)) (result (ref eq))))
   (import "env" "caml_jsbytes_of_string"
      (func $caml_jsbytes_of_string (param (ref eq)) (result (ref eq))))
   (import "env" "caml_js_get"
      (func $caml_js_get (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "env" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "env" "caml_raise_constant"
      (func $caml_raise_constant (param (ref eq))))
   (import "env" "caml_raise_zero_divide" (func $caml_raise_zero_divide))
   (import "env" "caml_named_value"
      (func $caml_named_value (param (ref eq)) (result (ref null eq))))
   (import "env" "caml_copy_int32"
      (func $caml_copy_int32 (param i32) (result (ref eq))))
   (import "env" "caml_copy_nativeint"
      (func $caml_copy_nativeint (param i32) (result (ref eq))))
   (import "env" "caml_copy_int64"
      (func $caml_copy_int64 (param i64) (result (ref eq))))
   (import "env" "Double_val" (func $Double_val (param (ref eq)) (result f64)))
   (import "env" "Int32_val" (func $Int32_val (param (ref eq)) (result i32)))
   (import "env" "Nativeint_val"
      (func $Nativeint_val (param (ref eq)) (result i32)))
   (import "env" "Int64_val" (func $Int64_val (param (ref eq)) (result i64)))
   (import "env" "caml_hash_mix_int"
      (func $caml_hash_mix_int (param i32) (param i32) (result i32)))
   (import "env" "caml_hash_mix_final"
      (func $caml_hash_mix_final (param i32) (result i32)))
   (import "env" "caml_register_custom_operations"
      (func $caml_register_custom_operations (param (ref $custom_operations))))
   (import "env" "caml_serialize_int_1"
      (func $caml_serialize_int_1 (param (ref eq)) (param i32)))
   (import "env" "caml_serialize_int_4"
      (func $caml_serialize_int_4 (param (ref eq)) (param i32)))
   (import "env" "caml_deserialize_uint_1"
      (func $caml_deserialize_uint_1 (param (ref eq)) (result i32)))
   (import "env" "caml_deserialize_int_4"
      (func $caml_deserialize_int_4 (param (ref eq)) (result i32)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))

   (type $compare
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $hash
      (func (param (ref eq)) (result i32)))
   (type $fixed_length (struct (field $bsize_32 i32) (field $bsize_64 i32)))
   (type $serialize
      (func (param (ref eq)) (param (ref eq)) (result i32) (result i32)))
   (type $deserialize (func (param (ref eq)) (result (ref eq)) (result i32)))
   (type $dup (func (param (ref eq)) (result (ref eq))))
   (type $custom_operations
      (struct
         (field $id (ref $string))
         (field $compare (ref null $compare))
         (field $compare_ext (ref null $compare))
         (field $hash (ref null $hash))
         (field $fixed_length (ref null $fixed_length))
         (field $serialize (ref null $serialize))
         (field $deserialize (ref null $deserialize))
         (field $dup (ref null $dup))))
   (type $custom (sub (struct (field (ref $custom_operations)))))

   (global $ml_z_custom_ops (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string 2 ;; "_z"
            (i32.const 95) (i32.const 122))
         (ref.func $ml_z_custom_compare)
         (ref.func $ml_z_custom_compare)
         (ref.func $ml_z_custom_hash)
         (ref.null $fixed_length)
         (ref.func $ml_z_custom_serialize)
         (ref.func $ml_z_custom_deserialize)
         (ref.null $dup)))

   (type $bigint
      (sub final $custom
         (struct
            (field (ref $custom_operations))
            (field $v (ref any)))))

   (func $unwrap_bigint (param (ref eq)) (result (ref any))
      (block $not_bigint (result (ref any))
         (return
            (struct.get $bigint $v
               (br_on_cast_fail
                  $not_bigint (ref eq) (ref $bigint) (local.get 0))))))

   (func $wrap_bigint (param (ref any)) (result (ref eq))
      (block $is_eq (result (ref eq))
         (return
            (struct.new $bigint
               (global.get $ml_z_custom_ops)
               (br_on_cast $is_eq (ref any) (ref eq) (local.get 0))))))

   (data $ml_z_overflow "ml_z_overflow")

   (func $ml_z_raise_overflow
      (call $caml_raise_constant
         (ref.as_non_null
            (call $caml_named_value
               (array.new_data $string $ml_z_overflow
                  (i32.const 0) (i32.const 13))))))

   (func (export "ml_z_mul_overflows")
      (param $vx (ref eq)) (param $vy (ref eq)) (result (ref eq))
      (local $x i64) (local $y i64) (local $z i64)
      (local.set $x
         (i64.extend_i32_s (i31.get_u (ref.cast (ref i31) (local.get $vx)))))
      (local.set $y
         (i64.extend_i32_s (i31.get_u (ref.cast (ref i31) (local.get $vy)))))
      (local.set $z (i64.mul (local.get $x) (local.get $y)))
      (ref.i31
         (i64.ne (local.get $z)
            (i64.shr_s
               (i64.shl (local.get $z) (i64.const 33)) (i64.const 33)))))

   (func (export "ml_z_init") (param (ref eq)) (result (ref eq))
      (call $caml_register_custom_operations (global.get $ml_z_custom_ops))
      (ref.i31 (i32.const 0)))

   (func (export "ml_z_neg") (param $z (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $neg (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_add")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $add
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_sub")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $sub
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_mul")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $mul
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (export "ml_z_divexact" (func $ml_z_div))
   (func $ml_z_div (export "ml_z_div")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (if (ref.eq (local.get $z2) (ref.i31 (i32.const 0)))
         (then (call $caml_raise_zero_divide)))
      (return_call $wrap_bigint
         (call $div
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func $ml_z_custom_serialize
      (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
      (local $z (ref any)) (local $nb i32)
      (local.set $z (call $unwrap_bigint (local.get $v)))
      (call $caml_serialize_int_1 (local.get $s)
         (select (i32.const 0) (i32.const 1) (call $positive (local.get $z))))
      (local.set $nb (i32.mul (call $size (local.get $z)) (i32.const 4)))
      (call $caml_serialize_int_4 (local.get $s) (local.get $nb))
      (call $serialize (ref.func $caml_serialize_int_1) (local.get $s)
         (local.get $z))
      (tuple.make 2
          (i32.add (i32.const 4) (local.get $nb))
          (i32.and (i32.const -8) (i32.add (i32.const 15) (local.get $nb)))))

   (func $ml_z_custom_deserialize
      (param $s (ref eq)) (result (ref eq)) (result i32)
      (local $neg i32) (local $nb i32)
      (local.set $neg (call $caml_deserialize_uint_1 (local.get $s)))
      (local.set $nb (call $caml_deserialize_int_4 (local.get $s)))
      (tuple.make 2
         (call $wrap_bigint
            (call $deserialize (ref.func $caml_deserialize_uint_1) (local.get $s)
               (local.get $neg) (local.get $nb)))
         (i32.add (i32.const 4) (local.get $nb))))

   (func (export "ml_z_cdiv")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (if (ref.eq (local.get $z2) (ref.i31 (i32.const 0)))
         (then (call $caml_raise_zero_divide)))
      (return_call $wrap_bigint
         (call $cdiv
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_fdiv")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (if (ref.eq (local.get $z2) (ref.i31 (i32.const 0)))
         (then (call $caml_raise_zero_divide)))
      (return_call $wrap_bigint
         (call $fdiv
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_rem")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (if (ref.eq (local.get $z2) (ref.i31 (i32.const 0)))
         (then (call $caml_raise_zero_divide)))
      (return_call $wrap_bigint
         (call $rem
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_div_rem")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (local $z1' (ref any)) (local $z2' (ref any))
      (if (ref.eq (local.get $z2) (ref.i31 (i32.const 0)))
         (then (call $caml_raise_zero_divide)))
      (local.set $z1' (call $unwrap_bigint (local.get $z1)))
      (local.set $z2' (call $unwrap_bigint (local.get $z2)))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (call $wrap_bigint (call $div (local.get $z1') (local.get $z2')))
         (call $wrap_bigint (call $rem (local.get $z1') (local.get $z2')))))

   (func (export "ml_z_succ") (param $z (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $add
            (call $unwrap_bigint (local.get $z)) (ref.i31 (i32.const 1)))))

   (func (export "ml_z_pred") (param $z (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $sub
            (call $unwrap_bigint (local.get $z)) (ref.i31 (i32.const 1)))))

   (func (export "ml_z_abs") (param $z (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $abs (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_logand")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $logand
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_logor")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $logor
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_logxor")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $logxor
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_lognot") (param $z (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $lognot (call $unwrap_bigint (local.get $z)))))

   (data $shift_left_error "Z.shift_left: count argument must be positive")

   (func (export "ml_z_shift_left")
      (param $z (ref eq)) (param $amt (ref eq)) (result (ref eq))
      (if (i32.lt_s
             (i31.get_s (ref.cast (ref i31) (local.get $amt)))
             (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $shift_left_error
                  (i32.const 0) (i32.const 45)))))
      (return_call $wrap_bigint
         (call $shift_left
            (call $unwrap_bigint (local.get $z)) (local.get $amt))))

   (data $shift_right_error "Z.shift_right: count argument must be positive")

   (func (export "ml_z_shift_right")
      (param $z (ref eq)) (param $amt (ref eq)) (result (ref eq))
      (if (i32.lt_s
             (i31.get_s (ref.cast (ref i31) (local.get $amt)))
             (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $shift_right_error
                  (i32.const 0) (i32.const 46)))))
      (return_call $wrap_bigint
         (call $shift_right
            (call $unwrap_bigint (local.get $z)) (local.get $amt))))

   (data $shift_right_trunc_error
      "Z.shift_right_trunc: count argument must be positive")

   (func (export "ml_z_shift_right_trunc")
      (param $z (ref eq)) (param $amt (ref eq)) (result (ref eq))
      (if (i32.lt_s
             (i31.get_s (ref.cast (ref i31) (local.get $amt)))
             (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $shift_right_trunc_error
                  (i32.const 0) (i32.const 52)))))
      (return_call $wrap_bigint
         (call $shift_right_trunc
            (call $unwrap_bigint (local.get $z)) (local.get $amt))))

   (func (export "ml_z_of_int32")
      (param $d (ref eq)) (result (ref eq))
      (local $x i32) (local $res (ref i31))
      (local.set $x (call $Int32_val (local.get $d)))
      (local.set $res (ref.i31 (local.get $x)))
      (if (i32.eq (local.get $x) (i31.get_s (local.get $res)))
         (then (return (local.get $res)))
         (else (return_call $wrap_bigint (call $of_int32 (local.get $x))))))

   (func (export "ml_z_of_nativeint")
      (param $d (ref eq)) (result (ref eq))
      (local $x i32) (local $res (ref i31))
      (local.set $x (call $Nativeint_val (local.get $d)))
      (local.set $res (ref.i31 (local.get $x)))
      (if (i32.eq (local.get $x) (i31.get_s (local.get $res)))
         (then (return (local.get $res)))
         (else (return_call $wrap_bigint (call $of_int32 (local.get $x))))))

   (func (export "ml_z_of_int64")
      (param $z (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $of_int64 (call $Int64_val (local.get $z)))))

   (func (export "ml_z_of_float")
      (param $vf (ref eq)) (result (ref eq))
      (local $f f64)
      (local.set $f (call $Double_val (local.get $vf)))
      (if (i32.or (f64.ne (local.get $f) (local.get $f))
             (i32.or (f64.eq (local.get $f) (f64.const inf))
                (f64.eq (local.get $f) (f64.const -inf))))
         (then (call $ml_z_raise_overflow)))
      (call $wrap_bigint (call $of_float (local.get $f))))

   (func (export "ml_z_to_int")
      (param $z (ref eq)) (result (ref eq))
      (if (i32.eqz (ref.test (ref i31) (local.get $z)))
         (then (call $ml_z_raise_overflow)))
      (local.get $z))

   (func (export "ml_z_to_int32")
      (param $z (ref eq)) (result (ref eq))
      (local $z' (ref any))
      (drop (block $large (result (ref eq))
         (return_call $caml_copy_int32
            (i31.get_s
               (br_on_cast_fail $large (ref eq) (ref i31) (local.get $z))))))
      (local.set $z' (call $unwrap_bigint (local.get $z)))
      (if (i32.eqz (call $fits_int32 (local.get $z')))
         (then (call $ml_z_raise_overflow)))
      (return_call $caml_copy_int32 (call $to_int32 (local.get $z'))))

   (func (export "ml_z_to_nativeint")
      (param $z (ref eq)) (result (ref eq))
      (local $z' (ref any))
      (drop (block $large (result (ref eq))
         (return_call $caml_copy_nativeint
            (i31.get_s
               (br_on_cast_fail $large (ref eq) (ref i31) (local.get $z))))))
      (local.set $z' (call $unwrap_bigint (local.get $z)))
      (if (i32.eqz (call $fits_int32 (local.get $z')))
         (then (call $ml_z_raise_overflow)))
      (return_call $caml_copy_nativeint (call $to_int32 (local.get $z'))))

   (func (export "ml_z_to_int64")
      (param $z (ref eq)) (result (ref eq))
      (local $z' (ref any))
      (drop (block $large (result (ref eq))
         (return_call $caml_copy_int64
            (i64.extend_i32_s
               (i31.get_s
                  (br_on_cast_fail $large (ref eq) (ref i31)
                     (local.get $z)))))))
      (local.set $z' (call $unwrap_bigint (local.get $z)))
      (if (i32.eqz (call $fits_int64 (local.get $z')))
         (then (call $ml_z_raise_overflow)))
      (return_call $caml_copy_int64 (call $to_int64 (local.get $z'))))

   (func (export "ml_z_testbit")
      (param $z (ref eq)) (param $vpos (ref eq)) (result (ref eq))
      (local $pos i32)
      (drop (block $large (result (ref eq))
         (local.set $pos (i31.get_s (ref.cast (ref i31) (local.get $vpos))))
         (if (i32.gt_s (local.get $pos) (i32.const 31))
            (then (local.set $pos (i32.const 31))))
         (return
            (ref.i31
               (i32.and (i32.const 1)
                  (i32.shr_s
                     (i31.get_s
                        (br_on_cast_fail $large (ref eq) (ref i31)
                           (local.get $z)))
                     (local.get $pos)))))))
      (ref.i31
         (call $testbit
            (call $unwrap_bigint (local.get $z)) (local.get $vpos))))

   (data $format_error "Unsupported format '")

   (func (export "ml_z_format")
      (param $vfmt (ref eq)) (param $z (ref eq)) (result (ref eq))
      (local $res (ref any))
      (local $fmt (ref $string)) (local $msg (ref $string)) (local $len i32)
      (local.set $res
         (call $format
            (call $unwrap (call $caml_jsstring_of_string (local.get $vfmt)))
            (call $unwrap_bigint (local.get $z))))
      (if (ref.test (ref i31) (local.get $res))
         (then
            (local.set $fmt (ref.cast (ref $string) (local.get $vfmt)))
            (local.set $len (array.len (local.get $fmt)))
            (local.set $msg
               (array.new $string (i32.const 0)
                  (i32.add (local.get $len) (i32.const 21))))
            (array.init_data $string $format_error
               (local.get $msg) (i32.const 0) (i32.const 0) (i32.const 20))
            (array.copy $string $string
               (local.get $msg) (i32.const 20)
               (local.get $fmt) (i32.const 0) (local.get $len))
            (array.set $string (local.get $msg)
               (i32.add (local.get $len) (i32.const 20))
               (i32.const 39)) ;; "'"
            (call $caml_failwith (local.get $msg))))
      (call $caml_string_of_jsstring (call $wrap (local.get $res))))

   (data $of_subtring_base_error
      "Z.of_substring_base: invalid offset or length")

   (data $of_subtring_base_bad_base
      "Z.of_substring_base: base must be between 2 and 16")

   (data $of_subtring_base_invalid_digit
      "Z.of_substring_base: invalid digit")

   (func (export "ml_z_of_substring_base")
      (param $vbase (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $s (ref $string))
      (local $base i32) (local $pos i32) (local $len i32)
      (local $res anyref)
      (local.set $s (ref.cast (ref $string) (local.get $vs)))
      (local.set $pos (i31.get_s (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (local.set $base (i31.get_s (ref.cast (ref i31) (local.get $vbase))))
      (if (i32.or (i32.lt_s (local.get $pos) (i32.const 0))
             (i32.or (i32.lt_s (local.get $len) (i32.const 0))
                (i32.lt_s (array.len (local.get $s))
                   (i32.add (local.get $pos) (local.get $len)))))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $of_subtring_base_error
                  (i32.const 0) (i32.const 45)))))
      (if (i32.ne (local.get $base) (i32.const 0))
         (then
            (if (i32.or (i32.lt_s (local.get $base) (i32.const 2))
                   (i32.gt_s (local.get $base) (i32.const 16)))
               (then
                  (call $caml_invalid_argument
                     (array.new_data $string $of_subtring_base_bad_base
                        (i32.const 0) (i32.const 50)))))))
      (local.set $res
         (call $of_js_string_base
            (local.get $vbase)
            (call $jsstring_of_substring
               (local.get $s)
               (local.get $pos)
               (local.get $len))))
      (block $error
         (return_call $wrap_bigint (br_on_null $error (local.get $res))))
      (call $caml_invalid_argument
         (array.new_data $string $of_subtring_base_invalid_digit
            (i32.const 0) (i32.const 34)))
      (ref.i31 (i32.const 0)))

   (func $ml_z_custom_compare
      (param $z1 (ref eq)) (param $z2 (ref eq)) (param i32) (result i32)
      (local $x1 i32) (local $x2 i32)
      (drop (block $large (result (ref eq))
         (local.set $x1
            (i31.get_s
               (br_on_cast_fail $large (ref eq) (ref i31) (local.get $z1))))
         (local.set $x2
            (i31.get_s
               (br_on_cast_fail $large (ref eq) (ref i31) (local.get $z2))))
         (return
            (i32.sub (i32.gt_s (local.get $x1) (local.get $x2))
                     (i32.lt_s (local.get $x1) (local.get $x2))))))
      (call $compare
         (call $unwrap_bigint (local.get $z1))
         (call $unwrap_bigint (local.get $z2))))

   (func (export "ml_z_compare")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (ref.i31
         (call $ml_z_custom_compare
            (local.get $z1) (local.get $z2) (i32.const 0))))

   (func (export "ml_z_equal")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (local $x1 i32) (local $x2 i32)
      ;; Equal if physically equal
      (if (ref.eq (local.get $z1) (local.get $z2))
         (then (return (ref.i31 (i32.const 1)))))
      ;; If one of them is a small integer, equality implies physical equality
      (if (i32.or (ref.test (ref i31) (local.get $z1))
             (ref.test (ref i31) (local.get $z2)))
         (then (return (ref.i31 (i32.const 0)))))
      (ref.i31
         (call $equal
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_sign")
      (param $z (ref eq)) (result (ref eq))
      (local $x i32)
      (drop (block $large (result (ref eq))
         (local.set $x
            (i31.get_s
               (br_on_cast_fail $large (ref eq) (ref i31) (local.get $z))))
         (return
            (ref.i31
               (i32.sub (i32.gt_s (local.get $x) (i32.const 0))
                  (i32.lt_s (local.get $x) (i32.const 0)))))))
      (select (ref.i31 (i32.const 1)) (ref.i31 (i32.const -1))
         (call $positive (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_gcd")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $gcd
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_numbits") (param $z (ref eq)) (result (ref eq))
      (ref.i31 (call $numbits (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_fits_int")
      (param $z (ref eq)) (result (ref eq))
      (ref.i31 (ref.test (ref i31) (local.get $z))))

   (func (export "ml_z_fits_int32")
      (param $z (ref eq)) (result (ref eq))
      (if (ref.test (ref i31) (local.get $z))
         (then (return (ref.i31 (i32.const 1)))))
      (ref.i31 (call $fits_int32 (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_fits_nativeint")
      (param $z (ref eq)) (result (ref eq))
      (if (ref.test (ref i31) (local.get $z))
         (then (return (ref.i31 (i32.const 1)))))
      (ref.i31 (call $fits_int32 (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_fits_int64")
      (param $z (ref eq)) (result (ref eq))
      (if (ref.test (ref i31) (local.get $z))
         (then (return (ref.i31 (i32.const 1)))))
      (ref.i31 (call $fits_int64 (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_powm")
      (param $base (ref eq)) (param $exp (ref eq)) (param $mod (ref eq))
      (result (ref eq))
      (local $exp' (ref any))
      (local.set $exp' (call $unwrap_bigint (local.get $exp)))
      (if (ref.eq (local.get $mod) (ref.i31 (i32.const 0)))
         (then (call $caml_raise_zero_divide)))
      (if (i32.eqz (call $positive (local.get $exp')))
         (then
            (local.set $base
               (call $ml_z_invert (local.get $base) (local.get $mod)))
            (local.set $exp' (call $neg (local.get $exp')))))
      (if (i32.or (ref.eq (local.get $mod) (ref.i31 (i32.const 1)))
              (ref.eq (local.get $mod) (ref.i31 (i32.const -1))))
         (then (return (ref.i31 (i32.const 0)))))
      (if (ref.eq (local.get $exp) (ref.i31 (i32.const 0)))
         (then (return (ref.i31 (i32.const 1)))))
      (return_call $wrap_bigint
         (call $powm
            (call $unwrap_bigint (local.get $base))
            (local.get $exp')
            (call $unwrap_bigint (local.get $mod)))))

   (data $pow_negative_exp "Z.pow: exponent must be nonnegative")

   (func (export "ml_z_pow")
      (param $z (ref eq)) (param $i (ref eq))
      (result (ref eq))
      (if (i32.lt_s
             (i31.get_s (ref.cast (ref i31) (local.get $i)))
             (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $pow_negative_exp
                  (i32.const 0) (i32.const 35)))))
      (call $wrap_bigint
         (call $pow (call $unwrap_bigint (local.get $z)) (local.get $i))))

   (func $ml_z_custom_hash (param $z (ref eq)) (result i32)
      (return_call $hash (ref.func $caml_hash_mix_int)
         (call $unwrap_bigint (local.get $z))))

   (func (export "ml_z_hash")
      (param $z (ref eq)) (result (ref eq))
      (ref.i31
         (i32.and (i32.const 0x3FFFFFFF)
            (call $caml_hash_mix_final
               (call $caml_hash_mix_int (i32.const 0)
                  (call $hash
                     (ref.func $caml_hash_mix_int)
                     (call $unwrap_bigint (local.get $z))))))))

   (func (export "ml_z_to_bits")
      (param $z (ref eq)) (result (ref eq))
      (return_call $caml_string_of_jsbytes
         (call $wrap (call $to_bits (call $unwrap_bigint (local.get $z))))))

   (func (export "ml_z_of_bits")
      (param $s (ref eq)) (result (ref eq))
      (return_call $wrap_bigint
         (call $of_bits
            (call $unwrap
               (call $caml_jsbytes_of_string (local.get $s))))))

   (data $powm_positive_exp "Z.powm_sec: exponent must be positive")
   (data $powm_odd_modulus "Z.powm_sec: modulus must be odd")

   (func (export "ml_z_powm_sec")
      (param $base (ref eq)) (param $exp (ref eq)) (param $mod (ref eq))
      (result (ref eq))
      (if (i32.or (ref.eq (local.get $exp) (ref.i31 (i32.const 0)))
             (i32.eqz (call $positive (call $unwrap_bigint (local.get $exp)))))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $powm_positive_exp
                  (i32.const 0) (i32.const 37)))))
      (if (i32.eqz
             (i31.get_s
                (ref.cast (ref i31)
                   (call $logand
                      (call $unwrap_bigint (local.get $mod))
                      (ref.i31 (i32.const 1))))))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $powm_odd_modulus
                  (i32.const 0) (i32.const 31)))))
      (if (i32.or (ref.eq (local.get $mod) (ref.i31 (i32.const 1)))
              (ref.eq (local.get $mod) (ref.i31 (i32.const -1))))
         (then (return (ref.i31 (i32.const 0)))))
      (return_call $wrap_bigint
         (call $powm
            (call $unwrap_bigint (local.get $base))
            (call $unwrap_bigint (local.get $exp))
            (call $unwrap_bigint (local.get $mod)))))

   (data $root_negative_exponent "Z.root: exponent must be positive")

   (data $root_even_neg "Z.root: even root of a negative number")

   (func (export "ml_z_root")
      (param $z (ref eq)) (param $vi (ref eq)) (result (ref eq))
      (local $i i32) (local $z' (ref any))
      (local.set $i (i31.get_s (ref.cast (ref i31) (local.get $vi))))
      (local.set $z' (call $unwrap_bigint (local.get $z)))
      (if (i32.le_s (local.get $i) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $root_negative_exponent
                  (i32.const 0) (i32.const 33)))))
      (if (i32.eqz (i32.and (local.get $i) (i32.const 1)))
         (then
            (if (i32.eqz (call $positive (local.get $z')))
               (then
                  (call $caml_invalid_argument
                     (array.new_data $string $root_even_neg
                        (i32.const 0) (i32.const 38)))))))
      (if (i32.or (ref.eq (local.get $z) (ref.i31 (i32.const 0)))
             (ref.eq (local.get $z) (ref.i31 (i32.const 1))))
         (then
            (return (local.get $z))))
      (return_call $wrap_bigint
         (call $root
            (call $unwrap_bigint (local.get $z))
            (local.get $vi))))

   (data $rootrem_negative_exponent "Z.rootrem: exponent must be positive")

   (data $rootrem_even_neg "Z.rootrem: even root of a negative number")

   (func (export "ml_z_rootrem")
      (param $z (ref eq)) (param $vi (ref eq)) (result (ref eq))
      (local $i i32) (local $z' (ref any)) (local $r (ref any))
      (local.set $i (i31.get_s (ref.cast (ref i31) (local.get $vi))))
      (local.set $z' (call $unwrap_bigint (local.get $z)))
      (if (i32.le_s (local.get $i) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $rootrem_negative_exponent
                  (i32.const 0) (i32.const 36)))))
      (if (i32.eqz (i32.and (local.get $i) (i32.const 1)))
         (then
            (if (i32.eqz (call $positive (local.get $z')))
               (then
                  (call $caml_invalid_argument
                     (array.new_data $string $rootrem_even_neg
                        (i32.const 0) (i32.const 41)))))))
      (if (i32.or (ref.eq (local.get $z) (ref.i31 (i32.const 0)))
             (ref.eq (local.get $z) (ref.i31 (i32.const 1))))
         (then
            (return
               (array.new_fixed $block 3 (ref.i31 (i32.const 0))
                  (local.get $z)
                  (ref.i31 (i32.const 0))))))
      (local.set $r
         (call $root
            (call $unwrap_bigint (local.get $z))
            (local.get $vi)))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (call $wrap_bigint (local.get $r))
         (call $wrap_bigint
            (call $sub
               (local.get $z')
               (call $pow (local.get $r) (local.get $vi))))))

   (func $ml_z_invert (export "ml_z_invert")
      (param $a (ref eq)) (param $n (ref eq)) (result (ref eq))
      (if (i32.or
             (ref.eq (local.get $n) (ref.i31 (i32.const 1)))
             (ref.eq (local.get $n) (ref.i31 (i32.const -1))))
         (then (return (ref.i31 (i32.const 0)))))
      (if (i32.and (ref.eq (local.get $n) (ref.i31 (i32.const 0)))
             (i32.or
                (ref.eq (local.get $a) (ref.i31 (i32.const 1)))
                (ref.eq (local.get $a) (ref.i31 (i32.const -1)))))
         (then (return (local.get $a))))
      (block $zero_divide
         (br_if $zero_divide
            (i32.or
               (ref.eq (local.get $n) (ref.i31 (i32.const 0)))
               (ref.eq (local.get $a) (ref.i31 (i32.const 0)))))
         (return_call $wrap_bigint
            (br_on_null $zero_divide
               (call $invert
                  (call $unwrap_bigint (local.get $a))
                  (call $unwrap_bigint (local.get $n))))))
      (call $caml_raise_zero_divide)
      (ref.i31 (i32.const 0)))

   (func (export "ml_z_perfect_power") (param $z (ref eq)) (result (ref eq))
      (ref.i31 (call $perfect_power (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_perfect_square") (param $z (ref eq)) (result (ref eq))
      (ref.i31 (call $perfect_square (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_probab_prime")
      (param $z (ref eq)) (param $i (ref eq)) (result (ref eq))
      (ref.i31
         (call $probab_prime
            (call $unwrap_bigint (local.get $z)) (local.get $i))))

   (func (export "ml_z_nextprime") (param $z (ref eq)) (result (ref eq))
      (call $wrap_bigint
         (call $next_prime (call $unwrap_bigint (local.get $z)))))

   (export "ml_z_extract_small" (func $ml_z_extract))
   (func $ml_z_extract (export "ml_z_extract")
      (param $z (ref eq)) (param $pos (ref eq)) (param $len (ref eq))
      (result (ref eq))
      (return_call $wrap_bigint
         (call $extract
            (call $unwrap_bigint (local.get $z))
            (local.get $pos)
            (local.get $len))))

   (func (export "ml_z_gcdext_intern")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (local $res (ref eq))
      (if (i32.or
             (ref.eq (local.get $z1) (ref.i31 (i32.const 0)))
             (ref.eq (local.get $z2) (ref.i31 (i32.const 0))))
         (then (call $caml_raise_zero_divide)))
      (local.set $res
         (call $wrap
            (call $gcdext_intern
               (call $unwrap_bigint (local.get $z1))
               (call $unwrap_bigint (local.get $z2)))))
      (array.new_fixed $block 4 (ref.i31 (i32.const 0))
         (call $wrap_bigint
            (ref.as_non_null
               (call $unwrap
                  (call $caml_js_get
                     (local.get $res) (ref.i31 (i32.const 0))))))
         (call $wrap_bigint
            (ref.as_non_null
               (call $unwrap
                  (call $caml_js_get
                     (local.get $res) (ref.i31 (i32.const 1))))))
         (ref.i31 (i32.const 1))))

   (data $sqrt_non_positive "Z.sqrt: square root of a negative number")

   (func (export "ml_z_sqrt")
      (param $z (ref eq)) (result (ref eq))
      (local $z' (ref any))
      (local.set $z' (call $unwrap_bigint (local.get $z)))
      (if (i32.eqz (call $positive (local.get $z')))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $sqrt_non_positive
                  (i32.const 0) (i32.const 40)))))
      (return_call $wrap_bigint
         (call $root (local.get $z') (ref.i31 (i32.const 2)))))

   (data $sqrt_rem_non_positive "Z.sqrt_rem: square root of a negative number")

   (func (export "ml_z_sqrt_rem")
      (param $z (ref eq)) (result (ref eq))
      (local $z' (ref any)) (local $r (ref any))
      (local.set $z' (call $unwrap_bigint (local.get $z)))
      (if (i32.eqz (call $positive (local.get $z')))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $sqrt_rem_non_positive
                  (i32.const 0) (i32.const 44)))))
      (local.set $r (call $root (local.get $z') (ref.i31 (i32.const 2))))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (call $wrap_bigint (local.get $r))
         (call $wrap_bigint
            (call $sub
               (local.get $z')
               (call $mul (local.get $r) (local.get $r))))))

   (func (export "ml_z_trailing_zeros")
      (param $z (ref eq)) (result (ref eq))
      (local $x i32)
      (drop (block $large (result (ref eq))
         (local.set $x
            (i31.get_s
               (br_on_cast_fail $large (ref eq) (ref i31) (local.get $z))))
         (if (i32.eq (local.get $x) (i32.const 0))
            (then (return (ref.i31 (i32.const 1073741823)))))
         (return
            (ref.i31 (i32.ctz (local.get $x))))))
      (ref.i31 (call $trailing_zeroes (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_popcount") (param $z (ref eq)) (result (ref eq))
      (local $x i32) (local $res i32)
      (drop (block $large (result (ref eq))
         (local.set $x
            (i31.get_s
               (br_on_cast_fail $large (ref eq) (ref i31) (local.get $z))))
         (if (i32.lt_s (local.get $x) (i32.const 0))
            (then (call $ml_z_raise_overflow)))
         (return (ref.i31 (i32.popcnt (local.get $x))))))
      (local.set $res (call $popcount (call $unwrap_bigint (local.get $z))))
      (if (i32.lt_s (local.get $res) (i32.const 0))
         (then (call $ml_z_raise_overflow)))
      (ref.i31 (local.get $res)))

   (func (export "ml_z_hamdist")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (local $x i32) (local $res i32)
      (drop (block $large (result (ref eq))
         (local.set $x
            (i32.xor
               (i31.get_s
                  (br_on_cast_fail $large (ref eq) (ref i31) (local.get $z1)))
               (i31.get_s
                  (br_on_cast_fail $large (ref eq) (ref i31) (local.get $z2)))))
         (if (i32.lt_s (local.get $x) (i32.const 0))
            (then (call $ml_z_raise_overflow)))
         (return (ref.i31 (i32.popcnt (local.get $x))))))
      (local.set $res
         (call $hamdist
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2))))
      (if (i32.lt_s (local.get $res) (i32.const 0))
         (then (call $ml_z_raise_overflow)))
      (ref.i31 (local.get $res)))

   (func (export "ml_z_size") (param $z (ref eq)) (result (ref eq))
      (if (ref.test (ref i31) (local.get $z))
         (then (return (ref.i31 (i32.const 1)))))
      (ref.i31 (call $size (call $unwrap_bigint (local.get $z)))))

   (func (export "ml_z_divisible")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (if (ref.eq (local.get $z2) (ref.i31 (i32.const 0)))
         (then
            (return
               (ref.i31 (ref.eq (local.get $z1) (ref.i31 (i32.const 0)))))))
      (ref.i31
         (call $divisible
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2)))))

   (func (export "ml_z_congruent")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (param $z3 (ref eq))
      (result (ref eq))
      (local $z (ref any))
      (local.set $z
         (call $sub
            (call $unwrap_bigint (local.get $z1))
            (call $unwrap_bigint (local.get $z2))))
      (if (ref.eq (local.get $z3) (ref.i31 (i32.const 0)))
         (then
            (drop (block $large (result (ref any))
               (return
                  (ref.i31
                     (ref.eq
                        (br_on_cast_fail $large (ref any) (ref i31)
                           (local.get $z))
                        (ref.i31 (i32.const 0)))))))
            (return (ref.i31 (i32.const 0)))))
      (ref.i31
         (call $divisible
            (local.get $z)
            (call $unwrap_bigint (local.get $z3)))))

   (func (export "ml_z_remove")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (local $res (ref eq))
      (if (ref.eq (local.get $z2) (ref.i31 (i32.const 0)))
         (then (call $caml_raise_zero_divide)))
      (if (i32.or (ref.eq (local.get $z1) (ref.i31 (i32.const 0)))
             (i32.or (ref.eq (local.get $z2) (ref.i31 (i32.const 1)))
                (ref.eq (local.get $z2) (ref.i31 (i32.const -1)))))
         (then
            (return
               (array.new_fixed $block 3 (ref.i31 (i32.const 0))
                  (local.get $z1) (ref.i31 (i32.const 0))))))
      (local.set $res
         (call $wrap
            (call $remove
               (call $unwrap_bigint (local.get $z1))
               (call $unwrap_bigint (local.get $z2)))))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (call $wrap_bigint
            (ref.as_non_null
               (call $unwrap
                  (call $caml_js_get
                     (local.get $res) (ref.i31 (i32.const 0))))))
         (ref.cast (ref i31)
            (call $caml_js_get
               (local.get $res) (ref.i31 (i32.const 1))))))

   (data $fac_non_positive "Z.fac: non-positive argument")

   (func (export "ml_z_fac")
      (param $a (ref eq)) (result (ref eq))
      (if (i32.lt_s
             (i31.get_s (ref.cast (ref i31) (local.get $a)))
             (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $fac_non_positive
                  (i32.const 0) (i32.const 28)))))
      (call $wrap_bigint (call $fac (local.get $a))))

   (data $fac2_non_positive "Z.fac2: non-positive argument")

   (func (export "ml_z_fac2")
      (param $a (ref eq)) (result (ref eq))
      (if (i32.lt_s
             (i31.get_s (ref.cast (ref i31) (local.get $a)))
             (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $fac2_non_positive
                  (i32.const 0) (i32.const 28)))))
      (call $wrap_bigint (call $fac2 (local.get $a))))

   (data $facM_non_positive "Z.facM: non-positive argument")

   (func (export "ml_z_facM")
      (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
      (if (i32.or
             (i32.lt_s
                (i31.get_s (ref.cast (ref i31) (local.get $a)))
                (i32.const 0))
             (i32.lt_s
                (i31.get_s (ref.cast (ref i31) (local.get $b)))
                (i32.const 0)))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $facM_non_positive
                  (i32.const 0) (i32.const 28)))))
      (call $wrap_bigint (call $facM (local.get $a) (local.get $b))))

   (data $fib_non_positive "Z.fib: non-positive argument")

   (func (export "ml_z_fib")
      (param $a (ref eq)) (result (ref eq))
      (if (i32.lt_s
             (i31.get_s (ref.cast (ref i31) (local.get $a)))
             (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $fib_non_positive
                  (i32.const 0) (i32.const 28)))))
      (call $wrap_bigint (call $fib (local.get $a))))

   (data $lucnum_non_positive "Z.lucnum: non-positive argument")

   (func (export "ml_z_lucnum")
      (param $a (ref eq)) (result (ref eq))
      (if (i32.lt_s
             (i31.get_s (ref.cast (ref i31) (local.get $a)))
             (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $lucnum_non_positive
                  (i32.const 0) (i32.const 31)))))
      (call $wrap_bigint (call $lucnum (local.get $a))))

   (data $jacobi_invalid_arg "Z.jacobi: second argument is negative or even")

   (export "ml_z_legendre" (func $ml_z_jacobi))
   (func $ml_z_jacobi (export "ml_z_jacobi")
      (param $n (ref eq)) (param $k (ref eq)) (result (ref eq))
      (local $k' (ref any))
      (local.set $k' (call $unwrap_bigint (local.get $k)))
      (if (i32.or (i32.eqz (call $positive (local.get $k')))
             (i32.eqz
                (i31.get_s
                   (ref.cast (ref i31)
                      (call $logand (local.get $k')
                         (ref.i31 (i32.const 1)))))))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $jacobi_invalid_arg
                  (i32.const 0) (i32.const 45)))))
      (ref.i31
         (call $jacobi (call $unwrap_bigint (local.get $n)) (local.get $k'))))

   (data $kronecker_not_implemented "ml_z_kronecker is not implemented")

   (func (export "ml_z_kronecker")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (call $caml_failwith
         (array.new_data $string $kronecker_not_implemented
            (i32.const 0) (i32.const 33)))
      (ref.i31 (i32.const 0)))

   (data $primorial_non_positive "Z.primorial: non-positive argument")

   (func (export "ml_z_primorial")
      (param $va (ref eq)) (result (ref eq))
      (local $a i32)
      (local.set $a (i31.get_s (ref.cast (ref i31) (local.get $va))))
      (if (i32.lt_s (local.get $a) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $primorial_non_positive
                  (i32.const 0) (i32.const 34)))))
      (return_call $wrap_bigint (call $primorial (local.get $a))))

   (data $bin_non_positive "Z.bin: non-positive argument")

   (func (export "ml_z_bin")
      (param $z1 (ref eq)) (param $z2 (ref eq)) (result (ref eq))
      (if (i32.lt_s
             (i31.get_s (ref.cast (ref i31) (local.get $z2)))
             (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string
                  $bin_non_positive (i32.const 0) (i32.const 28)))))
      (call $wrap_bigint
         (call $bin (call $unwrap_bigint (local.get $z1)) (local.get $z2))))
)
