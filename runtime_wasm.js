//Provides: wasm_z_normalize
function wasm_z_normalize(x) {
  // This is the primary reason runtime and runtime_wasm are separate despite looking.
  // very similar. JavaScript uses 32bit as its cutoff between number and BigInt, and
  // WebAssembly uses 31bit. With some amount of effort we could merge a lot of this
  // logic.
  return (x == BigInt.asIntN(31, x)) ? Number(x) : x;
}

//Provides: wasm_z_neg const
//Requires: wasm_z_normalize
function wasm_z_neg(z1) {
  return wasm_z_normalize(-BigInt(z1));
}

//Provides: wasm_z_add const
//Requires: wasm_z_normalize
function wasm_z_add(z1, z2) {
  return wasm_z_normalize(BigInt(z1) + BigInt(z2));
}

//Provides: wasm_z_sub const
//Requires: wasm_z_normalize
function wasm_z_sub(z1, z2) {
  return wasm_z_normalize(BigInt(z1) - BigInt(z2));
}

//Provides: wasm_z_mul const
//Requires: wasm_z_normalize
function wasm_z_mul(z1, z2) {
  return wasm_z_normalize(BigInt(z1) * BigInt(z2));
}

//Provides: wasm_z_div const
//Requires: wasm_z_normalize
function wasm_z_div(z1, z2) {
  return wasm_z_normalize(BigInt(z1) / BigInt(z2))
}

//Provides: wasm_z_serialize
function wasm_z_serialize(caml_serialize_int_1, s, z) {
  if (z < 0) z = -z;
  do {
    var x = Number(BigInt.asIntN(32, z));
    caml_serialize_int_1(s, x);
    caml_serialize_int_1(s, x >>> 8);
    caml_serialize_int_1(s, x >>> 16);
    caml_serialize_int_1(s, x >>> 24);
    z >>= 32n;
  } while (z);
}

//Provides: wasm_z_deserialize
function wasm_z_deserialize(caml_deserialize_uint_1, s, neg, count) {
  var z = 0n;
  for (var i = 0; i < count / 4; i++) {
    var y = BigInt(caml_deserialize_uint_1(s));
    y |= BigInt(caml_deserialize_uint_1(s)) << 8n;
    y |= BigInt(caml_deserialize_uint_1(s)) << 16n;
    y |= BigInt(caml_deserialize_uint_1(s)) << 24n;
    z |= y << BigInt(32 * i);
  };
  if (neg) z = -z;
  return z;
}

//Provides: wasm_z_cdiv
//Requires: ml_z_div, ml_z_add
function wasm_z_cdiv(z1, z2) {
  if (z1 == 0) return 0;
  if ((z1 > 0) == (z2 > 0)) /* Multiplication is like a signwise xor */ {
    if (BigInt(z1) % BigInt(z2) != 0) {
      return ml_z_add(ml_z_div(z1, z2), 1n);
    }
  }
  return ml_z_div(z1, z2);
}

//Provides: wasm_z_fdiv
//Requires: ml_z_div, ml_z_sub
function wasm_z_fdiv(z1, z2) {
  if (z1 == 0) return 0;
  if ((z1 > 0) != (z2 > 0)) /* Multiplication is like a signwise xor */ {
    if (BigInt(z1) % BigInt(z2) != 0) {
      return ml_z_sub(ml_z_div(z1, z2), 1n);
    }
  }
  return ml_z_div(z1, z2);
}

//Provides: wasm_z_rem const
//Requires: wasm_z_normalize
function wasm_z_rem(z1, z2) {
  return wasm_z_normalize(BigInt(z1) % BigInt(z2))
}

//Provides: wasm_z_compare const
function wasm_z_compare(z1, z2) {
  return (z1 > z2) - (z1 < z2);
}

//Provides: wasm_z_equal const
function wasm_z_equal(z1, z2) {
  return z1 == z2;
}

//Provides: wasm_z_gcd
//Requires: wasm_z_normalize
function wasm_z_gcd(z1, z2) {
  let a = BigInt(z1);
  if (a < 0) a = -a;
  let b = BigInt(z2);
  if (b < 0) b = -b;
  if (a === b) return wasm_z_normalize(a);
  if (a === 0n) return wasm_z_normalize(b);
  if (b === 0n) return wasm_z_normalize(a);
  let c = 1n, d, t;
  function min(a, b) { return a < b ? a : b }
  while ((a & 1n) === 0n && (b & 1n) === 0n) {
    d = min(a & -a, b & -b);
    a /= d;
    b /= d;
    c *= d;
  }
  while ((a & 1n) === 0n) {
    a /= a & -a;
  }
  do {
    while ((b & 1n) === 0n) {
      b /= b & -b;
    }
    if (a > b) {
      t = b; b = a; a = t;
    }
    b -= a;
  } while (b !== 0n);
  return wasm_z_normalize(c === 1n ? a : a * c);
}

//Provides: wasm_z_positive const
function wasm_z_positive(z) {
  return z >= 0;
}

//Provides: wasm_z_abs const
function wasm_z_abs(z) {
  return (z >= 0) ? z : -BigInt(z);
}

//Provides: wasm_z_logand const
//Requires: wasm_z_normalize
function wasm_z_logand(z1, z2) {
  return wasm_z_normalize(BigInt(z1) & BigInt(z2));
}

//Provides: wasm_z_logor const
//Requires: wasm_z_normalize
function wasm_z_logor(z1, z2) {
  return wasm_z_normalize(BigInt(z1) | BigInt(z2));
}

//Provides: wasm_z_logxor const
//Requires: wasm_z_normalize
function wasm_z_logxor(z1, z2) {
  return wasm_z_normalize(BigInt(z1) ^ BigInt(z2));
}

//Provides: wasm_z_lognot const
//Requires: wasm_z_normalize
function wasm_z_lognot(z1) {
  return wasm_z_normalize(~BigInt(z1));
}

//Provides: wasm_z_shift_left const
//Requires: wasm_z_normalize
function wasm_z_shift_left(z1, z2) {
  return wasm_z_normalize(BigInt(z1) << BigInt(z2));
}

//Provides: wasm_z_shift_right const
//Requires: wasm_z_normalize
function wasm_z_shift_right(z1, z2) {
  return wasm_z_normalize(BigInt(z1) >> BigInt(z2));
}

//Provides: wasm_z_shift_right_trunc const
//Requires: wasm_z_normalize, wasm_z_shift_right
function wasm_z_shift_right_trunc(z1, z2) {
  if (z1 >= 0) return wasm_z_shift_right(z1, z2);
  return wasm_z_normalize(-(BigInt(-z1) >> BigInt(z2)));
}

//Provides: wasm_z_of_float const
//Requires: wasm_z_normalize
function wasm_z_of_float(f1) {
  return wasm_z_normalize(BigInt(f1 < 0 ? Math.ceil(f1) : Math.floor(f1)));
}

//Provides: wasm_z_of_int32 const
function wasm_z_of_int32(z) {
  // z does not fit in an integer
  return BigInt(z);
}

//Provides: wasm_z_to_int32 const
function wasm_z_to_int32(z) {
  return Number(z)
}

//Provides: wasm_z_to_int64 const
function wasm_z_to_int64(z) {
  return z;
}

//Provides: wasm_z_testbit const
function wasm_z_testbit(z, pos) {
  return +((z & (1n << BigInt(pos))) != 0);
}

//Provides: wasm_z_format
function wasm_z_format(fmt, z1) {
  z1 = BigInt(z1);
  // https://github.com/ocaml/Zarith/blob/d0555d451ce295c4497f24a8d9993f8dd23097df/z.mlip#L297
  var base = 10;
  var cas = 0;
  var width = 0;
  var alt = 0;
  var dir = 0;
  var sign = '';
  var pad = ' ';
  var idx = 0;
  var prefix = "";
  while (fmt[idx] == '%') idx++;
  for (; ; idx++) {
    if (fmt[idx] == '#') alt = 1;
    else if (fmt[idx] == '0') pad = '0';
    else if (fmt[idx] == '-') dir = 1;
    else if (fmt[idx] == ' ' || fmt[idx] == '+') sign = fmt[idx];
    else break;
  }
  if (z1 < 0) { sign = '-'; z1 = -z1 };
  for (; fmt[idx] >= '0' && fmt[idx] <= '9'; idx++)
    width = 10 * width + (+fmt[idx]);
  switch (fmt[idx]) {
    case 'i': case 'd': case 'u': break;
    case 'b': base = 2; if (alt) prefix = "0b"; break;
    case 'o': base = 8; if (alt) prefix = "0o"; break;
    case 'x': base = 16; if (alt) prefix = "0x"; break;
    case 'X': base = 16; if (alt) prefix = "0X"; cas = 1; break;
    default:
      return -1;
  }
  if (dir) pad = ' ';
  var res = z1.toString(base);
  if (cas === 1) {
    res = res.toUpperCase();
  }
  var size = res.length;
  if (pad == ' ') {
    if (dir) {
      res = sign + prefix + res;
      for (; res.length < width;) res = res + pad;
    } else {
      res = sign + prefix + res;
      for (; res.length < width;) res = pad + res;
    }
  } else {
    var pre = sign + prefix;
    for (; res.length + pre.length < width;) res = pad + res;
    res = pre + res;
  }
  return res;
}

//Provides: wasm_z_of_js_string_base
//Requires: wasm_z_normalize
function wasm_z_of_js_string_base(base, s) {
  if (base == 0) { // https://github.com/ocaml/Zarith/blob/b8dbaf48a7927061df699ad7ce642bb4f1fe5308/caml_z.c#L598
    base = 10;
    var p = 0;
    var sign = 1;
    if (s[p] == '-') { sign = -1; p++ }
    else if (s[p] == '+') { p++ }
    if (s[p] == '0') {
      p++;
      if (s.length == p) {
        return 0;
      } else {
        var bc = s[p];
        if (bc == 'o' || bc == 'O') {
          base = 8;
        } else if (bc == 'x' || bc == 'X') {
          base = 16;
        } else if (bc == 'b' || bc == 'B') {
          base = 2;
        }
        if (base != 10) {
          s = s.substring(p + 1);
          if (sign == -1) s = "-" + s;
        }
      }
    }
  }
  function digit(code) {
    if (code >= 48 && code <= 57) return code - 48;
    if (code >= 97 && code <= 102) return code - 97 + 10;
    if (code >= 65 && code <= 70) return code - 65 + 10;
    return 1000;
  }
  var negative = false;
  var i = 0;
  if (s[i] == '+') {
    //remove leading '+'
    s = s.substring(1);
  }
  else if (s[i] == '-') {
    negative = true;
    i++;
  }
  if (s[i] == '_') return null;
  s = s.replace(/_/g, '');
  //normalize "empty" numbers
  if (s == '-' || s == '') s = '0';
  var res = 0n;
  var base2 = BigInt(base);
  for (; i < s.length; i++) {
    var c = digit(s.charCodeAt(i));
    if (c >= base) return null;
    res = res * base2 + BigInt(c)
  }
  if (negative) res = -res;
  return wasm_z_normalize(res);
}

//Provides: wasm_z_numbits const
function wasm_z_numbits(z1) {
  z1 = BigInt(z1);
  if (z1 < 0) z1 = -z1;
  var n = 0;
  var upperBound = 1n;
  while (upperBound <= z1) {
    n += 1;
    upperBound <<= 1n;
  }
  return n; // 2^{n-1} <= |x| < 2^n
}

//Provides: wasm_z_fits_int32 const
function wasm_z_fits_int32(z) {
  return +(z == BigInt.asIntN(32, z));
}

//Provides: wasm_z_fits_int64 const
function wasm_z_fits_int64(z) {
  return +(z == BigInt.asIntN(64, z));
}

//Provides: wasm_z_powm
//Requires: wasm_z_normalize, ml_z_invert, jsoo_bigint_mod_pow
function wasm_z_powm(z1, z2, z3) {
  let z3n = BigInt(z3);
  if (z2 < 0) {
    let inv = BigInt(ml_z_invert(z1, z3));
    let r = jsoo_bigint_mod_pow(inv, BigInt(-z2), z3n);
    if (r < 0) r = r + (z3n < 0 ? -z3n : z3n);
    return wasm_z_normalize(r);
  } else {
    let r = jsoo_bigint_mod_pow(BigInt(z1), BigInt(z2), z3n);
    if (r < 0) r = r + (z3n < 0 ? -z3n : z3n);
    return wasm_z_normalize(r);
  }
}

//Provides: wasm_z_pow const
//Requires: wasm_z_normalize
function wasm_z_pow(z1, z2) {
  return wasm_z_normalize(BigInt(z1) ** BigInt(z2));
}

//Provides: wasm_z_hash const
function wasm_z_hash(caml_hash_mix_int, z1) {
  z1 = BigInt(z1);
  var neg = z1 < 0;
  if (neg) z1 = - z1;
  var i = 0;
  var acc = 0;
  while (z1) {
    i++;
    acc = caml_hash_mix_int(acc, Number(BigInt.asIntN(32, z1)));
    z1 >>= 32n;
  };
  if (i & 1) acc = caml_hash_mix_int(acc, 0);
  if (neg) acc++;
  return acc;
}

//Provides: wasm_z_to_bits const
function wasm_z_to_bits(z1) {
  z1 = BigInt(z1);
  if (z1 < 0) z1 = -z1;
  var res = "";
  while (z1 != 0) {
    res += String.fromCharCode(Number(z1 & 255n));
    z1 >>= 8n;
  }
  while ((res.length & 3) != 0) {
    res += String.fromCharCode(0);
  }
  return res;
}

//Provides: wasm_z_of_bits const
//Requires: wasm_z_normalize
function wasm_z_of_bits(s) {
  var r = 0n;
  for (var i = s.length - 1; i >= 0; i--) {
    var d = s.charCodeAt(i);
    r = (r << 8n) + BigInt(d);
  }
  return wasm_z_normalize(r);
}

//Provides: wasm_z_root
//Requires: wasm_z_normalize, wasm_z_numbits
function wasm_z_root(z, i) {
  if (z == 0 || z == 1) {
    return z;
  }
  z = BigInt(z);
  i = BigInt(i);
  var log2z = wasm_z_numbits(z);
  var i_minus_1 = i - 1n;
  // Start with an upper bound of the root
  var x = 1n << ((BigInt(log2z) + i_minus_1) / i);
  while (1) {
      // Use Newton's method to get a better approximation of the root
      var next = ((i_minus_1 * x) + (z / (x ** i_minus_1))) / i;
      // The sequence is strictly decreasing until we reach the result
      // See https://github.com/waldemarhorwat/integer-roots for a proof
      if (x <= next) {
          return wasm_z_normalize(x);
      }
      x = next
  }
}

//Provides: wasm_z_invert
//Requires: wasm_z_gcdext_intern, wasm_z_normalize
function wasm_z_invert(a, n) {
  // Because [a.modInv(n)] produces different results for edge cases,
  // we wrote our own implementation based on gcdext_intern.
  a = BigInt(a);
  n = BigInt(n);
  var n_abs = (n < 0) ? (-n) : n;
  var x = wasm_z_gcdext_intern(a, n);
  var r = BigInt(x[1]);
  var tmp = (a * r) % n;
  if (tmp < 0) tmp += n_abs;
  if (r < 0) r += n_abs;
  if (tmp == 1) {
    return wasm_z_normalize(r);
  }
  return null;
}

//Provides: wasm_z_perfect_power
//Requires: wasm_z_numbits, wasm_z_root
function wasm_z_perfect_power(z) {
  // Return true if op is a perfect power, i.e., if there exist integers a and
  // b, with b > 1, such that op = a^b.
  // Otherwise false.
  if (z == 0 || z == 1 || z == -1) {
    return 1;
  }
  z = BigInt(z);
  var log2z = wasm_z_numbits(z);
  var zp = (z >= 0) ? z : -z;
  for (var b = 2; b <= log2z; b++) {
    if (z < 0 && b % 2 == 0) continue;
    var p = BigInt(wasm_z_root(zp, b));
    if (z < 0) p = -p;
    var r = p ** BigInt(b);
    if (z == r) {
      return 1;
    }
  }
  return 0;
}

//Provides: wasm_z_perfect_square
//Requires: wasm_z_root
function wasm_z_perfect_square(z) {
  if (z < 0) return 0;
  var root = BigInt(wasm_z_root(z, 2));
  return +(root * root == z);
}

//Provides: wasm_z_extract
//Requires: wasm_z_normalize
function wasm_z_extract(z1, pos, len) {
  return wasm_z_normalize(BigInt.asUintN(len, BigInt(z1) >> BigInt(pos)));
}

//Provides: wasm_z_gcdext_intern
//Requires: ml_z_gcd, wasm_z_normalize
function wasm_z_gcdext_intern(z1, z2) {
  z1 = BigInt(z1);
  z2 = BigInt(z2);
  var gcd = ml_z_gcd(z1, z2);
  var a = z1;
  var b = z2;
  var x = 0n;
  var lastx = 1n;
  var y = 1n;
  var lasty = 1n;
  var q, t, r;
  while (b != 0n) {
    q = a / b;
    r = a - (q * b);
    t = x;
    x = lastx - (q * x);
    lastx = t;
    t = y;
    y = lasty - (q * y);
    lasty = t;
    a = b;
    b = r;
  }
  if (a < 0)
    return [wasm_z_normalize(-a), wasm_z_normalize(-lastx)]
  else
    return [wasm_z_normalize(a), wasm_z_normalize(lastx)]
}

//Provides: wasm_z_trailing_zeros const
function wasm_z_trailing_zeros(z) {
  if (z < 0) z = - z;
  var i = 0;
  z = (z ^ (z - 1n)) >> 1n;
  for (i = 0; z != 0; i++) {
    z = z >> 1n;
  }
  return i;
}

//Provides: wasm_z_popcount
function wasm_z_popcount(z) {
  if (z < 0) return -1;
  for (var i = 0; z != 0n; i++) {
    z = z & (z - 1n);
  }
  return i;
}

//Provides: wasm_z_hamdist
//Requires: wasm_z_popcount
function wasm_z_hamdist(z1, z2) {
  return wasm_z_popcount(BigInt(z1) ^ BigInt(z2));
}


//Provides: wasm_z_size const
function wasm_z_size(z1) {
  if (z1 < 0) z1 = -z1;
  var n = 0;
  var upperBound = 1n;
  while (upperBound <= z1) {
    n += 1;
    upperBound <<= 32n;
  }
  return n;
}

//Provides: wasm_z_divisible
function wasm_z_divisible(a, b) {
  return +(BigInt(a) % BigInt(b) == 0)
}

//Provides: wasm_z_remove
//Requires: wasm_z_normalize
function wasm_z_remove(a, b) {
  a = BigInt(a);
  b = BigInt(b);
  var i = 0;
  while (a % b == 0) {
    a = a / b;
    i++;
  }
  return [wasm_z_normalize(a), i];
}

//Provides: wasm_z_fac
//Requires: wasm_z_facM
function wasm_z_fac(i) {
  return wasm_z_facM(i, 1);
}

//Provides: wasm_z_fac2
//Requires: wasm_z_facM
function wasm_z_fac2(i) {
  return wasm_z_facM(i, 2);
}

//Provides: wasm_z_facM
//Requires: wasm_z_normalize
function wasm_z_facM(i, m) {
  m = BigInt(m);
  var current = BigInt(i);
  var res = 1n;
  while (current > 0) {
    res *= current
    current = current - m;
  }
  return wasm_z_normalize(res);
}

//Provides: wasm_z_fib
//Requires: wasm_z_normalize
function wasm_z_fib(i) {
  if (i == 0 || i == 1) return i;
  var a = 0n, b = 1n;
  for (var k = 1; k < i; k++) {
    var b2 = b;
    b = a + b;
    a = b2;
  }
  return wasm_z_normalize(b);
}

//Provides: wasm_z_lucnum
//Requires: wasm_z_normalize
function wasm_z_lucnum(i) {
  if (i == 0) return 2;
  if (i == 1) return 1;
  var a = 2n, b = 1n;
  for (var k = 1; k < i; k++) {
    var b2 = b;
    b = a + b;
    a = b2;
  }
  return wasm_z_normalize(b);
}

//Provides: wasm_z_jacobi
function wasm_z_jacobi(n, k) {
  n = BigInt(n);
  k = BigInt(k);
  n = n % k;
  if (n < 0) n += k;
  var t = 1;
  while (n) {
    while ((n & 1n) == 0) {
      n >>= 1n;
      var r = k & 7n;
      if (r == 3 || r == 5) {
        t = -t
      }
    }
    var n1 = n, k1 = k;
    n = k1;
    k = n1;
    if (((n & 3n) == 3) && ((k & 3n) == 3)) {
      t = -t
    }
    n = n % k;
  }
  if (k == 1)
    return t
  else
    return 0
}

//Provides: wasm_z_bin
//Requires: wasm_z_normalize, ml_z_neg
function wasm_z_bin(n, k) {
  var n = BigInt(n);
  var k = BigInt(k);
  var coeff;
  if (n < 0) {
    coeff = wasm_z_bin(-n + k - 1n, k);
    if (k & 1n) coeff = ml_z_neg(coeff);
    return coeff;
  }
  if (k > n) return 0;
  var k2 = n - k;
  if (k2 < k) k = k2;
  var coeff = 1n;
  for (var i = 1n; i <= k; i++)
    coeff = coeff * (n - i + 1n) / i;
  return wasm_z_normalize(coeff);
}

//Provides: wasm_z_probab_prime const
//Requires: jsoo_bigint_mod_pow, jsoo_bigint_rand_between
//Note: called with [1n] from [wasm_z_primorial]
function wasm_z_probab_prime(z, i) {
  if (z < 0) z = -z;

  // Test for basic primes (multiples of 2, 3, 5)
  if (z == 1) return 0;
  if (z == 2 || z == 3 || z == 5) return 1;
  let n = BigInt(z);
  if (n % 2n === 0n || n % 3n === 0n || n % 5n === 0n) return 0;
  if (z < 49) return 1;

  // Miller-Rabin test
  let nPrev = n - 1n,
    b = nPrev,
    r = 0,
    a, d, j, x;
  while (b % 2n === 0n) b /= 2n, r++;
  next: for (j = 0; j < i; j++) {
    a = jsoo_bigint_rand_between(2n, n - 2n);
    x = jsoo_bigint_mod_pow(a, b, n);
    if (x === 1n || x === nPrev) continue;
    for (d = r - 1; d != 0; d--) {
      x = (x * x) % n;
      if (x === 1n) return 0;
      if (x === nPrev) continue next;
    }
    return 0;
  }
  return 1;
}

//Provides: wasm_z_primorial
//Requires: wasm_z_probab_prime, wasm_z_normalize, caml_invalid_argument
function wasm_z_primorial(a) {
  let z1 = 1n;
  let res = 1n;
  while (z1 <= a) {
    if (wasm_z_probab_prime(z1, 25)) {
      res *= z1;
    }
    if (z1 === 1n || z1 === 2n) z1 += 1n;
    else z1 += 2n;
  }
  return wasm_z_normalize(res);
}

//Provides: wasm_z_nextprime const
//Requires: wasm_z_normalize, wasm_z_probab_prime
function wasm_z_nextprime(z1) {
  // Interestingly, the zarith next_prime only returns
  // probabalistic primes.  We do the same, with the
  // same probablistic parameter of 25.
  // https://fossies.org/dox/gmp-6.1.2/mpz_2nextprime_8c_source.html

  if (z1 < 1 || z1 == 1) {
    return 2;
  }

  let z1n = BigInt(z1)
  if ((z1n & 1n) === 1n) {
    z1n += 2n;
  } else {
    z1n += 1n;
  }

  for (; ;) {
    if (wasm_z_probab_prime(z1n, 25)) {
      return wasm_z_normalize(z1n);
    } else {
      z1n += 2n;
    }
  }
}
