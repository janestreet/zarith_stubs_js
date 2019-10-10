# Zarith Stubs JS

[Zarith](https://github.com/ocaml/Zarith) is an OCaml library that implements common operations 
over arbitrary-precision integers and rationals.  It is implemented via a C api that primarily
calls out to [Gnu Multiple Precision Arithmetic Library](https://gmplib.org).  Because of that 
C API, Zarith could not be compiled via Js_of_ocaml until now.

[Zarith Stubs JS](https://github.com/janestreet/zarith_stubs_js) is a reimplementation of the 
native C functions in JavaScript.  It makes extensive use of 
[peterolson/BigInteger.js](https://github.com/peterolson/BigInteger.js) as a shim for browser 
BigInt functionality and also for the implementations of many numerical algorithms.



