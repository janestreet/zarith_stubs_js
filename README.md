# Zarith Stubs JS

[Zarith](https://github.com/ocaml/Zarith) is an OCaml library that
implements common operations over arbitrary-precision integers and
rationals.  It is implemented via a C api that primarily calls out to
[Gnu Multiple Precision Arithmetic Library](https://gmplib.org).
Because of that C API, Zarith could not be compiled via Js_of_ocaml
until now.

[Zarith Stubs JS](https://github.com/janestreet/zarith_stubs_js) is a
reimplementation of the native C functions in JavaScript. It uses the native
[BigInt][BigInt] primitive type available in modern browsers (see the
[compatibility table][browser_compatibility]).

## How to use it ?

In order to use `zarith` with `js_of_ocaml`, just provide the javascript
runtime files to the `js_of_ocaml` compiler and use `zarith` as you would
normally do.

In practice,
- if you use [dune](https://github.com/ocaml/dune) to build your project, just
  add `zarith_stubs_js` as a library dependency.
- if you don't use dune, you need to find a way to pass the
  [runtime.js](runtime.js) file to the `js_of_ocaml` compiler command line.

## Usage with Web Workers

This library uses the native BigInt primitive to represent values of type
`Z.t`, making it compatible with the [structured clone algorithm][ssc]. This
allows safely transfering values between different execution contexts such as
web workers.

## Usage with legacy browsers

This library requires a modern execution environment with support for the
BigInt primitive (this includes all major browsers released after 2020 -- see
the [compatibility table][browser_compatibility]). If support for legacy
execution environments without support for the BigInt primitive is required,
older versions of this library (up to v0.17) should be used (without support
for data transfer to web workers).

[BigInt]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
[browser_compatibility]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt#browser_compatibility
[ssc]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Structured_clone_algorithm
