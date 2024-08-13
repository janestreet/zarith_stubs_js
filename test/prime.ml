open! Core
open! Import

module Ml_z_probab_prime = struct
  let%expect_test "print x, probab_prime x" =
    Static.quickcheck
      ~f:(fun x ->
        List.range 22 27
        |> List.map ~f:(fun i ->
          let probab_prime x i = not (Int.equal (Z.probab_prime x i) 0) in
          [%message (x : t) (i : int) (probab_prime x i : bool)])
        |> Sexp.List)
      ();
    [%expect {| ((hash 841bf3c0441e2a8f411a38399b51b11e) (uniqueness_rate 85.742188)) |}]
  ;;
end

module Ml_z_nextprime = struct
  let%expect_test "print x, nextprime x" =
    Static.quickcheck ~f:(fun x -> [%message (x : t) (nextprime x : t)]) ();
    [%expect "((hash e4324c97c8ef012678595c7c8203b8c8) (uniqueness_rate 85.742188))"]
  ;;
end

module Ml_z_primorial = struct
  let%expect_test "primorial x" =
    let open Zarith.Z in
    Sexp.List (List.init 100 ~f:(fun i -> [%message (i : int) (primorial i : t)]))
    |> print_s;
    [%expect
      {|
      (((i 0) ("primorial i" 1)) ((i 1) ("primorial i" 1))
       ((i 2) ("primorial i" 2)) ((i 3) ("primorial i" 6))
       ((i 4) ("primorial i" 6)) ((i 5) ("primorial i" 30))
       ((i 6) ("primorial i" 30)) ((i 7) ("primorial i" 210))
       ((i 8) ("primorial i" 210)) ((i 9) ("primorial i" 210))
       ((i 10) ("primorial i" 210)) ((i 11) ("primorial i" 2310))
       ((i 12) ("primorial i" 2310)) ((i 13) ("primorial i" 30030))
       ((i 14) ("primorial i" 30030)) ((i 15) ("primorial i" 30030))
       ((i 16) ("primorial i" 30030)) ((i 17) ("primorial i" 510510))
       ((i 18) ("primorial i" 510510)) ((i 19) ("primorial i" 9699690))
       ((i 20) ("primorial i" 9699690)) ((i 21) ("primorial i" 9699690))
       ((i 22) ("primorial i" 9699690)) ((i 23) ("primorial i" 223092870))
       ((i 24) ("primorial i" 223092870)) ((i 25) ("primorial i" 223092870))
       ((i 26) ("primorial i" 223092870)) ((i 27) ("primorial i" 223092870))
       ((i 28) ("primorial i" 223092870)) ((i 29) ("primorial i" 6469693230))
       ((i 30) ("primorial i" 6469693230)) ((i 31) ("primorial i" 200560490130))
       ((i 32) ("primorial i" 200560490130)) ((i 33) ("primorial i" 200560490130))
       ((i 34) ("primorial i" 200560490130)) ((i 35) ("primorial i" 200560490130))
       ((i 36) ("primorial i" 200560490130)) ((i 37) ("primorial i" 7420738134810))
       ((i 38) ("primorial i" 7420738134810))
       ((i 39) ("primorial i" 7420738134810))
       ((i 40) ("primorial i" 7420738134810))
       ((i 41) ("primorial i" 304250263527210))
       ((i 42) ("primorial i" 304250263527210))
       ((i 43) ("primorial i" 13082761331670030))
       ((i 44) ("primorial i" 13082761331670030))
       ((i 45) ("primorial i" 13082761331670030))
       ((i 46) ("primorial i" 13082761331670030))
       ((i 47) ("primorial i" 614889782588491410))
       ((i 48) ("primorial i" 614889782588491410))
       ((i 49) ("primorial i" 614889782588491410))
       ((i 50) ("primorial i" 614889782588491410))
       ((i 51) ("primorial i" 614889782588491410))
       ((i 52) ("primorial i" 614889782588491410))
       ((i 53) ("primorial i" 32589158477190044730))
       ((i 54) ("primorial i" 32589158477190044730))
       ((i 55) ("primorial i" 32589158477190044730))
       ((i 56) ("primorial i" 32589158477190044730))
       ((i 57) ("primorial i" 32589158477190044730))
       ((i 58) ("primorial i" 32589158477190044730))
       ((i 59) ("primorial i" 1922760350154212639070))
       ((i 60) ("primorial i" 1922760350154212639070))
       ((i 61) ("primorial i" 117288381359406970983270))
       ((i 62) ("primorial i" 117288381359406970983270))
       ((i 63) ("primorial i" 117288381359406970983270))
       ((i 64) ("primorial i" 117288381359406970983270))
       ((i 65) ("primorial i" 117288381359406970983270))
       ((i 66) ("primorial i" 117288381359406970983270))
       ((i 67) ("primorial i" 7858321551080267055879090))
       ((i 68) ("primorial i" 7858321551080267055879090))
       ((i 69) ("primorial i" 7858321551080267055879090))
       ((i 70) ("primorial i" 7858321551080267055879090))
       ((i 71) ("primorial i" 557940830126698960967415390))
       ((i 72) ("primorial i" 557940830126698960967415390))
       ((i 73) ("primorial i" 40729680599249024150621323470))
       ((i 74) ("primorial i" 40729680599249024150621323470))
       ((i 75) ("primorial i" 40729680599249024150621323470))
       ((i 76) ("primorial i" 40729680599249024150621323470))
       ((i 77) ("primorial i" 40729680599249024150621323470))
       ((i 78) ("primorial i" 40729680599249024150621323470))
       ((i 79) ("primorial i" 3217644767340672907899084554130))
       ((i 80) ("primorial i" 3217644767340672907899084554130))
       ((i 81) ("primorial i" 3217644767340672907899084554130))
       ((i 82) ("primorial i" 3217644767340672907899084554130))
       ((i 83) ("primorial i" 267064515689275851355624017992790))
       ((i 84) ("primorial i" 267064515689275851355624017992790))
       ((i 85) ("primorial i" 267064515689275851355624017992790))
       ((i 86) ("primorial i" 267064515689275851355624017992790))
       ((i 87) ("primorial i" 267064515689275851355624017992790))
       ((i 88) ("primorial i" 267064515689275851355624017992790))
       ((i 89) ("primorial i" 23768741896345550770650537601358310))
       ((i 90) ("primorial i" 23768741896345550770650537601358310))
       ((i 91) ("primorial i" 23768741896345550770650537601358310))
       ((i 92) ("primorial i" 23768741896345550770650537601358310))
       ((i 93) ("primorial i" 23768741896345550770650537601358310))
       ((i 94) ("primorial i" 23768741896345550770650537601358310))
       ((i 95) ("primorial i" 23768741896345550770650537601358310))
       ((i 96) ("primorial i" 23768741896345550770650537601358310))
       ((i 97) ("primorial i" 2305567963945518424753102147331756070))
       ((i 98) ("primorial i" 2305567963945518424753102147331756070))
       ((i 99) ("primorial i" 2305567963945518424753102147331756070)))
      |}]
  ;;
end
