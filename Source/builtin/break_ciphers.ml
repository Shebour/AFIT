(** Factoring Built-In Int Primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = let (x,_) = key
                in let rec break_rec q i =
                     match q with
                       |q when q <= i -> i,q
                       |q when q mod i = 0 -> i, quot q i
                       |_ -> break_rec q (i +1)
                   in break_rec x 2;;
