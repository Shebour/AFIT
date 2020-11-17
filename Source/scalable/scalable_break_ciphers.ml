(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = ([],[])



  (*let (x,_) = key
                in let rec break_rec q i =
                     match q with
                       |q when compare_b q i = (-1) -> i,q
                       |q when compare_b q i = 0 -> i,q
                       |q when mod_b q i = [] -> i, quot_b q i
                       |_ -> break_rec q (add_b i [0;1])
                   in break_rec x [0;0;1];;*)
