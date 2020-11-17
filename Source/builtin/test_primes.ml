(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n = match n with
  |1                  -> false
  |2                  -> true
  |n when n mod 2 = 0 -> false
  |_                  -> let rec is_prime_rec n i =
                           match i with
                           |i when i > n / 2   -> true
                           |i when n mod i = 0 -> false
                           |_                  -> is_prime_rec n (i + 2)
                         in is_prime_rec n 3;;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)

let is_pseudo_prime p test_seq = let rec is_pseudo_prime_rec p test_seq =
                                   match test_seq with
                                     |[] -> true
                                     |e::l1 -> if p = 2 then true
                                       else
                                         if gcd e p > 1 then false
                                         else
                                           if modulo (power p (e - 1)) e = 1 then true && is_pseudo_prime_rec p l1
                                           else
                                             is_pseudo_prime_rec p l1
                                 in is_pseudo_prime_rec p test_seq;;
