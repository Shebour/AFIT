(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class in the
   built-in integer case.
*)

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n = let rec pow_rec x n =
                match n with
                  |[] -> [0;1]
                  |_ -> mult_b x (pow_rec x (diff_b n [0;1]))
                                  in pow_rec x n;;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n = let rec power_rec x n = if n = [] then [0;1]
  else if n = [0;1] then x
  else if mod_b n [0;0;1] = [] then power_rec (mult_b x x) (quot_b n [0;0;1])
  else mult_b x (power_rec (mult_b x x) (quot_b n [0;0;1]))
                in power_rec x n;;

(* Modular expnonentiation ; modulo a given natural (bitarray without
   sign bits).
*)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m = let rec mod_power_rec x n m =
                        match n with
                          |[] -> mod_b (from_int 1) m
                          |_ -> if n = from_int 1
                            then mod_b x m
                            else
                              if (mod_b n (from_int 2)) = []
                              then mod_b (mult_b((mod_power_rec x (quot_b n (from_int 2)) m)) (mod_power_rec x (quot_b n (from_int 2)) m)) m
                              else mod_b (mult_b(mod_power_rec x (diff_b n (from_int 1)) m) x) m
                      in mod_power_rec x n m;;
(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let rec prime_mod_power x n p = if compare_b n p = (-1)
                            then mod_power x n p
                            else let a,b = div_b n p
                                             in mod_b (mult_b (prime_mod_power x a p) (mod_power x b p)) p;;
