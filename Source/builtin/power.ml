(** Power function implementations for built-in integers *)

open Builtin
open Basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n = let rec pow_rec x n = match n with
    0 -> 1
  |_ -> x * pow_rec x (n - 1)
              in pow_rec x n;;


(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n = let rec power_rec x n = if n = 0 then
    1
  else if n = 1 then
    x
  else if n mod 2 = 0 then
    power_rec (x * x) (n / 2)
  else
    x * power_rec (x * x) (n / 2)
                in power_rec x n;;

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)

let mod_power x n m = let rec mod_power_rec x n m = let b = if n = 0
  then 1
  else let a = mod_power_rec x (n/2) m
       in if n mod 2 = 0
         then (a*a) mod m
         else (((a*a) mod m ) * x) mod m in
                                                    if x < 0 && n mod 2 = 1
                                                    then b + m
                                                    else b
                      in mod_power_rec x n m;;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let rec prime_mod_power x n p = if n < p
                                then mod_power x n p
                                else let a,b = div n p
                                     in modulo ((prime_mod_power x a p) * (mod_power x b p)) p;;
