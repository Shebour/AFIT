(** Basic arithmetics with built-in integers *)

open Builtin;;

(* Greater common divisor and smaller common multiple
   implemetations.
*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
*)
let gcd a b = let rec gcd_rec a b = let r = a mod b in
  if r = 0 then
    b
  else if gcd_rec b r < 0 then
    -1 * gcd_rec b r
  else gcd_rec b r
              in gcd_rec a b;;




(* Extended Euclidean algorithm. Computing Bezout Coefficients. *)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b = let a = abs(a) and b = abs(b) in
  let rec euc(u, v, r, u2, v2, r2) =
    if r2 = 0 then
      (u, v, gcd a b)
    else let q = r / r2 in
         euc(u2,v2,r2, u-q*u2, v-q*v2, r-q*r2)
  in euc(1,0,a,0,1,b);;




