(** Basic arithmetics for ordered euclidian ring. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB = let rec gcd_rec_b bA bB = let r = mod_b bA bB in
                  if r = []
                  then bB
                  else
                    if compare_b (gcd_rec_b bB r) [] = (-1)
                    then mult_b [1;1] (gcd_rec_b bB r)
                    else gcd_rec_b bB r
                  in gcd_rec_b bA bB;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  let rec bezout_b_rec r1 u1 v1 r2 u2 v2 =
    if r2 = []
    then (u1,v1,r1)
    else bezout_b_rec r2 u2 v2 (mod_b r1 r2) (diff_b u1 (mult_b (quot_b r1 r2) u2)) (diff_b v1 (mult_b (quot_b r1 r2) v2))
                                                                                     in bezout_b_rec bA [0;1] [] bB [] [0;1];;
