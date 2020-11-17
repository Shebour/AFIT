(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

 *)
let power_bis x n = let rec power_rec x n = if n = 0 then
    1
  else if n = 1 then
    x
  else if n mod 2 = 0 then
    power_rec (x * x) (n / 2)
  else
    x * power_rec (x * x) (n / 2)
                in power_rec x n;;

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
 *)

let from_int x = let rec bitarrays x = let a = abs(x) in
                        match x with
                        |0 -> []
                        |_ -> (a mod 2)::bitarrays (a/2)
                            in if x > 0 then 0::bitarrays x else if x < 0 then 1::bitarrays x else [] ;;

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA = let rec bitarrays bA i =
                  match bA with
                  |[] -> 0
                  |a::e::bA when i = 0 -> if a = 0 then e + bitarrays bA 1 else (-e) + (-1)* (bitarrays bA 1)
                  |a::bA -> a* (power_bis 2 i) + bitarrays bA (i + 1)
                in bitarrays bA 0;;

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA = let rec print_b_rec bA i =
                   match bA with
                   |[] -> ""
                   |a::bA when i = 0 -> if a = 1 then "-"^print_b_rec bA (i+1) else print_b_rec bA (i+1)
                   |a::bA -> print_b_rec bA (i+1)^string_of_int(a)
                                      in print_string(print_b_rec bA 0);;

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let compare_n nA nB = if List.length nA < List.length nB then (-1) else
                        if List.length nA > List.length nB then 1 else
  let nA = List.rev(nA) and nB = List.rev(nB) in
  let rec compare_n_rec nA nB =
    match (nA,nB) with
      |(e1::nA,e2::nB) -> if e1 = e2
        then compare_n_rec nA nB
        else
          if e1 > e2
          then 1
          else (-1)
      |_ -> 0
  in compare_n_rec nA nB ;;


(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = if compare_n nA nB = 1 then true else false;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = if compare_n nA nB = (-1) then true else false;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = if compare_n nA nB = -1 then false else true;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = if compare_n nA nB = 1 then false else true;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
 *)

let compare_b bA bB = match (bA,bB) with
  |([],e2::bB) -> if e2 = 1 then 1 else (-1)
  |(e1::bA,[]) -> if e1 = 1 then (-1) else 1
  |(e1::bA,e2::bB) when e1 != e2 -> if e1 < e2 then 1 else (-1)
  |(e1::bA,e2::bB) when e1 = 1 && e2 = 1 -> if compare_n bA bB = 1
    then (-1)
    else
      if compare_n bA bB= (-1)
      then 1
      else 0
  |(e1::bA,e2::bB) when e1 = 0 && e2 = 0 -> compare_n bA bB
  |_ -> 0;;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)

let (<<) bA bB = if compare_b bA bB = (-1) then true else false;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = if compare_b bA bB = 1 then true else false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = if compare_b bA bB = 1 then false else true;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = if compare_b bA bB = (-1) then false else true;;

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
  |e1::bA -> if e1 = 0 then 1 else (-1)
  |_ -> 0;;

(** Absolute value of bitarray.
    @param bA Bitarray.
 *)
let abs_b bA = match bA with
  |e::bA -> if e = 1 then 0::bA else e::bA
  |_ -> [];;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = a / 2;;

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = a mod 2;;

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a);;

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
 *)
let add_n nA nB = let rec add_n_rec nA nB r =
                    match (nA,nB) with
                    |([],[]) -> if r = 1 then 1::[] else []
                    |(e1::nA,e2::nB) when e1 + e2 + r = 0 -> 0::add_n_rec nA nB 0
                    |(e1::nA,e2::nB) when e1 + e2 + r = 1 -> 1::add_n_rec nA nB 0
                    |(e1::nA,e2::nB) when e1 + e2 + r = 2 -> 0::add_n_rec nA nB 1
                    |(e1::nA,e2::nB) when e1 + e2 + r = 3 -> 1::add_n_rec nA nB 1
                    |(e1::nA,[]) -> add_n_rec (e1::nA) [0] r
                    |([],e2::nB) -> add_n_rec [0] (e2::nB) r
                    |_ -> []
                  in add_n_rec nA nB 0;;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let remove_0 l = let l = List.rev(l)
                 in let rec remove_0_rec l =
                      match l with
                        |[] -> []
                        |e::l when e = 1 -> e::l
                        |e::l -> remove_0_rec l
                    in List.rev(remove_0_rec l);;

let diff_n nA nB = let rec diff_n_rec nA nB r =
                    match (nA,nB) with
                      |([],[])                                   -> []
                      |(e1::[],e2::[])                           -> if e1 - (e2 + r) = 1 then e1::[] else []
                      |(e1::nA,e2::nB) when e1 - (e2 + r) = 0    -> 0::diff_n_rec nA nB 0
                      |(e1::nA,e2::nB) when e1 - (e2 + r) = 1    -> 1::diff_n_rec nA nB 0
                      |(e1::nA,e2::nB) when e1 - (e2 + r) = (-1) -> 1::diff_n_rec nA nB 1
                      |(e1::nA,e2::nB) when e1 - (e2 + r) = (-2) -> 0::diff_n_rec nA nB 1
                      |(e1::nA,[])                               -> diff_n_rec (e1::nA) [0] r
                      |_                                         -> []
                   in diff_n_rec nA nB 0;;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB =remove_0(match (bA,bB) with
  |([],[]) -> []
  |(e1::bA,e2::bB) when e1 = e2 && e1 = 0 -> 0::add_n bA bB
  |(e1::bA,e2::bB) when e1 = e2 && e1 = 1 -> 1::add_n bA bB
  |(e1::bA,e2::bB) when e1 = 1 && e2 = 0 -> if compare_n bA bB = 1
    then 1::diff_n bA bB
    else
      if compare_n bA bB = (-1)
      then 0::diff_n bB bA
      else []
  |(e1::bA,e2::bB) when e1 = 0 && e2 = 1 -> if compare_n bA bB = 1
    then 0::diff_n bA bB
    else
      if compare_n bA bB = (-1)
      then 1::diff_n bB bA
      else []
  |(bA,[]) -> bA
  |([],bB) -> bB
  |_ -> []);;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB = remove_0(match (bA,bB) with
  |([],[]) -> []
  |(e1::bA,e2::bB) when e1 = e2 && e1 = 0 -> if compare_n bA bB = 1
    then 0::diff_n bA bB
    else
      if compare_n bA bB = 0
      then []
      else 1::diff_n bB bA
  |(e1::bA,e2::bB) when e1 = e2 && e1 = 1 -> add_b (e1::bA) (0::bB)
  |(e1::bA,e2::bB) when e1 = 1 && e2 = 0 -> 1::add_n bA bB
  |(e1::bA,e2::bB) when e1 = 0 && e2 = 1 -> 0::add_n bA bB
  |([],e2::bB) when e2 = 0 -> 1::bB
  |([],e2::bB) when e2 = 1 -> 0::bB
  |(e1::bA,[]) -> e1::bA
  |_ -> []);;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
 *)


let shift bA d = if d = 0
  then bA
  else
    if bA = []
    then []
    else let rec shift_rec bA d i =
           match (d,bA) with
             |(d,a::bA) when i = 0 -> a::(shift_rec bA d 1)
             |(1,bA) -> 0::bA
             |(d,bA) -> 0::(shift_rec bA (d - 1) 1)
         in shift_rec bA d 0;;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)

let mult_n nA nB = let rec mult_n_rec nA nB =
                     match (nA,nB) with
                       |(nA,[]) -> []
                       |(nA,e::nB) when e = 1 -> let nC = 0::nA in (add_n nA (mult_n_rec nC nB))
                       |(nA,e::nB) when e = 0 -> let nC = 0::nA in mult_n_rec nC nB
                       |_ -> []
                   in mult_n_rec nA nB;;

let mult_b bA bB = let rec mult_b_rec bA bB =
                     match (bA,bB) with
                       |(e1::bA,e2::bB) when e1 = e2 -> 0::(mult_n bA bB)
                       |(e1::bA,e2::bB) -> 1::(mult_n bA bB)
                       |_ -> []
                   in mult_b_rec bA bB;;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)

let quot_n nA nB = let rec quot_n_rec nA nB i =
                     match (nA,nB) with
                       |(nA,nB) when compare_n nA nB = (-1) -> i
                       |_ -> quot_n_rec (diff_n nA nB) nB (add_n i [1])
                   in quot_n_rec nA nB [];;

let quot_b bA bB = let rec quot_b_rec bA bB =
                     match (bA,bB) with
                     |(e1::bA,e2::bB) when e1 = e2 -> if compare_n bA bB = (-1) then [] else 0::(quot_n (bA) (bB))
                     |(e1::bA,e2::bB)              -> if mult_n (quot_n bA bB) bB = bA then 1::(quot_n (bA) (bB)) else 1::(add_n (quot_n bA bB) [1])
                     |_                            -> []
                   in quot_b_rec bA bB;;

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = diff_b bA (mult_b bB (quot_b bA bB));;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = (quot_b bA bB, mod_b bA bB);;
