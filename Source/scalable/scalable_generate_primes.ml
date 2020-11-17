(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power
open Scalable_test_primes

(* Initializing list of bitarrays for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n = let n = abs_b (from_int (n))
                          in if n = [0;0;1]
                            then [[0;0;1]]
                            else let rec erat n i =
                               match n with
                                 |n when compare_b n i = (-1) -> []
                                 |_                           -> i::(erat n (add_b i [0;0;1]))
                             in (from_int 2)::(erat n [0;1;1]);;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n = let n = abs_b (from_int (n)) and list = init_eratosthenes n
                     in let rec erat n list =
                          match list with
                          |[] -> []
                          |i::list when is_prime i -> i::(erat n list)
                          |i::list -> erat n list
                        in erat n list;;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = let opening = open_out file
                         in let rec write li opening = match li with
                           |[] -> close_out opening;
                           |e::[] -> begin
                             output_string opening (string_of_int (to_int e));
                             close_out opening;
                           end
                           |e::li -> begin
                             output_string opening (string_of_int (to_int e));
                             output_string opening "\n";
                             write li opening;
                             close_out opening;
                           end
                            in write li opening;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None;;

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = let rec create_list_rec test =
                         match input_line_opt in_c with
                         |None   -> test
                         |Some l -> l::(create_list_rec test)
                       in create_list_rec [];;

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = let string_list_envcam = create_list (open_in file)
                            in let rec read_list_rec string_list_envcam =
                                 match string_list_envcam with
                                 |[]      -> []
                                 |e::list -> from_int(int_of_string e)::read_list_rec list
                                           in read_list_rec string_list_envcam;;

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t;;

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime bitarrays for specific or fun
   purposes.
 *)

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime =  let limit = from_int limit in
                                   let rec double_primes_rec i =
                                     match i with
                                       |i when compare_b i limit = 1 -> []
                                       |_ -> if (isprime i) && (isprime (add_b (mult_b i [0;0;1]) [0;1]))
                                         then (i,(add_b (mult_b i [0;0;1]) [0;1]))::double_primes_rec (add_b i [0;1])
                                         else double_primes_rec (add_b i [0;1])
                                   in double_primes_rec [0;0;1];;

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime = let limit = from_int limit in
                                let rec twin_primes_rec i =
                                  match i with
                                    |i when compare_b i limit = 1 -> []
                                    |_ -> if i = [0;0;1]
                                      then (i, add_b i [0;1])::(twin_primes_rec (add_b i [0;1]))
                                      else if (isprime i) && (isprime (add_b i [0;0;1]))
                                      then (i, add_b i [0;0;1])::twin_primes_rec (add_b i [0;1])
                                      else twin_primes_rec (add_b i [0;1])
                                in twin_primes_rec [0;0;1];;
