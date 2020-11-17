(** Generating primes *)

open Builtin
open Basic_arithmetics
open Test_primes


(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n number of elements in the list of integers.
 *)

let init_eratosthenes n =
  let n = abs(n)
  in let rec erat n i =
       match n with
       |n when i > n -> []
       |_            -> i::erat n (i+2)
           in 2::erat n 3;;


(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)


let eratosthenes n = let n = abs(n) and list = init_eratosthenes n
                     in let rec erat n list = match list with
                          |[]                      -> []
                          |i::list when is_prime i -> i::erat n list
                          |i::list                 -> erat n list
                        in erat n list;;


(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)

let write_list li file = let opening = open_out file
                         in let rec write li opening = match li with
                              |[]    -> close_out opening;
                              |e::[] -> begin
                                  output_string opening (string_of_int e);
                                  close_out opening;
                                end
                              |e::li -> begin
                                  output_string opening (string_of_int e);
                                  output_string opening "\n";
                                  write li opening;
                                  close_out opening;
                                end
                                      in write li opening;;


(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c = let rec create_list_rec test =
                         match input_line_opt in_c with
                         |None   -> test
                         |Some l -> l::(create_list_rec test)
                       in create_list_rec [];;

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = let string_list_envcam = create_list (open_in file)
                            in let rec read_list_rec string_list_envcam =
                                 match string_list_envcam with
                                 |[]      -> []
                                 |e::list -> int_of_string e::read_list_rec list
                                           in read_list_rec string_list_envcam;;
                                      

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "Builtin.generate_primes.last_element: Your list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Builtin.generate_primes.last_two: List has \
                          to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime = let rec double_primes_rec i =
                                    match i with
                                    |i when i > limit -> []
                                    |_ -> if (isprime i) && (isprime ((i * 2) + 1))
                                          then (i, i * 2 + 1)::double_primes_rec (i + 1)
                                          else double_primes_rec (i + 1)
                                        in double_primes_rec 2;;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime = let rec twin_primes_rec i =
                                  match i with
                                  |i when i > limit -> []
                                  |_ -> if i = 2
                                        then (i, i + 1)::(twin_primes_rec (i + 1))
                                        else if (isprime i) && (isprime (i + 2))
                                        then (i, i + 2)::twin_primes_rec (i + 1)
                                        else twin_primes_rec (i + 1)
                                in twin_primes_rec 2;;
                                                           
