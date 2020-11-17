(** Test suites for builtin power ml file using oUnit. *)

open OUnit2
open Builtin
open Basic_arithmetics
open Test_primes
open Generate_primes
open Test_builtin_templates


let () = let t_list = [(2, [2]); (3, [2; 3]); (6, [2; 3; 5])] in
         run_test template_1_L "Initialization list for eratosthenes" init_eratosthenes t_list
;;

let () = let t_list =  [(2, [2]);
                        (3, [2; 3]);
                        (6, [2; 3; 5]);
                        (25, [2; 3; 5; 7; 11; 13; 17; 19; 23])
                       ]
         in
         run_test template_1_L "Erathosthenes Sieve" eratosthenes t_list
;;

let () = let t_list = [((20, is_prime), [(2, 5); (3, 7); (5, 11); (11, 23)])]
         in
         run_test template_1f_L2 "Double Primes Generator" double_primes t_list
;;

let () = let t_list = [((20, is_prime), [(2, 3); (3, 5); (5, 7); (11, 13); (17, 19)])]
         in
         run_test template_1f_L2 "Twin Primes Generator" twin_primes t_list
;;
