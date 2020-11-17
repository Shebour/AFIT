(** Test suites for builtin test_primes ml file using oUnit. *)

open OUnit2
open Scalable
open Scalable_basic_arithmetics
open Scalable_test_primes
open Test_scalable_templates

let () = let t_list = [(from_int 2, true); (from_int 3, true); (from_int 5, true);
                       (from_int 7, true); (from_int 11, true); (from_int 13, true);
                       (from_int 4, false); (from_int 6, false); (from_int 12, false);
                       (from_int 45, false); (from_int 77, false); (from_int 63, false)]
         in
         run_test template_1_b "Is Prime Function" is_prime t_list
;;

let () = let t_list = [((from_int 2, [from_int 2; from_int 4; from_int 8; from_int 12]), true);
                       ((from_int 11, [from_int 2; from_int 4; from_int 5; from_int 20]), true);
                       ((from_int 23, [from_int 2; from_int 9; from_int 15; from_int 18]), true);
                       ((from_int 29,[from_int 30; from_int 41; from_int 52]), true);
                       ((from_int 4, [from_int 2; from_int 9; from_int 15; from_int 18]), false);
                       ((from_int 22,[from_int 30; from_int 41; from_int 52]), false);
                       ((from_int 15, [from_int 2; from_int 9; from_int 15; from_int 18]), false);
                       ((from_int 27,[from_int 30; from_int 41; from_int 52]), false)
                      ]
         in
         run_test template_1L_b "Is Pseudo Prime Function" is_pseudo_prime t_list
;;
