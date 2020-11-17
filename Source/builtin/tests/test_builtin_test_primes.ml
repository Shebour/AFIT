(** Test suites for builtin test_primes ml file using oUnit. *)

open OUnit2
open Builtin
open Basic_arithmetics
open Test_primes
open Test_builtin_templates

let () = let t_list = [(2, true); (3, true); (5, true);
                       (7, true); (11, true); (13, true);
                       (4, false); (6, false); (12, false);
                       (45, false); (77, false); (63, false)]
         in
         run_test template_1_b "Is Prime Function" is_prime t_list
;;

let () = let t_list = [((2, [2; 4; 8; 12]), true); ((11, [2; 4; 5; 20]), true);
                       ((23, [2; 9; 15; 18]), true); ((29,[30; 41; 52]), true);
                       ((4, [2; 9; 15; 18]), false); ((22,[30; 41; 52]), false);
                        ((15, [2; 9; 15; 18]), false); ((27,[30; 41; 52]), false)
                      ]
         in
         run_test template_1L_b "Is Pseudo Prime Function" is_pseudo_prime t_list
;;
