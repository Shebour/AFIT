(** Test suites for scalable power ml file using oUnit. *)

open OUnit2
open Scalable
open Scalable_basic_arithmetics
open Scalable_test_primes
open Scalable_generate_primes
open Test_scalable_templates


let () = let t_list = [(2, [from_int 2]);
                       (3, [from_int 2; from_int 3]);
                       (6, [from_int 2; from_int 3; from_int 5])]
         in
         run_test template_1i_L "Initialization list for eratosthenes" init_eratosthenes t_list
;;

(* let () = let t_list =  [(2, [from_int 2]);
 *                         (3, [from_int 2; from_int 3]);
 *                         (6, [from_int 2; from_int 3; from_int 5]);
 *                         (25, [from_int 2; from_int 3; from_int 5;
 *                                        from_int 7; from_int 11; from_int 13;
 *                                        from_int 17; from_int 19; from_int 23])
 *                        ]
 *          in
 *          run_test template_1i_L "Erathosthenes Sieve" eratosthenes t_list
 * ;;
 *
 * let () = let t_list = [((20, is_prime),
 *                         [(from_int 2, from_int 5);
 *                          (from_int 3, from_int 7);
 *                          (from_int 5, from_int 11);
 *                          (from_int 11, from_int 23)])
 *                       ]
 *          in
 *          run_test template_1f_L2 "Double Primes Generator" double_primes t_list
 * ;;
 *
 * let () = let t_list = [((20, is_prime),
 *                         [(from_int 2, from_int 3);
 *                          (from_int 3, from_int 5);
 *                          (from_int 5, from_int 7);
 *                          (from_int 11, from_int 13);
 *                          (from_int 17, from_int 19)])
 *                       ]
 *          in
 *          run_test template_1f_L2 "Twin Primes Generator" twin_primes t_list
 * ;; *)
