(** Test suites for scalable power ml file using oUnit. *)

open OUnit2
open Scalable
open Scalable_basic_arithmetics
open Scalable_power
open Test_scalable_templates

let () =  let t_list = [((from_int (-1), from_int 12), from_int 1);
                        ((from_int (-1), from_int 11), from_int (-1) );
                        ((from_int 0, from_int 2), from_int 0);
                        ((from_int 3, from_int 1), from_int 3);
                        ((from_int 5, from_int 0), from_int 1);
                        ((from_int (-2), from_int 2), from_int 4);
                        ((from_int (-2), from_int 3), from_int (-8));
                        ((from_int 2, from_int 5), from_int 32);
                        ((from_int 3, from_int 3), from_int 27)]
          in
          run_test template_2_1 "Pow Function" pow t_list
;;

let () = let t_list = [((from_int (-1), from_int 12), from_int 1);
                       ((from_int (-1), from_int 11), from_int (-1));
                       ((from_int 0, from_int 2), from_int 0);
                       ((from_int 3, from_int 1), from_int 3);
                       ((from_int 5, from_int 0), from_int 1);
                       ((from_int (-2), from_int 2), from_int 4);
                       ((from_int (-2), from_int 3), from_int (-8));
                       ((from_int 2, from_int 5), from_int 32);
                       ((from_int 3, from_int 3), from_int 27)]
         in
         run_test template_2_1 "Power Function" power t_list
;;

let () = let t_list =  [((from_int (-1), from_int 12, from_int 10), from_int 1);
                        ((from_int (-1), from_int 11, from_int 11), from_int 10);
                        ((from_int 0, from_int 2, from_int 3), from_int 0);
                        ((from_int 3, from_int 1, from_int 3), from_int 0);
                        ((from_int 5, from_int 0, from_int 2), from_int 1);
                        ((from_int (-2), from_int 2, from_int 5), from_int 4);
                        ((from_int (-2), from_int 3, from_int 9), from_int 1);
                        ((from_int 2, from_int 5, from_int 17), from_int 15);
                        ((from_int 3, from_int 3, from_int 17), from_int 10)]
         in
         run_test template_3_1 "Modular Power Function" mod_power t_list
;;

let () = let t_list = [((from_int (-1), from_int 12, from_int 7), from_int 1);
                       ((from_int (-1), from_int 11, from_int 11), from_int 10);
                       ((from_int 0, from_int 2, from_int 3), from_int 0);
                       ((from_int 3, from_int 1, from_int 3), from_int 0);
                       ((from_int 5, from_int 0, from_int 2), from_int 1);
                       ((from_int (-2), from_int 2, from_int 5), from_int 4);
                       ((from_int (-2), from_int 3, from_int 5), from_int 2);
                       ((from_int 2, from_int 5, from_int 17), from_int 15);
                       ((from_int 3, from_int 3, from_int 17), from_int 10)]
         in
         run_test template_3_1 "Power Modulo Prime Function" prime_mod_power t_list
;;
