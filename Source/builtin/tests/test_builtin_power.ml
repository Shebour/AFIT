(** Test suites for builtin power ml file using oUnit. *)

open OUnit2
open Builtin
open Basic_arithmetics
open Power
open Test_builtin_templates

let () =  let t_list = [((-1, 12), 1); ((-1, 11), -1); ((0, 2), 0);
                        ((3, 1), 3); ((5, 0), 1); ((-2, 2), 4);
                        ((-2, 3), -8); ((2, 5), 32); ((3, 3), 27)]
          in
          run_test template_2_1 "Pow Function" pow t_list
;;

let () = let t_list = [((-1, 12), 1); ((-1, 11), -1); ((0, 2), 0);
                       ((3, 1), 3); ((5, 0), 1); ((-2, 2), 4);
                       ((-2, 3), -8); ((2, 5), 32); ((3, 3), 27)]
         in
         run_test template_2_1 "Power Function" power t_list
;;

let () = let t_list =  [((-1, 12, 10), 1); ((-1, 11, 11), 10); ((0, 2, 3), 0);
                        ((3, 1, 3), 0); ((5, 0, 2), 1); ((-2, 2, 5), 4);
                        ((-2, 3, 9), 1); ((2, 5, 17), 15); ((3, 3, 17), 10)]
         in
         run_test template_3_1 "Modular Power Function" mod_power t_list
;;

let () = let t_list = [((-1, 12, 7), 1); ((-1, 11, 11), 10); ((0, 2, 3), 0);
                       ((3, 1, 3), 0); ((5, 0, 2), 1); ((-2, 2, 5), 4);
                       ((-2, 3, 5), 2); ((2, 5, 17), 15); ((3, 3, 17), 10)]
         in
         run_test template_3_1 "Power Modulo Prime Function" prime_mod_power t_list
;;
