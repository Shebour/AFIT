(** Test suites for builtin basic_arithmetic ml file using oUnit. *)

open OUnit2
open Builtin
open Basic_arithmetics
open Test_builtin_templates

let () = let t_list = [(32, 6), 2; (18, 12), 6; (-18, -12), 6] in
         run_test template_2_1 "GCD Function" gcd t_list
;;

let () = let t_list = [(18, 22), (5, -4, 2); (22, 18), (-4, 5, 2);
                       (17, 21), (5, -4, 1); (21, 17), (-4, 5, 1)]
         in
         run_test template_2_3 "Bezout Function" bezout t_list
;;
