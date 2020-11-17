(** Test suites for builtin builtin ml file using oUnit. *)

open OUnit2
open Builtin
open Test_builtin_templates

let () = let t_list = [(1, 1); (-1, -1); (0, 1)] in
         run_test template_1_1 "Sign Function" sign t_list
;;

let () = let t_list = [(10, 3), 3; (-10, 3), -4;
                       (10, 2), 5; (-10, 2), -5]
         in
         run_test template_2_1 "Quot Function" quot t_list
;;

let () = let t_list = [(10, 3), 1; (10, 2), 0;
                       (10, 2), 0; (-10, 2), 0]
         in
         run_test template_2_1 "Modulo Function" modulo t_list
;;

let () = let t_list = [(10, 3), (3, 1); (-10, 3), (-4, 2);
                       (10, 2), (5, 0); (-10, 2), (-5, 0)]
                       in
         run_test template_2_2 "Div Function" div t_list
;;
