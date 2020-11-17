(** Test suites for builtin encoding_msg ml file using oUnit. *)

open OUnit2
open Builtin
open Basic_arithmetics
open Power
open Encoding_msg
open Test_builtin_templates

let () = let t_list = [(("Bashar", 7), 2294023860466)]
         in
         run_test template_s1_1 "Encode Function" encode t_list
;;

let () = let t_list = [((2294023860466, 7), "Bashar")]
         in
         run_test template_2_s "Decode Function" decode t_list
;;
