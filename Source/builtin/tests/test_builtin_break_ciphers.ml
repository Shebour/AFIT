(** Test suites for builtin break_cifers ml file using oUnit. *)

open OUnit2
open Builtin
open Basic_arithmetics
open Break_ciphers
open Test_builtin_templates

(* Only tests for RSA for now. *)

let () = let t_list = [((99400891, 36199003), (9967, 9973))]
         in
         run_test template_cple_2 "Break Cifer RSA Test" break t_list
;;
