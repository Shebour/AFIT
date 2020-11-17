(** Test suites for scalable break_cifers ml file using oUnit. *)

open OUnit2
open Scalable
open Scalable_basic_arithmetics
open Scalable_break_ciphers
open Test_scalable_templates

(* Only tests for RSA for now. *)

let () = let t_list = [((from_int 99400891, from_int 36199003),
                        (from_int 9967, from_int 9973))]
         in
         run_test template_cple_2 "Break Cifer RSA Test" break t_list
;;
