(** Test suites for builtin basic_arithmetic ml file using oUnit. *)

open OUnit2
open Scalable
open Scalable_basic_arithmetics
open Test_scalable_templates

let () =
  let t_list =
    [(from_int 32, from_int 6), from_int 2;
     (from_int 18, from_int 12), from_int 6;
     (from_int (-18), from_int (-12)), from_int 6]
  in
  run_test template_2_1 "GCD Function" gcd_b t_list
;;

let () =
  let t_list =
    [(from_int 18, from_int 22), (from_int 5, from_int (-4), from_int 2);
     (from_int 22, from_int 18), (from_int (-4), from_int 5, from_int 2);
     (from_int 17, from_int 21), (from_int 5, from_int (-4), from_int 1);
     (from_int 21, from_int 17), (from_int (-4), from_int 5, from_int 1)]
  in
  run_test template_2_3 "Bezout Function" bezout_b t_list
;;
