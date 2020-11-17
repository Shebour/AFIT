(** Test suites for builtin cifers ml file using oUnit. *)

open OUnit2
open Builtin
open Basic_arithmetics
open Power
open Ciphers
open Test_builtin_templates


let str2list_f str f =
 let rec __str2list i =
   if i >= String.length str then
        []
   else
     (f str.[i])::__str2list (i + 1)
 in
 __str2list 0

;;

let str2list str =
  str2list_f str (function x -> Char.code x)
;;

let () = let t_list = [((2, [2; 3; 6], 10), [4; 5; 8]);
                       ((0, str2list "hello", 255), str2list "hello");
                       ((2, str2list "ABC", 255), str2list "CDE");
                       ((-1, str2list "xyz", 255), str2list "wxy")]
        in
        run_test template_1L1_L "Encrypt Cesar Test" encrypt_cesar t_list
;;

let () = let t_list = [((2, [4; 5; 8], 10), [2; 3; 6]);
                       ((0, str2list "hello", 255), str2list "hello");
                       ((2, str2list "CDE", 255), str2list "ABC");
                       ((-1, str2list "wxy", 255), str2list "xyz")]
        in
        run_test template_1L1_L "Decrypt Cesar Test" decrypt_cesar t_list
;;

let p = 9967 and q = 9973
let ((_, e), (n, d)) = generate_keys_rsa p q

let phin = (p-1) * (q-1)
let is_inverse x y n = modulo ((modulo x n) * (modulo y n)) n = 1
let () = let t_list = [(e, d, phin), true]
         in
         run_test template_3_b "Generate RSA Keys Test" is_inverse t_list
;;

let () = let t_list = [((281237, (99400891, 36199003)), 70133953)]
         in
         run_test template_12_1 "Encrypt RSA Test" encrypt_rsa t_list
;;

let () = let t_list = [((70133953, (99400891, 30869683)), 281237)]
         in
         run_test template_12_1 "Decrypt RSA Test" decrypt_rsa t_list
;;

(* Test for ElGamal *)

let (g, p) = public_data_g 100000007 ;;
let (pub, priv) = generate_keys_g (g, p) ;;
let (g_k, xA_k) = encrypt_g 42 (g, p) pub ;;

let () = let t_list = [((g_k, xA_k), priv, (g, p)), 42];
        in
        run_test template_212_1 "Decrypt ElGamal Keys" decrypt_g t_list
;;
