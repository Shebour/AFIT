(** Test suites for builtin cifers ml file using oUnit. *)

open OUnit2
open Scalable
open Scalable_basic_arithmetics
open Scalable_power
open Scalable_ciphers
open Test_scalable_templates


let p = from_int 9967 and q = from_int 9973
let ((_, e), (n, d)) = generate_keys_rsa p q

let phin = mult_b (diff_b p [1;1]) (diff_b q [1;1])
let is_inverse x y n = mod_b (mult_b (mod_b x n) (mod_b y n)) n = [1; 1]
let () = let t_list = [(e, d, phin), true]
         in
         run_test template_3_b "Generate RSA Keys Test" is_inverse t_list
;;

let () = let t_list = [((from_int 281237, (from_int 99400891, from_int 36199003)),
                        from_int 70133953)
                      ]
         in
         run_test template_12_1 "Encrypt RSA Test" encrypt_rsa t_list
;;

let () = let t_list = [((from_int 70133953, (from_int 99400891, from_int 30869683)),
                        from_int 281237)
                      ]
         in
         run_test template_12_1 "Decrypt RSA Test" decrypt_rsa t_list
;;

(* Test for ElGamal *)

let (g, p) = public_data_g (from_int 100000007) ;;
let (pub, priv) = generate_keys_g (g, p) ;;
let (g_k, xA_k) = encrypt_g (from_int 42) (g, p) pub ;;

let () = let t_list = [((g_k, xA_k), priv, (g, p)), from_int 42];
        in
        run_test template_212_1 "Decrypt ElGamal Keys" decrypt_g t_list
;;
