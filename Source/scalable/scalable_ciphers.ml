(** Ciphers
    bitarrays based ciphers.
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power
open Scalable_test_primes

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)
let generate_keys_rsa p q = (([],[]),([],[])) (*let p = abs_b(q) and q = abs_b(q)
                            in if is_prime p = false || is_prime q = false || p = q
                               then failwith "non prime or equal"
                               else let n = mult_b p q
                                    in let z = mult_b (diff_b p [0;1]) (diff_b q [0;1])
                                       in let e = let rec prime i =
                                                    if gcd_b z i != [0;1]
                                                    then prime (diff_b i [0;1])
                                                    else i
                                                  in prime (diff_b z [0;1])
                                          in let (d,_,_) = bezout_b e z
                                                         in ((n, e), (n, d));;*)
(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let  public_data_g p = let g = from_int(Random.int(to_int(p))) in let rec public_data_g_rec p =
                                                            match g with
                                                              |g when mod_power g [0;0;1] p != [0;1] -> (g,p)
                                                              |_ -> public_data_g_rec  p
                                                          in public_data_g_rec  p;;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = let rec generate_keys_g_rec (g, p) =
                               let a = from_int(Random.int(to_int(p)))
                               in match a with
                                  |a when mod_power a [0;0;1] p != [0;1] -> (mod_power g a p, a)
                                  |_ -> generate_keys_g_rec (g, p)
                             in generate_keys_g_rec (g, p);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = let k = add_b [0;1] (from_int(max_int / 2))
                                    in (mod_power g k p, mod_b (mult_b msg (mod_power kA k p)) p);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = let d = mod_power msgA a p
                                      in let (x, y, z) = bezout_b d p
                                         in mod_b (mult_b x msgB) p;;
