(** Ciphers
    Built-in integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power
open Test_primes

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 255.
 *)
let encrypt_cesar k m b = if m = [] then failwith "empty list"
                          else let rec encrypt_rec k m b = match m with
                                 |[]   -> []
                                 |e::m -> (e+k) mod b::encrypt_rec k m b
                                        in encrypt_rec k m b;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 255.
 *)
let decrypt_cesar k m b = if m = [] then failwith "empty list"
                          else let rec decrypt_rec k m b = match m with
                                 |[]   -> []
                                 |e::m -> (e-k) mod b::decrypt_rec k m b
                                        in decrypt_rec k m b;;

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q = let p = abs(q) and q = abs(q)
                            in if is_prime p = false || is_prime q = false || p = q
                               then failwith "non prime or equal"
                               else let n = p * q
                                    in let z = (p - 1) * (q - 1)
                                       in let e = let rec prime i =
                                                    if gcd z i != 1
                                                    then prime (i - 1)
                                                    else i
                                                  in prime (z - 1)
                                          in let (d,_,_) = bezout e z
                                                         in ((n, e), (n, d));;

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g having high enough order modulo p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p = let g = Random.int(p)
                          in match g with
                             |g when mod_power g 2 p != 1 -> (g,p)
                             |_ -> public_data_g p;;


(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = let rec generate_keys_g_rec (g, p) =
                               let a = Random.int(p)
                               in match a with
                                  |a when mod_power a 2 p != 1 -> (mod_power g a p, a)
                                  |_ -> generate_keys_g_rec (g, p)
                             in generate_keys_g_rec (g, p);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = let k = 1 + (max_int / 2)
                                    in (mod_power g k p, modulo(msg*(mod_power kA k p)) p);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = let d = mod_power msgA a p
                                      in let (x, y, z) = bezout d p
                                         in modulo (x*msgB) p;;
