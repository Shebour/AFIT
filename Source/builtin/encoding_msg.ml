(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)

let encode str bits =
  let rec encode_rec str bits i j  =
    match i with
    |(-1) -> 0
    |_ -> Char.code str.[i] * (power (power 2 bits) j) + encode_rec str bits (i - 1) (j + 1)
  in encode_rec str bits ((String.length str) - 1) 0;;
 

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)

let decode msg bits =
  let rec decode_rec msg bits i string =
    match i with
    |i when i = bits - 1  -> string
    |_ -> decode_rec ((msg - (msg mod (power (power 2 bits) (i+1))) / (power (power 2 bits) i))) bits (i+1) ((Char.escaped (Char.chr (msg mod (power (power 2 bits) (i+1)) / (power (power 2 bits) i))))^string)
  in decode_rec msg bits 0 "";;

