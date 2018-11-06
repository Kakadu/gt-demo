(* HK5 *)
open Utils
open Printf

let __ = Obj.magic ()

type 'l a = A of b     | C | E of 'l a | F of 'l
and     b = I of int a | J | K of b

type a_trf = { a_trf : 'a 'sa 'syn . ('a -> 'sa) -> 'a a -> 'syn }
type b_trf = { b_trf :        'syn .                   b -> 'syn }


type xxx =
(* (b -> string) -> ('a -> string) -> 'a a -> string *)
let rec show_a show_b fa = function
  | A b -> sprintf "A %s" (show_b.b_trf b)
  | C   -> "C"
  | E x -> sprintf "E %s" (show_a show_b fa x)
  | F l -> sprintf "F %s" (fa l)
(* ((int -> string) -> int a -> string) -> b -> string *)
let rec show_b : a_trf -> b -> string =
  fun show_a -> function
  | I aa -> sprintf "I %s" (show_a.a_trf (sprintf "%d") aa)
  | J    -> "J"
  | K b  -> sprintf "K %s" (show_b show_a b)

(* (b -> b) -> ('a -> 'b) -> 'a a -> 'b a *)
let rec gmap_a gmap_b fa = function
  | A b -> A (gmap_b b)
  | C   -> C
  | E x -> E (gmap_a gmap_b fa x)
  | F l -> F (fa l)

(* (('a -> 'a) -> int a -> int a) -> b -> b *)
let rec gmap_b gmap_a = function
  | I aa -> I (gmap_a id aa)
  | J    -> J
  | K b  -> K (gmap_b gmap_a b)
