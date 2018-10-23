(* HK5 *)
open Utils
open Printf

let __ = Obj.magic ()

type 'l a = A of b     | C | E of 'l a
and     b = I of int a | J | K of b

let rec gcata_a tr inh = function
  | A a -> tr#c_A inh a
  | C   -> tr#c_C inh
  | E a -> tr#c_E inh a
let rec gcata_b tr inh = function
  | I b -> tr#c_I inh b
  | J   -> tr#c_J inh
  | K b -> tr#c_K inh b

class virtual [ 'il, 'l, 'sl, 'inh, 'self, 'syn ] a_t = object
  method virtual c_A   : 'inh -> b    -> 'syn
  method virtual c_C   : 'inh         -> 'syn
  method virtual c_E   : 'inh -> 'l a -> 'syn
  method virtual c_D   : 'inh ->   'l -> 'syn
end
class virtual [ 'inh, 'self, 'syn ] b_t = object
  method virtual c_I   : 'inh -> int a -> 'syn
  method virtual c_J   : 'inh          -> 'syn
  method virtual c_K   : 'inh ->     b -> 'syn
end

(* Final transformation function here *)
type ('a, 'sa, 's) a_trf  = { a_trf: ('a -> 'sa) -> 'a a -> 's }
type           's  b_trf  = { b_trf: b -> 's }

class ['l, 'self] show_a_stub for_a for_b ~fself fl = object
  inherit [unit, 'l, string, unit, 'self, string] a_t
  method c_A () be = sprintf "A (%s)" (for_b.b_trf be)
  method c_C ()    = "C"
  method c_E () a  = sprintf "E (%a)" (fun () -> fself) a
  method c_D () l  = sprintf "D (%a)" (fun () -> fl) l
end
class ['self] show_b_stub for_a for_b ~fself = object
  inherit [unit, 'self, string] b_t
  method c_I () a  =
    (* if type `a` is not used in b than fl doesn't matter
       if it is used we know the type of argument
       if it is used with two different types than it is regularity restriction
    *)
    let fl n = string_of_int n in
    sprintf "I (%s)" (for_a.a_trf fl a)
  method c_J ()    = "J"
  method c_K () b  = sprintf "K (%a)" (fun () -> fself) b
end

type ('aa, 'saa, 'sa, 'sb, 'a, 'selfa) typ_for_a =
  { a_func :
               ('aa, 'saa, 'sa) a_trf ->
               'sb b_trf ->
               fself:('a a -> 'sa) ->
      ('a -> 'sa) ->
      (unit, 'aa, 'saa, unit, 'selfa, 'sa) a_t
  }
type ('aa, 'saa, 'sa, 'sb, 'a, 'self) typ_for_b =
  { b_func :
               ('aa, 'saa, 'sa) a_trf ->
               'sb b_trf ->
               fself:(b -> 'sb) ->
      (unit, 'self, 'sb) b_t
  }

let (a0,b0) =
  ( {a_func = new show_a_stub}
  , {b_func = new show_b_stub}
  )

let myfix (a0,b0) =
  let rec show_a = { a_trf = fun fl a ->
      gcata_a (a0.a_func show_a show_b (show_a.a_trf fl) fl) () a
    }
  and     show_b = { b_trf = fun    b ->
      gcata_b (b0.b_func show_a show_b show_b.b_trf  ) () b }
  in
  (show_a, show_b)

let (fix_result_1, fix_result_2) = myfix (a0,b0)

(* TODO: too monomorphic *)
let show_a fa a = fix_result_1.a_trf fa a
let show_b    b = fix_result_2.b_trf    b

let _ =
  let show_int = string_of_int in
  printf "Stage 1\n";
  printf "%s\n" @@ show_a show_int (E C);
  printf "%s\n" @@ show_a show_int (A (I C));
  printf "%s\n" @@ show_b          (I (A J));
  printf "%s\n" @@ show_b          (K J);
  ()


(* Reimplementing some stuff *)

