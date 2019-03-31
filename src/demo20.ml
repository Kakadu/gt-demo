open Utils
open Printf

type    'a a = A of b     | C | B of 'a
and     b    = I of int a | J

let rec gcata_a tr inh = function
  | A a -> tr#c_A inh a
  | B x -> tr#c_B inh x
  | C   -> tr#c_C inh

let rec gcata_b tr inh = function
  | I b -> tr#c_I inh b
  | J   -> tr#c_J inh

class virtual [ 'ia, 'a, 'sa, 'inh, 'self, 'syn ] a_t = object
  method virtual c_A   : 'inh -> b    -> 'syn
  method virtual c_B   : 'inh -> 'a   -> 'syn
  method virtual c_C   : 'inh         -> 'syn
end
class virtual [ 'inh, 'self, 'syn ] b_t = object
  method virtual c_I   : 'inh ->   int a -> 'syn
  method virtual c_J   : 'inh            -> 'syn
end

module Attempt1 = struct

(* if 'sr is a type parameter we will  restrict it too much because of b *)
(* type ('s, 'sr) a_trf  = { a_trf: 'r     . ('r -> 'sr) -> 'r a -> 's } *)
type    a_trf  = { a_trf: 'r 'sr 's . ('r -> 'sr) -> 'r a -> 's }
type    b_trf  = { b_trf:        's .                   b -> 's }
(* 's can't be a parameter because class show for a will instantiate this parameter
   to be always a string *)

class ['a, 'self] show_a_stub for_a for_b ~fself fa = object
  inherit [unit, 'a, string, unit, 'self, string] a_t
  method c_A () be = sprintf "A (%s)" (for_b.b_trf be)
  method c_B () x  = sprintf "B (%s)" (fa x)
  method c_C ()    = "C"
end
class ['self] show_b_stub for_a for_b ~fself = object
  inherit [unit, 'self, string] b_t
  method c_I () a  = sprintf "I (%s)" (for_a.a_trf (sprintf "%d") a)
  method c_J ()    = "J"
end

type ('inh, 'sa, 'sb, 's, 'sr) typ_for_a =
  { a_func : 'selfa 'r .
               a_trf -> b_trf -> fself:('r a -> 's) ->
               ('r -> 'sr) ->
      ('inh, 'r, 'sa, 'inh, 'selfa, 's) a_t
  }
type ('sa, 'sr, 'sb, 'inh, 's) typ_for_b =
  { b_func : 'self .
                a_trf -> b_trf -> fself:(b -> 's) ->
      (('inh, 'self, 's) b_t)
  }

let (a0,b0) =
  ( {a_func = new show_a_stub}
  , {b_func = new show_b_stub}
  )

(* ERROR: less general then *)
(* let myfix (a0,b0) =
 *   let rec trait_a = { a_trf = fun f a ->
 *       gcata_a (a0.a_func trait_a trait_b (trait_a.a_trf f) f) () a
 *     }
 *   and     trait_b = { b_trf = fun    b ->
 *       gcata_b (b0.b_func trait_a trait_b  trait_b.b_trf  ) () b }
 *   in
 *   (trait_a, trait_b) *)


end

module Attempt2 = struct

end

module Attempt3 = struct

end
