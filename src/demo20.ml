open Utils
open Printf

type    'a a = A of 'a b | C | B of 'a
and     'a b = I of 'a a | J

let rec gcata_a tr inh = function
  | A a -> tr#c_A inh a
  | B x -> tr#c_B inh x
  | C   -> tr#c_C inh

let rec gcata_b tr inh = function
  | I b -> tr#c_I inh b
  | J   -> tr#c_J inh

class virtual [ 'ia, 'a, 'sa, 'inh, 'self, 'syn ] a_t = object
  method virtual c_A   : 'inh -> 'a b -> 'syn
  method virtual c_B   : 'inh -> 'a   -> 'syn
  method virtual c_C   : 'inh         -> 'syn
end
class virtual [ 'ia, 'a, 'sa, 'inh, 'self, 'syn ] b_t = object
  method virtual c_I   : 'inh ->   'a a -> 'syn
  method virtual c_J   : 'inh           -> 'syn
end

module Attempt1 = struct

type ('r, 'sr, 's) a_trf  = { a_trf:  ('r -> 'sr) -> 'r a -> 's }
type ('r, 'sr, 's) b_trf  = { b_trf:  ('r -> 'sr) -> 'r b -> 's }

class ['a, 'self] show_a_stub for_a for_b ~fself fa = object
  inherit [unit, 'a, string, unit, 'self, string] a_t
  method c_A () be = sprintf "A (%s)" (for_b.b_trf fa be)
  method c_B () x  = sprintf "B (%s)" (fa x)
  method c_C ()    = "C"
end
class ['a, 'self] show_b_stub for_a for_b ~fself fa = object
  inherit [ unit, 'a, string, unit, 'self, string] b_t
  method c_I () a  = sprintf "I (%s)" (for_a.a_trf fa a)
  method c_J ()    = "J"
end

type ('inh, 'r, 'sr, 's) typ_for_a =
  { a_func : 'selfa .
               ('r,'sr, 's) a_trf ->
               ('r,'sr, 's) b_trf ->
               fself:('r a -> 's) ->
               ('r -> 'sr) ->
      ('inh, 'r, 'sr, 'inh, 'selfa, 's) a_t
  }
type ('inh, 'r, 'sr, 's) typ_for_b =
  { b_func : 'self .
               ('r,'sr, 's) a_trf ->
               ('r,'sr, 's) b_trf ->
               fself:('r b -> 's) ->
               ('r -> 'sr) ->
      ('inh, 'r, 'sr, 'inh, 'self, 's) b_t
  }

let (showa0, showb0) =
  ( {a_func = new show_a_stub}
  , {b_func = new show_b_stub}
  )

(* ERROR: less general then *)
let myfix (a0,b0) =
  let rec trait_a = { a_trf = fun f a ->
      gcata_a (a0.a_func trait_a trait_b (trait_a.a_trf f) f) () a
    }
  and     trait_b = { b_trf = fun f  b ->
      gcata_b (b0.b_func trait_a trait_b (trait_b.b_trf f) f) () b }
  in
  (trait_a, trait_b)

let a_res,b_res = myfix (showa0,showb0)

let show_a = (fun fa s -> a_res.a_trf fa s)
let show_b = (fun fa s -> b_res.b_trf fa s)


(* let (_:int) = show_a *)
end

module Attempt2 = struct

end

module Attempt3 = struct

end
