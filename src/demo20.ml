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

type ('ir, 'r, 'sr, 'i, 's) a_trf  = ('ir -> 'r -> 'sr) -> 'i -> 'r a -> 's
type ('ir, 'r, 'sr, 'i, 's) b_trf  = ('ir -> 'r -> 'sr) -> 'i -> 'r b -> 's

let fixab a0 b0 =
  Printf.printf "myfix\n";
  let rec trait_a f i a = gcata_a (a0 trait_a trait_b (trait_a f) f) i a
  and     trait_b f i b = gcata_b (b0 trait_a trait_b (trait_b f) f) i b
  in
  (trait_a, trait_b)

module ShowAB = struct

  class ['a, 'self] show_a_stub for_a for_b fself fa = object
    inherit [unit, 'a, string, unit, 'self, string] a_t
    method c_A () be = sprintf "A (%s)" (for_b fa () be)
    method c_B () x  = sprintf "B (%s)" (fa () x)
    method c_C ()    = "C"
  end
  class ['a, 'self] show_b_stub for_a for_b fself fa = object
    inherit [ unit, 'a, string, unit, 'self, string] b_t
    method c_I () a  = sprintf "I (%s)" (for_a fa () a)
    method c_J ()    = "J"
  end

  let showa0 a b c d = Printf.printf "new!\n"; new show_a_stub a b c d
  let showb0 a b c d = Printf.printf "new!\n"; new show_b_stub a b c d

  let show_a fa s =
    (fst @@ fixab showa0 showb0) fa s

  let show_b fa s =
    (snd @@ fixab showa0 showb0) fa s

  let _ = Printf.printf "%s\n" (show_a (lift string_of_int) () (A (I (B 4))))

end

module GmapAB = struct

  class ['a, 'b, 'self] gmap_a_stub for_a for_b fself fa = object
    inherit [unit, 'a, 'b, unit, 'self, 'b a] a_t
    method c_A () be = A (for_b fa () be)
    method c_C ()    = C
    method c_B () x  = B (fa () x)
  end
  class ['a, 'b, 'self] gmap_b_stub for_a for_b fself fa = object
    inherit [ unit, 'a, 'b, unit, 'self, 'b b] b_t
    method c_I () a  = I (for_a fa () a)
    method c_J ()    = J
  end

  let trait_a0 a b c d = new gmap_a_stub a b c d
  let trait_b0 a b c d = new gmap_b_stub a b c d

  let gmap_a fa () s =
    (fst @@ fixab trait_a0 trait_b0) fa () s

  let gmap_b fa s =
    (snd @@ fixab trait_a0 trait_b0) fa () s

  let _ =
    Printf.printf "%s\n" @@ ShowAB.show_a (lift string_of_int) () @@
    gmap_a (lift ((+)1)) () (A (I (B 4)));

end

module StatefulAB = struct
  class ['a, 'b, 'env, 'self] stateful_a_stub for_a for_b fself fa = object
    inherit ['env, 'a, 'env*'b, 'env, 'self, 'env * 'b a] a_t
    method c_A env be =
      let (e2,next) = for_b fa env be in
      (e2, A next)
    method c_C env    = (env,C)
    method c_B env x  = let (e2,y) = fa env x in (e2, B y)
  end
  class ['a, 'b, 'env, 'self] stateful_b_stub for_a for_b fself fa = object
    inherit ['env, 'a, 'env*'b, 'env, 'self, 'env * 'b b] b_t
    method c_I env be =
      let (e2,next) = for_a fa env be in
      (e2, I next)
    method c_J env    = (env,J)
  end

  let gmap_a fa s =
    (fst @@ fixab (new stateful_a_stub) (new stateful_b_stub)) fa s

  let gmap_b fa s =
    (snd @@ fixab (new stateful_a_stub) (new stateful_b_stub)) fa s

end
