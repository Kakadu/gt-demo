open Utils
open Printf

(* type    'a a = A of 'a b | C | B of 'a
 * and     'a b = I of 'a a | J
 *
 * let rec gcata_a tr inh = function
 *   | A a -> tr#c_A inh a
 *   | B x -> tr#c_B inh x
 *   | C   -> tr#c_C inh
 *
 * let rec gcata_b tr inh = function
 *   | I b -> tr#c_I inh b
 *   | J   -> tr#c_J inh
 *
 * class virtual [ 'ia, 'a, 'sa, 'inh, 'self, 'syn ] a_t = object
 *   method virtual c_A   : 'inh -> 'a b -> 'syn
 *   method virtual c_B   : 'inh -> 'a   -> 'syn
 *   method virtual c_C   : 'inh         -> 'syn
 * end
 * class virtual [ 'ia, 'a, 'sa, 'inh, 'self, 'syn ] b_t = object
 *   method virtual c_I   : 'inh ->   'a a -> 'syn
 *   method virtual c_J   : 'inh           -> 'syn
 * end
 *
 * type ('ir, 'r, 'sr, 'i, 's) a_trf  = ('ir -> 'r -> 'sr) -> 'i -> 'r a -> 's
 * type ('ir, 'r, 'sr, 'i, 's) b_trf  = ('ir -> 'r -> 'sr) -> 'i -> 'r b -> 's
 *
 * let fixab a0 b0 =
 *   Printf.printf "myfix\n";
 *   let rec trait_a f i a = gcata_a (a0 trait_a trait_b (trait_a f) f) i a
 *   and     trait_b f i b = gcata_b (b0 trait_a trait_b (trait_b f) f) i b
 *   in
 *   (trait_a, trait_b)
 *
 * module ShowAB = struct
 *
 *   class ['a, 'self] show_a_stub for_a for_b fself fa = object
 *     inherit [unit, 'a, string, unit, 'self, string] a_t
 *     method c_A () be = sprintf "A (%s)" (for_b fa () be)
 *     method c_B () x  = sprintf "B (%s)" (fa () x)
 *     method c_C ()    = "C"
 *   end
 *   class ['a, 'self] show_b_stub for_a for_b fself fa = object
 *     inherit [ unit, 'a, string, unit, 'self, string] b_t
 *     method c_I () a  = sprintf "I (%s)" (for_a fa () a)
 *     method c_J ()    = "J"
 *   end
 *
 *   let showa0 a b c d = Printf.printf "new!\n"; new show_a_stub a b c d
 *   let showb0 a b c d = Printf.printf "new!\n"; new show_b_stub a b c d
 *
 *   let show_a fa s =
 *     (fst @@ fixab showa0 showb0) fa s
 *
 *   let show_b fa s =
 *     (snd @@ fixab showa0 showb0) fa s
 *
 *   let _ = Printf.printf "%s\n" (show_a (lift string_of_int) () (A (I (B 4))))
 *
 * end
 *
 * module GmapAB = struct
 *
 *   class ['a, 'b, 'self] gmap_a_stub for_a for_b fself fa = object
 *     inherit [unit, 'a, 'b, unit, 'self, 'b a] a_t
 *     method c_A () be = A (for_b fa () be)
 *     method c_C ()    = C
 *     method c_B () x  = B (fa () x)
 *   end
 *   class ['a, 'b, 'self] gmap_b_stub for_a for_b fself fa = object
 *     inherit [ unit, 'a, 'b, unit, 'self, 'b b] b_t
 *     method c_I () a  = I (for_a fa () a)
 *     method c_J ()    = J
 *   end
 *
 *   let trait_a0 a b c d = new gmap_a_stub a b c d
 *   let trait_b0 a b c d = new gmap_b_stub a b c d
 *
 *   let gmap_a fa () s =
 *     (fst @@ fixab trait_a0 trait_b0) fa () s
 *
 *   let gmap_b fa s =
 *     (snd @@ fixab trait_a0 trait_b0) fa () s
 *
 *   let _ =
 *     Printf.printf "%s\n" @@ ShowAB.show_a (lift string_of_int) () @@
 *     gmap_a (lift ((+)1)) () (A (I (B 4)));
 *
 * end
 *
 * module StatefulAB = struct
 *   class ['a, 'b, 'env, 'self] stateful_a_stub for_a for_b fself fa = object
 *     inherit ['env, 'a, 'env*'b, 'env, 'self, 'env * 'b a] a_t
 *     method c_A env be =
 *       let (e2,next) = for_b fa env be in
 *       (e2, A next)
 *     method c_C env    = (env,C)
 *     method c_B env x  = let (e2,y) = fa env x in (e2, B y)
 *   end
 *   class ['a, 'b, 'env, 'self] stateful_b_stub for_a for_b fself fa = object
 *     inherit ['env, 'a, 'env*'b, 'env, 'self, 'env * 'b b] b_t
 *     method c_I env be =
 *       let (e2,next) = for_a fa env be in
 *       (e2, I next)
 *     method c_J env    = (env,J)
 *   end
 *
 *   let stateful__a fa s =
 *     (fst @@ fixab (new stateful_a_stub) (new stateful_b_stub)) fa s
 *
 *   let stateful__b fa s =
 *     (snd @@ fixab (new stateful_a_stub) (new stateful_b_stub)) fa s
 *
 * end *)

(* ******************************************************************************* *)
module PV = struct

type a = [`A of b | `B of int]
and  b = [`C of a | `D of string]

let rec gcata_a tr inh subj =
  match subj with
  | `A b -> tr#c_A inh subj b
  | `B x -> tr#c_B inh subj x

let rec gcata_b tr inh subj =
  match subj with
  | `C a -> tr#c_C inh subj a
  | `D s -> tr#c_D inh subj s

class virtual [  'inh, 'self, 'syn ] a_t = object
  method virtual c_A   : 'inh -> 'self ->   b -> 'syn
  method virtual c_B   : 'inh -> 'self -> int -> 'syn
end
class virtual [ 'inh, 'self, 'syn ] b_t = object
  method virtual c_C   : 'inh -> 'self ->   a -> 'syn
  method virtual c_D   : 'inh -> 'self -> string -> 'syn
end

type ( 'i, 's) a_trf  =  'i -> a -> 's
type ( 'i, 's) b_trf  =  'i -> b -> 's

let fixab a0 b0 =
  Printf.printf "myfix ab\n";
  let rec trait_a i a = gcata_a (a0 (trait_a, trait_b) trait_a) i a
  and     trait_b i b = gcata_b (b0 (trait_a, trait_b) trait_b) i b
  in
  (trait_a, trait_b)


module Show = struct

  class ['self] show_a_stub (for_a,for_b) fself = object
    inherit [unit, 'self, string] a_t
    constraint 'self = [> a ] as 'self
    method c_A () _ be = sprintf "A (%s)" (for_b () be)
    method c_B () _ d  = sprintf "B (%d)" d
  end
  class ['self] show_b_stub (for_a,for_b) fself = object
    inherit [ unit, 'self, string] b_t
    constraint 'self = [> b ] as 'self
    method c_C () (_: 'self) a  = sprintf "C (%s)" (for_a () a)
    method c_D () (_: 'self) s  = sprintf "D %s" s
  end

  let showa0 a b = Printf.printf "new!\n"; new show_a_stub a b
  let showb0 a b = Printf.printf "new!\n"; new show_b_stub a b

  let fixab ?(a0=showa0) ?(b0=showb0) () = fixab a0 b0
  let show_a fa s =
    (fst @@ fixab ()) fa s

  let show_b fa s =
    (snd @@ fixab ()) fa s

  let _ = Printf.printf "%s\n" (show_a () (`A (`C (`A (`D "4")))))

end

module Show2 = struct
  (* class ['a, 'self] show_a_stub for_a for_b fself = object
   *   inherit [unit, 'self, string] a_t
   *   method c_A () be = sprintf "A (%s)" (for_b () be)
   *   method c_B () d  = sprintf "B (%d)" d
   * end *)
  class ['self] show_b_stub2 (for_a,for_b) fself = object
    inherit ['self] Show.show_b_stub (for_a,for_b) fself
    method c_C () _ a  = sprintf "new C (%s)" (for_a () a)
    method c_D () _ s  = sprintf "new D %s" s
  end

  let showa0 a b = Printf.printf "new!\n"; new Show.show_a_stub a b
  let showb0 a b = Printf.printf "new!\n"; new show_b_stub2 a b

  let show_a fa s =
    (fst @@ fixab showa0 showb0) fa s

  let show_b fa s =
    (snd @@ fixab showa0 showb0) fa s

  let _ = Printf.printf "%s\n" (show_a () (`A (`C (`A (`D "4")))))

end

end

module PV2 = struct
  type c = [ PV.b | `E of int ]
  let gcata_c tr inh subj = match subj with
    | `E n -> tr#c_E inh subj n
    | #PV.b as b -> PV.gcata_b tr inh b

  class virtual [ 'inh, 'self, 'syn ] c_t = object
    inherit [ 'inh, 'self, 'syn ] PV.b_t
    method virtual c_E   : 'inh -> c -> int -> 'syn
  end

  (* let fixc c0 =
   *   Printf.printf "myfix c\n";
   *   let rec trait_c i a = gcata_c (c0 trait_c trait_c) i a
   *   in
   *   trait_c *)

  (* let (_:int) = fixab PV.Show.showa0 *)

  module Show = struct
    class ['self] show_c_stub make_clas fself =
      let show_a2,show_b2  =
        PV.fixab
          PV.Show.showa0
          (fun b c -> ((make_clas c ()) :> _ PV.Show.show_b_stub) )
      in
      object
        inherit [unit, 'self, string] c_t
        (* constraint 'self = [> c ]  as 'self *)
        inherit [ 'self] PV.Show.show_b_stub (show_a2,show_b2)
            (fun inh -> function (#PV.b as x) -> fself inh x)
        method! c_C () _ a = sprintf "new `C (%s)" (show_a2 () a)
        method! c_D () _ s = sprintf "new `D %s" s
        method  c_E () (_:c) s = sprintf "`E %d" s
      end

    let rec showc0 fself () = Printf.printf "new c0!\n"; new show_c_stub showc0 fself

    (* let fixc ?(c0=showc0) () = fixc (fun a b -> c0 a b ()) *)

    let (_: (unit -> PV.b -> string) -> c -> string) = fun self -> gcata_c (showc0 self ()) ()

    let show_c () s =
      let rec trait () s = gcata_c (showc0 trait ()) () s
      in
      trait () s


    let _ = Printf.printf "%s\n" (show_c () (`C (`A (`D "4"))))
  end
end
