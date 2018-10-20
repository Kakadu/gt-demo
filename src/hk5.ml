(* Tying to make one big FIX: attempt 4
 * Now we don't tolerate any non-regualrity
 *)
open Utils
open Printf

let __ = Obj.magic ()
let (!!) = Obj.magic

type    'a a = A of 'a b  | C | B of 'a
and     'a b = I of 'a a  | J | K of 'a

let gcata_a tr inh = function
  | A a -> tr#c_A inh a
  | B x -> tr#c_B inh x
  | C   -> tr#c_C inh

let gcata_b tr inh = function
  | I b -> tr#c_I inh b
  | J   -> tr#c_J inh
  | K x -> tr#c_K inh x

class virtual [ 'ia, 'a, 'sa, 'inh, 'self, 'syn ] a_t = object
  method virtual c_A   : 'inh -> 'a b -> 'syn
  method virtual c_C   : 'inh         -> 'syn
  method virtual c_B   : 'inh -> 'a   -> 'syn
end
class virtual [ 'ia, 'a, 'sa, 'inh, 'self, 'syn ] b_t = object
  method virtual c_I   : 'inh ->   'a a -> 'syn
  method virtual c_J   : 'inh            -> 'syn
  method virtual c_K   : 'inh ->   'a    -> 'syn
end

(* Final transformation function here *)
(* if 'sr is a type parameter we will  restrict it too much because of b
   so we add universal polymorphism
*)

type  ('s, 'sa, 'r)       a_trf  = { a_trf:       ('r -> 'sa) -> 'r a -> 's }
type  ('s, 'sa, 'r)       b_trf  = { b_trf:       ('r -> 'sa) -> 'r b -> 's }

class ['a, 'self] show_a_stub for_a for_b ~fself fa = object
  inherit [unit, 'a, string, unit, 'self, string] a_t
  method c_A () be = sprintf "A (%s)" (for_b.b_trf fa be)
  method c_B () x  = sprintf "B (%s)" (fa x)
  method c_C ()    = "C"
end
class ['a, 'self] show_b_stub for_a for_b ~fself (fa: 'a -> string) = object
  inherit [unit, 'a, string, unit, 'self, string] b_t
  method c_I () a  = sprintf "I (%s)" (for_a.a_trf fa a)
  method c_J ()    = "J"
  method c_K () x  = sprintf "K (%s)" (fa x)
end

type ('r, 'sr, 'sa1, 'sa2, 'sa, 'sb, 'r2, 'inha, 'inh) t2_a =
  { a_func : 'self .
      ('sa, 'sa1, 'r) a_trf -> ('sb, 'sa2, 'r2) b_trf ->
      ('r -> 'sr) ->
      ('inha, 'r, 'sr, 'inh, 'self, 'sa) a_t
  }
type ('r, 'sr, 'sa1, 'sa2, 'sa, 'sb, 'r2, 'inha, 'inh) t2_b =
  { b_func :  'self .
      ('sa, 'sa1, 'r) a_trf -> ('sb, 'sa2, 'r2) b_trf ->
      ('r -> 'sr) ->
      ('inha, 'r, 'sr, 'inh, 'self, 'sb) b_t
  }

let a0 =
  {a_func = (fun tra trb fa ->  (new show_a_stub tra trb ~fself:(fun _ -> assert false) fa)

      ) }

let b0 =
  {b_func = (fun tra trb fa ->
          (new show_b_stub tra trb ~fself:(fun _ -> assert false) fa)
      ) }

let myfix (a0,b0) =
  let rec show_a = { a_trf = fun fl subj ->
      gcata_a (a0.a_func show_a show_b fl) () subj
    }
  and     show_b = { b_trf = fun fl subj ->
      gcata_b (b0.b_func show_a show_b fl) () subj
    }
  in
  (show_a, show_b)

let (fix_result_1, fix_result_2) = myfix (a0,b0)

let show_a    a = fix_result_1.a_trf    a
let show_b    b = fix_result_2.b_trf    b

(* let _ =
 *   printf "Stage 1\n";
 *   (\* printf "%s\n" @@ show_a    (E C); *\)
 *   printf "%s\n" @@ show_a show_int (A (I C));
 *   printf "%s\n" @@ show_b show_int (I (A J));
 *   () *)


(* (\* Reimplementing some stuff *\)
 *
 * class ['a, 'self] show_a_stub2 for_a for_b ~fself fa = object
 *   inherit ['a, 'self] show_a_stub for_a for_b ~fself fa
 *   method! c_A () be = sprintf "A {%a}" (fun () -> for_b.b_trf fa) be
 * end
 *
 * let a0 =
 *   {a_func = (fun tra trb fa subj ->
 *        fix (fun fself ->
 *            (gcata_a (new show_a_stub2 tra trb ~fself fa) )
 *          )
 *          ()
 *          subj
 *       ) }
 *
 * let (fix_result_1, fix_result_2) = myfix (a0,b0)
 *
 * let show_a    s = fix_result_1.a_trf    s
 * let show_b    s = fix_result_2.b_trf    s
 *
 * let _ =
 *   printf "Stage 2 (a reimplemented):\n";
 *   let show_int n = string_of_int n in
 *   printf "%s\n" @@ show_a show_int C;
 *   printf "%s\n" @@ show_a show_int (A (I C));
 *   printf "%s\n" @@ show_b show_int (I (A J));
 *   printf "%s\n" @@ show_b show_int J;
 *   ()
 *
 * (\** gmap *********************************************** *\)
 *
 * class ['r, 'r2, 'self] gmap_a_stub for_a for_b ~fself fa = object
 *   inherit [unit, 'r, 'r2, unit, 'self, 'r2 a] a_t
 *   method c_A () x  = A (for_b.b_trf fa x)
 *   method c_B () x  = B (fa x)
 *   method c_C ()    = C
 * end
 * class ['r, 'r2, 'self] gmap_b_stub for_a for_b ~fself fa = object
 *   inherit [unit, 'r, 'r2, unit, 'self, 'r2 b] b_t
 *   method c_I () a  = I (for_a.a_trf fa a)
 *   method c_J ()    = J
 *   method c_K () x  = K (fa x)
 * end
 *
 * let (gmap_a0, gmap_b0) =
 *   ( {a_func = (fun tra trb fa subj ->
 *        fix (fun fself ->
 *            (gcata_a (new gmap_a_stub tra trb ~fself fa) )
 *          )
 *          ()
 *          subj
 *       ) }
 *   , {b_func = (fun tra trb fa subj ->
 *           (gcata_b (new gmap_b_stub tra trb ~fself:(fun _ -> assert false) fa)
 *              ()
 *              subj)
 *     ) }
 *   )
 *
 * let (fix_result_1, fix_result_2) = myfix (gmap_a0, gmap_b0)
 *
 * (\* extra hacks to skip inherited attribute *\)
 * let gmap_a fa s = fix_result_1.a_trf fa  s
 * let gmap_b    s = fix_result_2.b_trf     s
 *
 * let _ =
 *   let t1 = B "5" in
 *   let t2 = A (I C) in
 *   printf "Stage 2\n";
 *   printf "%s\n" @@ show_a show_string @@ t1;
 *   printf "%s\n" @@ show_a show_int @@ t2;
 *   printf "%s\n" @@ show_a show_string @@ gmap_a id t1;
 *   () *)
