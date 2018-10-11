(* Tying to make one big FIX: attempt 3 *)
open Utils
open Printf

let __ = Obj.magic ()
let (!!) = Obj.magic

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

(* Final transformation function here *)
(* if 'sr is a type parameter we will  restrict it too much because of b
   so we add universal polymorphism
*)

type  's       a_trf  = { a_trf: 'r 'sa   . ('r -> 'sa) -> 'r a -> 's }
type  's       b_trf  = { b_trf:                              b -> 's }

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

(* So we will add universal polymorphism for 'saparam there too to work around this
   But it is wrong because a class for transformation always know concrete 'saparam
   and we can't hide it behind a quantifier
 *)

(* type ('iota, 'pi) typ_for_a =
 *   { a_func : 'inh 'syn  'r 'sr 'sa 'sb .
 *                ('syn -> 'pi) ->
 *       'sa a_trf -> 'sb b_trf ->
 *       ('r -> 'sr) ->
 *       'r a ->
 *       'pi
 *   }
 * type ('iota, 'pi) typ_for_b =
 *   { b_func : 'r 'inh 'syn 'sa 'sb .
 *                (\* ('iota -> 'inh) -> *\) ('syn -> 'pi) ->
 *                'sa a_trf -> 'sb b_trf ->
 *       b ->
 *       'syn
 *   } *)

type 'pi t2_a =
  { a_func : 'inh 'syn  'r 'sr 'sa 'sb .
               ('syn -> 'pi) ->
      'sa a_trf -> 'sb b_trf ->
      ('r -> 'sr) ->
      'r a ->
      'pi
  }
type 'pi t2_b =
  { b_func : 'r 'inh 'syn 'sa 'sb .
               ('syn -> 'pi) ->
      'sa a_trf -> 'sb b_trf ->
      b ->
      'syn
  }

let b0 =
  {b_func = (fun prj tra trb subj ->
        !!prj @@
          (gcata_b (new show_b_stub !!tra trb  ~fself:(fun _ -> assert false))
             ()
             subj)
      ) }

let a0 =
  {a_func = (fun prj tra trb fa subj ->
       !!prj @@
       fix (fun fself ->
           (gcata_a (new show_a_stub tra !!trb ~fself !!fa) )
         )
         ()
         subj
      ) }


let myfix (a0,b0) =
  let rec show_a = { a_trf = fun fl subj ->
      a0.a_func id show_a show_b fl subj
    }
  and     show_b = { b_trf = fun subj ->
      b0.b_func id show_a show_b subj }
  in
  (show_a, show_b)

let (fix_result_1, fix_result_2) = myfix (a0,b0)

let show_a    a = fix_result_1.a_trf    a
let show_b    b = fix_result_2.b_trf    b

let _ =
  let show_int = string_of_int in
  printf "Stage 1\n";
  (* printf "%s\n" @@ show_a    (E C); *)
  printf "%s\n" @@ show_a show_int (A (I C));
  printf "%s\n" @@ show_b          (I (A J));
  ()

(* let (fix_result_1, fix_result_2) = myfix (a0,b0)
 *
 * let show_a    a = fix_result_1.a_trf    a
 * let show_b    b = fix_result_2.b_trf    b
 *
 * let _ =
 *   let show_int = string_of_int in
 *   printf "Stage 1\n";
 *   (\* printf "%s\n" @@ show_a    (E C); *\)
 *   printf "%s\n" @@ show_a show_int (A (I C));
 *   printf "%s\n" @@ show_b          (I (A J));
 *   ()
 *
 * (\* Reimplementing some stuff *\)
 *
 * (\* class ['a, 'self] show_a_stub2 for_a for_b ~fself fa = object
 *  *   inherit ['a, 'self] show_a_stub for_a for_b ~fself fa
 *  *   method! c_A () be = sprintf "A {%a}" (fun () -> for_b.b_trf) be
 *  * end
 *  *
 *  * let (a0,b0) =
 *  *   ( {a_func = new show_a_stub2}
 *  *   , {b_func = new show_b_stub}
 *  *   )
 *  *
 *  * let (fix_result_1, fix_result_2) = myfix (a0,b0)
 *  *
 *  * let show_a    s = fix_result_1.a_trf    s
 *  * let show_b    s = fix_result_2.b_trf    s
 *  *
 *  * let _ =
 *  *   printf "Stage 2 (a reimplemented):\n";
 *  *   let show_int n = string_of_int n in
 *  *   printf "%s\n" @@ show_a  show_int C;
 *  *   printf "%s\n" @@ show_a  show_int (A (I C));
 *  *   printf "%s\n" @@ show_b (I (A J));
 *  *   printf "%s\n" @@ show_b (J);
 *  *   () *\)
 *
 * (\** gmap *********************************************** *\)
 *
 * class ['r, 'r2, 'self] gmap_a_stub for_a for_b ~fself fa = object
 *   inherit [unit, 'r, 'r2, unit, 'self, 'r2 a] a_t
 *   method c_A () be = A (for_b.b_trf be)
 *   method c_B () x  = B (fa x)
 *   method c_C ()    = C
 * end
 * class ['self] gmap_b_stub for_a for_b ~fself = object
 *   inherit [unit, 'self, b] b_t
 *   method c_I () a  = I (for_a.a_trf id a)
 *   method c_J ()    = J
 * end
 *
 * let (gmap_a0,gmap_b0) =
 *   ( { a_func = new gmap_a_stub}
 *   , { b_func = new gmap_b_stub}
 *   )
 *
 * let (fix_result_1, fix_result_2) = myfix (gmap_a0, gmap_b0)
 *
 * (\* extra hacks to skip inherited attribute *\)
 * let gmap_a fa s = fix_result_1.a_trf fa  s
 * (\* And now gmap_a is too restricted because parameter of type a is always an int *\)
 * (\* FAIL *\)
 * let gmap_b    s = fix_result_2.b_trf     s
 *
 * (\* let _ =
 *  *   let t1 : int a = A (I C) in
 *  *   let t2 : float a = A (I C) in
 *  *   printf "Stage 2\n";
 *  *   printf "%s\n" @@ show_a id @@ t1;
 *  *   printf "%s\n" @@ show_a id @@ t2;
 *  *   printf "%s\n" @@ show_a id @@ gmap_a id t1;
 *  *   () *\) *)
