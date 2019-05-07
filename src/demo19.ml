open Utils

module FixV(Sym: sig type 'a i end) =
struct
  type fn = { call: 'a. 'a Sym.i -> 'a }
  (* ∀t.((∀α.α t → α) → (∀α.α t → α)) → (∀α.α t → α) *)
  let fixv f =
    let rec g = lazy { call = fun x -> (f (Lazy.force g)).call x } in
    Lazy.force g
end

type a = [ `A of b | `C of int ]
and b = [ `B of a | `D of string ]

class virtual ['inh, 'extra, 'syn] a_t =
  object
    method virtual c_A : 'inh -> a -> b -> 'syn
    method virtual c_C : 'inh -> a -> int -> 'syn
  end
class virtual ['inh, 'extra, 'syn] b_t =
  object
    method virtual c_B : 'inh -> b -> a -> 'syn
    method virtual c_D : 'inh -> b -> string -> 'syn
  end

let gcata_a tr inh subj =
  match subj with
    `A ___001_ -> tr#c_A inh subj ___001_
  | `C ___002_ -> tr#c_C inh subj ___002_
let gcata_b tr inh subj =
  match subj with
    `B ___003_ -> tr#c_B inh subj ___003_
  | `D ___004_ -> tr#c_D inh subj ___004_

module type IndexResult_a =
  sig
    type 'a result
    type 'dummy0 i = A : a result i | B : b result i
  end
module Index_a (S : sig type 'a result end) =
  struct
    type 'a result = 'a S.result
    type 'dummy0 i = A : a result i | B : b result i
  end

let show_int = Printf.sprintf "%d"
let show_string = Printf.sprintf "%S"

module Ishow_a = Index_a (struct type 'a result = unit -> 'a -> string end)
module Fix_show_a = FixV (Ishow_a)

class ['extra_a] show_a_t fix fself_a =
  object
    inherit [unit, 'extra_a, string] a_t
    method c_A inh___005_ _ _x__006_ =
      Printf.sprintf "`A (%s)" (fix.Fix_show_a.call Ishow_a.B () _x__006_)
    method c_C inh___007_ _ _x__008_ =
      Printf.sprintf "`C (%s)"
        ((fun () subj -> show_int subj) () _x__008_)
  end
class ['extra_b] show_b_t ({Fix_show_a.call = call} as _mutuals_pack) fself_b =
  object
    inherit [unit, 'extra_b, string] b_t
    method c_B inh___009_ _ _x__010_ =
      Printf.sprintf "`B (%s)" (call Ishow_a.A () _x__010_)
    method c_D inh___011_ _ _x__012_ =
      Printf.sprintf "`D (%s)"
        ((fun () subj -> show_string subj) () _x__012_)
  end
let show_a_0 call inh0 subj =
   fix (fun fself -> gcata_a (new show_a_t call fself)) inh0 subj
let show_b_0 call inh0 subj =
  fix (fun fself -> gcata_b (new show_b_t call fself)) inh0 subj

let show_a_fix =
  Fix_show_a.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Ishow_a.i) ->
           (match sym with
              Ishow_a.A -> show_a_0 f
            | Ishow_a.B -> show_b_0 f :
            a)})

let show_a_fix = show_a_fix
let show_b_fix = show_a_fix

let a =  {gcata = gcata_a;}
let b =  {gcata = gcata_b;}

(* ************************************************************************** *)
type c = [ `E of int | b ]
class virtual ['inh, 'extra, 'syn] c_t =
  object
    method virtual c_E : 'inh -> c -> int  -> 'syn
    inherit ['inh, 'extra, 'syn] b_t
  end

let gcata_c tr inh subj =
  match subj with
    `E ___013_ -> tr#c_E inh subj ___013_
  | (#b as subj) -> gcata_b tr inh subj
module type IndexResult_c =
  sig
    type 'a result
    type 'dummy0 i = C : c result i
  end
module Index_c (S : sig type 'a result end) =
  struct
    type 'a result = 'a S.result
    type 'dummy0 i = C : c result i
  end

module Ishow_c = Index_c (struct type 'a result = unit -> 'a -> string end)
module Fix_show_c = FixV (Ishow_c)

class ['extra_c] show_c_t {Fix_show_c.call}  fself_c =
  object
    inherit [unit, 'extra_c, string] c_t
    method c_E inh___014_ _ _x__015_ =
      Printf.sprintf "`E (%d)" _x__015_
    inherit
      ['extra_c] show_b_t show_b_fix
        (fun () subj -> match subj with (#b as subj) -> fself_c () subj)
  end

let show_c_0 call inh0 subj =
  fix (fun fself -> gcata_c (new show_c_t call fself)) inh0 subj

let show_c_fix =
  Fix_show_c.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Ishow_c.i) ->
           (match sym with Ishow_c.C -> show_c_0 f : a)})

let c =  {gcata = gcata_c;}

let x = `A (`B (`C 3))
let y = `B (`A (`D "3"))
let z = `E 1

let () =
  Printf.printf "%s\n" (show_a_0 show_a_fix () x);
  Printf.printf "%s\n" (show_b_0 show_a_fix () y);
  Printf.printf "%s\n" (show_c_0 show_c_fix () z);
  Printf.printf "%s\n" (show_c_0 show_c_fix () y)


(* class show_c' fixC fixAB fself =
 *   object (this)
 *     inherit [_] show_b_t fixAB fself as superB
 *     inherit [_] show_c_t fixC fself as super
 *     method! c_E () s y = "new " ^ super#c_E () s y
 *     method! c_B () s y = "new " ^ super#c_B () s y
 *     method! c_D () s y = "new " ^ super#c_D () s y
 *   end *)


(* let show_c_0 call inh0 subj =
 *   fix (fun fself -> gcata_c (new show_c' call fself)) inh0 subj
 *
 * let show_c_fix2 =
 *   Fix_show_c.fixv
 *     (fun f ->
 *        {call =
 *          fun (type a) (sym : a Ishow_c.i) ->
 *            (match sym with
 *               Ishow_c.C ->
 *                 transform_gcata_c ((new show_c') f show_b_fix) :
 *             a)}) *)


(* let show_b_as_c call inh0 subj =
 *   fix (fun fself -> gcata_b (new show_c_t call fself)) inh0 subj
 *
 * let show_ab_fix2 =
 *   Fix_show_a.fixv
 *     (fun f ->
 *        {call =
 *          (fun (type w) (sym : w Ishow_a.i) : w->
 *             match sym with
 *               Ishow_a.A -> show_a_0 f
 *             | Ishow_a.B -> show_b_as_c f
 *          )})
 *
 * let () =
 *   Printf.printf "%s\n"
 *     (transform c ((new show_c') show_c_fix2 show_b_fix) () z);
 *   Printf.printf "%s\n"
 *     (transform c ((new show_c') show_c_fix2 show_b_fix) () y) *)
