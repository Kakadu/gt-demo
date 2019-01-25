open Printf

type 'a s = SS of 'a
and t = int s
and u = float s

(* ATTEMPT 1*)
(* let show0_s fa _s _t _u (SS a) = fa a
 * let show0_t s _t _u = s (sprintf "%d") s _t _u
 * let show0_u s _t _u = s (sprintf "%f") s _t _u
 *
 * let fix1 (s0,t0,u0) =
 *   let rec s: ('a -> 's) -> 'a s -> 's = fun fa -> s0 fa s t u
 *   and t = fun what -> t0 s t u what
 *   and u what = u0 s t u what
 *   in
 *   (s,t,u)
 *
 * let (show_s,show_t,show_u) = fix1 (show0_s, show0_t, show0_u)
 *     (\* Irregularity strikes *\) *)

(* ATTEMPT 2
  we imlement show using existentional quantification
*)

type  init2s =
  { s_init2: 'a . ('a -> string) ->
       init2s ->  init2t ->  init2u ->
      'a s -> string }
and  init2t =
  { t_init2:
       init2s ->  init2t ->  init2u ->
      t -> string }
and  init2u =
  { u_init2:
       init2s ->  init2t ->  init2u ->
      u -> string }

let show2_s = { s_init2 = (fun fa _s _t _u (SS a) -> fa a) }
let show2_t = { t_init2 = (fun     s t u -> s.s_init2 (sprintf "%d") s t u) }
let show2_u = { u_init2 = (fun     s t u -> s.s_init2 (sprintf "%f") s t u) }

type 's wrap1s = { s_trf1: 'a . ('a -> 's) -> 'a s -> 's }
type 's wrap1t = { t_trf1:                       t -> 's }
type 's wrap1u = { u_trf1:                       u -> 's }


let fix2 (s0,t0,u0) =
  let rec s = { s_trf1 = fun fa subj -> s0.s_init2 fa s0 t0 u0 subj }
  and t = { t_trf1 = fun subj -> t0.t_init2 s0 t0 u0 subj }
  and u = { u_trf1 = fun subj -> u0.u_init2 s0 t0 u0 subj }
  in
  (s,t,u)

let () =
  let s,t,u = fix2 (show2_s,show2_t,show2_u) in
  let () = printf "%s\n%!" @@ s.s_trf1 (sprintf "%S") (SS "asdf") in
  let () = printf "%s\n%!" @@ s.s_trf1 (sprintf "%b") (SS true) in
  let () = printf "%s\n%!" @@ t.t_trf1 (SS 42) in
  let () = printf "%s\n%!" @@ u.u_trf1 (SS 3.1415) in
  ()


(* ATTEMPT 3
  we want polymorphism over synthesized attribute ('syn)
   It can't be existentionally quantified because we want it's types to be defined
   by inital transformation function (string for showingm, and 'b t for functors)
*)
open Higher
type ('syns,'syna,'synt,'synu) init3s =
  { s_init3: 'a . ('a -> ('a,'syna) app) ->
      ('syns,'syna,'synt,'synu) init3s ->
      ('syns,'syna,'synt,'synu) init3t ->
      ('syns,'syna,'synt,'synu) init3u ->
      'a s -> ('a,'syns) app }
and  ('syns,'syna,'synt,'synu) init3t =
  { t_init3: 'syn .
      ('syns,'syna,'synt,'synu) init3s ->
      ('syns,'syna,'synt,'synu) init3t ->
      ('syns,'syna,'synt,'synu) init3u ->
      t -> 'syn }
and  ('syns,'syna,'synt,'synu) init3u =
  { u_init3: 'syn .
      ('syns,'syna,'synt,'synu) init3s ->
      ('syns,'syna,'synt,'synu) init3t ->
      ('syns,'syna,'synt,'synu) init3u ->
      u -> 'synu }

let show3_s = { s_init3 = (fun fa _s _t _u (SS a) -> fa a) }
let show3_t = { t_init3 = (fun     s t u ->
    let module HS = Higher.Newtype1(struct type 'a t = 'a s end) in
    let module HA = Higher.Newtype1(struct type 'a t = string end) in
    (* HA.t esacpes its scope *)
    M.inj @@ s.s_init3 (fun n -> HA.inj @@ sprintf "%d" n ) s t u)
  }
let show3_u = { u_init3 = (fun     s t u -> s.s_init3 (sprintf "%f") s t u) }
