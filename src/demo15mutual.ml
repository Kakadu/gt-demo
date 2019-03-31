open Printf




type 'a s = SS of 'a
and  t = int s
and u = float s

let show0_s fa _s _t _u (SS a) = fa a
let show0_t     s _t _u = s (sprintf "%d") s _t _u
let show0_u     s _t _u = s (sprintf "%f") s _t _u


let fix (s0,t0,u0) =
  let rec s: 'a . ('a -> 's) -> 'a s -> 's = fun fa -> s0 fa s t u
  and t  = fun what -> t0 s t u what
  and u what = u0 s t u what
  in
  (s,t,u)

let (show_s,show_t,show_u) = fix (show0_s, show0_t, show0_u)
