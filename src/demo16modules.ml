open Printf

module FixV(Sym: sig type 'a i end) =
struct
  type fn = { call: 'a. 'a Sym.i -> 'a }
  (* ∀t.((∀α.α t → α) → (∀α.α t → α)) → (∀α.α t → α) *)
  let fixv f = let rec g = { call = fun x -> (f g).call x } in g
end

type 'a s = SS of 'a
and t = int s
and u = float s

(* should be generated *)
module Index (S: sig type 'a result end) =
struct
  type 'a i =
    | S : ('a S.result -> 'a s S.result) i
    | T : t S.result i
    | U : u S.result i
end

module Show =
struct
  module I = Index(struct type 'a result = 'a -> string end)
  include FixV(I)

  let show0_s {call} fa (SS a) = fa a
  let show0_t {call}           = call I.S (sprintf "%d")
  let show0_u {call}           = call I.S (sprintf "%f")

  let show = fixv @@ fun f ->
   { call = fun (type a) (sym : a I.i) : a -> match sym with
     | I.S -> show0_s f
     | I.T -> show0_t f
     | I.U -> show0_u f }

  let show_s x = show.call I.S x
  let show_t x = show.call I.T x
  let show_u x = show.call I.U x
end

let () =
  let s,t,u = Show.(show_s, show_t, show_u) in
  let () = printf "%s\n%!" @@ s (sprintf "%S") (SS "asdf") in
  let () = printf "%s\n%!" @@ s (sprintf "%b") (SS true) in
  let () = printf "%s\n%!" @@ t (SS 42) in
  let () = printf "%s\n%!" @@ u (SS 3.1415) in
  ()

module M = struct
  module I = Index(struct type 'a result = 'a -> 'a end)
  include FixV(I)

  let map0_s {call} fa (SS a) = SS (fa a)
  let map0_t {call}           = call I.S (fun (x:int) -> x)
  let map0_u {call}           = call I.S (fun (x:float) -> x)

  let map = fixv @@ fun f ->
   { call = fun (type a) (sym : a I.i) : a -> match sym with
     | I.S -> map0_s f
     | I.T -> map0_t f
     | I.U -> map0_u f }

  let map_s x = map.call I.S x
  let map_t x = map.call I.T x
  let map_u x = map.call I.U x
end


(* should be generated *)
module Index2 (S: sig type ('a,'b) result end) =
struct
  type 'a i =
    | S : (('a,'b) S.result -> ('a s,'b s) S.result) i
    | T : (t,t) S.result i
    | U : (u,u) S.result i
end

module GMap = struct
  module I = Index2(struct type ('a,'b) result = 'a -> 'b end)
  include FixV(I)

  let map0_s {call} fa (SS a) = SS (fa a)
  let map0_t {call}           = call I.S (fun (x:int) -> x)
  let map0_u {call}           = call I.S (fun (x:float) -> x)

  let map = fixv @@ fun f ->
   { call = fun (type a) (sym : a I.i) : a -> match sym with
     | I.S -> map0_s f
     | I.T -> map0_t f
     | I.U -> map0_u f }

  let map_s x = map.call I.S x
  let map_t x = map.call I.T x
  let map_u x = map.call I.U x
end

module EQ = struct
  module I = Index(struct type 'a result = 'a -> 'a -> bool end)
  include FixV(I)

  let eq0_s {call} fa (SS a) (SS b)= (fa a b)
  let eq0_t {call}           = call I.S (fun (x:int) (y:int)-> x=y)
  let eq0_u {call}           = call I.S (fun (x:float) (y:float) -> x=y)

  let eq = fixv @@ fun f ->
   { call = fun (type a) (sym : a I.i) : a -> match sym with
     | I.S -> eq0_s f
     | I.T -> eq0_t f
     | I.U -> eq0_u f }

  let eq_s x = eq.call I.S x
  let eq_t x = eq.call I.T x
  let eq_u x = eq.call I.U x
end

let () =
  let eq_int: int -> int -> bool = (=) in
  let s,t,u = EQ.(eq_s, eq_t, eq_u) in
  assert (s eq_int (SS 5) (SS 5));
  assert (t        (SS 5) (SS 5));
  assert (u        (SS 5.) (SS 5.));
  assert (not (u   (SS 5.) (SS 6.)))

module Fmt = struct
  module I = Index(struct type 'a result = Format.formatter -> 'a -> unit end)
  include FixV(I)

  let fmt0_s {call} fa fmt (SS a) = Format.fprintf fmt "SS"
  let fmt0_t {call}           = call I.S (fun fmt (y:int)-> Format.fprintf fmt "%d" y)
  let fmt0_u {call}           = call I.S (fun fmt (y:float) -> Format.fprintf fmt "%f" y)

  let fmt = fixv @@ fun f ->
   { call = fun (type a) (sym : a I.i) : a -> match sym with
     | I.S -> fmt0_s f
     | I.T -> fmt0_t f
     | I.U -> fmt0_u f }

  let fmt_s x = fmt.call I.S x
  let fmt_t x = fmt.call I.T x
  let fmt_u x = fmt.call I.U x
end

let () =
  let s,t,u = Show.(show_s, show_t, show_u) in
  let () = Format.fprintf Format.std_formatter "%s\n%!" @@ s (sprintf "%S") (SS "asdf") in
  let () = Format.fprintf Format.std_formatter "%s\n%!" @@ s (sprintf "%b") (SS true) in
  let () = Format.fprintf Format.std_formatter "%s\n%!" @@ t (SS 42) in
  let () = Format.fprintf Format.std_formatter "%s\n%!" @@ u (SS 3.1415) in
  ()


(* Now let's try multiparameter data types *)
type ('a,'b) x = XX of 'a * 'b
and 'b y       = (int,'b) x
and z          = (float,int) x

module Show2 =
struct
  module I = Index(struct type 'a result = 'a -> string end)
  include FixV(I)

  let show0_s {call} fa (SS a) = fa a
  let show0_t {call}           = call I.S (sprintf "%d")
  let show0_u {call}           = call I.S (sprintf "%f")

  let show = fixv @@ fun f ->
   { call = fun (type a) (sym : a I.i) : a -> match sym with
     | I.S -> show0_s f
     | I.T -> show0_t f
     | I.U -> show0_u f }

  let show_s x = show.call I.S x
  let show_t x = show.call I.T x
  let show_u x = show.call I.U x
end
