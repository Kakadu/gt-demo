open Printf

module FixV(Sym: sig type 'a i end) =
struct
  type fn = { call: 'a. 'a Sym.i -> 'a }
  (* ∀t.((∀α.α t → α) → (∀α.α t → α)) → (∀α.α t → α) *)
  let fixv f =
    let rec g = lazy { call = fun x -> (f (Lazy.force g)).call x } in
    Lazy.force g
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

(* let () =
 *   let s,t,u = Show.(show_s, show_t, show_u) in
 *   let () = printf "%s\n%!" @@ s (sprintf "%S") (SS "asdf") in
 *   let () = printf "%s\n%!" @@ s (sprintf "%b") (SS true) in
 *   let () = printf "%s\n%!" @@ t (SS 42) in
 *   let () = printf "%s\n%!" @@ u (SS 3.1415) in
 *   () *)

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

(* let () =
 *   let eq_int: int -> int -> bool = (=) in
 *   let s,t,u = EQ.(eq_s, eq_t, eq_u) in
 *   assert (s eq_int (SS 5) (SS 5));
 *   assert (t        (SS 5) (SS 5));
 *   assert (u        (SS 5.) (SS 5.));
 *   assert (not (u   (SS 5.) (SS 6.))) *)

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

(* let () =
 *   let s,t,u = Show.(show_s, show_t, show_u) in
 *   let () = Format.fprintf Format.std_formatter "%s\n%!" @@ s (sprintf "%S") (SS "asdf") in
 *   let () = Format.fprintf Format.std_formatter "%s\n%!" @@ s (sprintf "%b") (SS true) in
 *   let () = Format.fprintf Format.std_formatter "%s\n%!" @@ t (SS 42) in
 *   let () = Format.fprintf Format.std_formatter "%s\n%!" @@ u (SS 3.1415) in
 *   () *)


(* Now let's try multiparameter data types *)
type ('a,'b) x = XX of 'a * 'b
and y          = (int,float) x
and z          = (float,int) x

module FixV2(Sym: sig type ('a,'b) i end) =
struct
  type fn = { call: 'a 'b . ('a,'b) Sym.i -> 'a }

  let fixv f = let rec g = { call = fun x -> (f g).call x } in g
end

(* should be geerated *)
module IndexXYZ (S: sig type 'a result end) =
struct
  type ('a,'b) i =
    | X : ('a S.result -> 'b S.result -> ('a,'b) x S.result,
           'a S.result -> 'b S.result -> ('a,'b) x S.result) i
          (* ('a S.result -> 'a x S.result, 'b S.result -> 'b x S.result) i *)
    | Y : (y S.result,y S.result) i
    | Z : (z S.result,z S.result) i
end

module Show2 =
struct
  module I = IndexXYZ(struct type 'a result = 'a -> string end)
  include FixV2(I)

  let show0_x {call} fa fb  (XX (a,b)) = sprintf "XX(%s,%s)" (fa a) (fb b)
  let show0_y {call}           = call I.X (sprintf "%d") (sprintf "%f")
  let show0_z {call}           = call I.X (sprintf "%f") (sprintf "%d")

  let show = fixv @@ fun f ->
   { call = fun (type a) (type b) (sym : (a,b) I.i) : a -> match sym with
     | I.X -> show0_x f
     | I.Y -> show0_y f
     | I.Z -> show0_z f }

  let show_x w = show.call I.X w
  let show_y w = show.call I.Y w
  let show_z w = show.call I.Z w
end

(* ******************************************************************* *)
(* regular polymorphc variants *)
type 'a wtf = [ `A of 'a | `B of 'a qwe ]
and  'a qwe = [ `C of 'a wtf | `D of int ]

let gcata_wtf tr inh : _ wtf -> _ = function
    `A a -> tr#c_A inh a
  | `B s -> tr#c_B inh s
let gcata_qwe tr inh : _ qwe -> _ = function
  | `C a -> tr#c_C inh a
  | `D s -> tr#c_D inh s

class virtual ['ia,'a,'sa, 'inh, 'self, 'syn] wtf_t = object
  method virtual c_A : 'inh -> 'a -> 'syn
  method virtual c_B : 'inh -> 'a qwe -> 'syn
end
class virtual ['ia,'a,'sa, 'inh, 'self, 'syn] qwe_t = object
  method virtual c_C : 'inh -> 'a wtf -> 'syn
  method virtual c_D : 'inh -> int -> 'syn
end

module Show3 =
struct
  (* should be generated *)
  module Index (S: sig type 'a result end) =
  struct
    type ('a,'b) i =
      | QWE : ('a S.result -> 'a qwe S.result, 'a S.result -> 'a wtf S.result) i
      | WTF : ('a S.result -> 'a wtf S.result, 'a S.result -> 'a qwe S.result) i
  end

  module I = Index(struct type 'a result = 'a -> string end)
  include FixV2(I)

  (* We pass FixV2.fn here to avoid erros like
     `this has type 'a blah but expected 'a . 'a blah`
  *)
  class ['a, 'self] show_wtf_t {call} fa fself = object
    inherit [unit,'a,string, unit, 'self, string] wtf_t
    method c_A () a = fa a
    method c_B () (x: _ qwe) = call I.QWE fa x
  end
  class ['a,'self] show_qwe_t {call} fa fself = object
    inherit [unit,'a,string, unit, 'self, string] qwe_t
    method c_C () a = call I.WTF fa a
    method c_D () x = sprintf "%d" x
  end

  let show0_qwe {call} fa (s: _ qwe) =
    let rec obj = lazy (new show_qwe_t {call} fa fself)
    and fself s =  gcata_qwe (Lazy.force obj) () s
    in
    fself s

  let show0_wtf {call} fa (s: _ wtf) =
    let rec obj = lazy (new show_wtf_t {call} fa fself)
    and fself s =  gcata_wtf (Lazy.force obj) () s
    in
    fself s


  let show = fixv @@ fun f ->
   { call = fun (type a) (type b) (sym : (a,b) I.i) : a -> match sym with
     | I.QWE -> show0_qwe f
     | I.WTF -> show0_wtf f
   }

  let show_wtf w = show.call I.WTF w
  let show_qwe w = show.call I.QWE w

end

(* ******************************************************************* *)
(* non-regular mutual algebraic data types *)
type 'a ii = A of 'a     | B of 'a jj
and  'a jj = C of 'a ii  | D of int | E of int ii | F of 'a jj

let gcata_i tr inh : _ ii -> _ = function
  | A a -> tr#c_A inh a
  | B s -> tr#c_B inh s
let gcata_j tr inh : _ jj -> _ = function
  | C i -> tr#c_C inh i
  | D n -> tr#c_D inh n
  | E i -> tr#c_E inh i
  | F j -> tr#c_F inh j

class virtual ['ia,'a,'sa, 'inh, 'self, 'syn] i_t = object
  method virtual c_A : 'inh -> 'a -> 'syn
  method virtual c_B : 'inh -> 'a jj -> 'syn
end
class virtual ['ia,'a,'sa, 'inh, 'self, 'syn] j_t = object
  method virtual c_C : 'inh -> 'a ii -> 'syn
  method virtual c_D : 'inh -> int -> 'syn
  method virtual c_E : 'inh -> int ii -> 'syn
  method virtual c_F : 'inh -> 'a jj -> 'syn
end

module Show4 =
struct
  (* should be generated *)
  module Index (S: sig type 'a result end) =
  struct
    type 'a i =
      | I : ('a S.result -> 'a ii S.result) i
      | J : ('a S.result -> 'a jj S.result) i
  end

  module I = Index(struct type 'a result = 'a -> string end)
  include FixV(I)

  (* We pass FixV2.fn here to avoid erros like
     `this has type 'a blah but expected 'a . 'a blah`
  *)
  class ['a, 'self] show_i_t {call} fa fself = object
    inherit [unit,'a,string, unit, 'self, string] i_t
    method c_A () a = fa a
    method c_B () (x: _) = call I.J fa x
  end
  class ['a,'self] show_j_t {call} fa fself = object
    inherit [unit,'a,string, unit, 'self, string] j_t
    method c_C () i = call I.I fa i
    method c_D () x = sprintf "%d" x
    method c_E () x = call I.I (sprintf "%d") x
    method c_F () jj =
      fself jj
      (* The below will start to generate more objects than needed *)
      (* call I.J fa jj *)
  end

  let show0_i {call} fa (s: _ ii) =
    let rec obj = lazy (let () = printf "new II\n" in new show_i_t {call} fa fself)
    and fself s =  gcata_i (Lazy.force obj) () s
    in
    fself s

  let show0_j {call} fa (s: _ jj) =
    let rec obj = lazy (let () = printf "new JJ\n" in new show_j_t {call} fa fself)
    and fself s =  gcata_j (Lazy.force obj) () s
    in
    fself s


  let show = fixv @@ fun f ->
    { call =
        let c = fun (type a) (sym : a I.i) : a -> match sym with
          | I.I -> show0_i f
          | I.J -> show0_j f
        in c
   }

  let show_ii w = show.call I.I w
  let show_jj w = show.call I.J w

  (* let () =
   *   (\* print_endline @@ show_ii (sprintf "%d") ( B (F (F (C (B (F (F(D 18)))))))); *\)
   *   print_endline @@ show_jj (sprintf "%d") (F (F (F (F (F (F (F(D 18)))))))); *)


end

(* ******************************************************************* *)

module XXX = struct

type 'a zz = A of 'a     | B of int zz | C of float zz


let gcata_zz tr inh : _ zz -> _ = function
  | A s -> tr#c_A inh s
  | B s -> tr#c_B inh s
  | C s -> tr#c_C inh s

class virtual ['ia,'a,'sa, 'inh, 'self, 'syn] zz_t = object
  method virtual c_A : 'inh -> 'a -> 'syn
  method virtual c_B : 'inh -> int zz -> 'syn
  method virtual c_C : 'inh -> float zz -> 'syn
end

module Show4 =
struct
  (* should be generated *)
  module Index (S: sig type 'a result end) =
  struct
    type 'a i =
      | ZZ : ('a S.result -> 'a zz S.result) i
  end

  module I = Index(struct type 'a result = 'a -> string end)
  include FixV(I)

  (* We pass FixV2.fn here to avoid erros like
     `this has type 'a blah but expected 'a . 'a blah`
  *)
  class ['a, 'self] show_zz_t {call} fa fself =
    object
    inherit [unit,'a,string, unit, 'self, string] zz_t
    method c_A () a = fa a
    method c_B () (x: _) = call I.ZZ (sprintf "%d") x
    method c_C () (x: _) = call I.ZZ (sprintf "%f") x
  end

  let show0_zz {call} fa (s: _ zz) =
    let rec obj = lazy (let () = printf "new ZZ\n" in new show_zz_t {call} fa fself)
    and fself s =  gcata_zz (Lazy.force obj) () s
    in
    fself s


  let show =
    let foo = fun f ->
      { call =
          fun (type a) (sym : a I.i) : a -> match sym with
            | I.ZZ -> show0_zz f

      }
    in
    fixv foo

  let show_zz w = show.call I.ZZ w

  let () =
    print_endline @@ show_zz (sprintf "%f") (C (C (C (C (A 1.8)))));

end

end
