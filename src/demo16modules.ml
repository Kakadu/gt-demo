open Printf
let id x = x

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

let gcata_s tr inh = function
  | SS a -> tr#c_SS inh a
let gcata_t = gcata_s
let gcata_u = gcata_s

(* should be generated *)
module Index (S: sig type 'a result end) =
struct
  type 'a i =
    | S : ('a S.result -> 'a s S.result) i
    | T : t S.result i
    | U : u S.result i
end

(* should be generated *)
module type IndexResult2 = sig
  type (_,_) result
  type _ i =
    | S : (('a,'b) result -> ('a s,'b s) result) i
    | T : (t,t) result i
    | U : (u,u) result i
end
module Index2 (S : sig type ('a, 'b) result end) = struct
  type ('a,'b) result = ('a,'b) S.result
  type _ i =
    | S : (('a,'b) result -> ('a s,'b s) result) i
    | T : (t,t) result i
    | U : (u,u) result i
end

module Show =
struct
  module I = Index(struct type 'a result = 'a -> string end)
  module FixSgmap = FixV(I)

  class ['a] show_s (call: FixSgmap.fn) fa = object
    method c_SS () (a: 'a) = sprintf "SS (%s)" (fa a)
  end
  class ['a] show_t call = object
    method do_t () x : string = call.FixSgmap.call I.S (sprintf "%d") x
  end
  class ['a] show_u call = object
    method do_u () x = call.FixSgmap.call I.S (sprintf "%f") x
  end
  let show0_s call fa s =
    gcata_s (new show_s call fa) () s

  let show0_t {FixSgmap.call}           = call I.S (sprintf "%d")
  let show0_u {FixSgmap.call}           = call I.S (sprintf "%f")

  let show = FixSgmap.fixv @@ fun f ->
   { call = fun (type a) (sym : a I.i) : a -> match sym with
     | I.S -> show0_s f
     | I.T -> show0_t f
     | I.U -> show0_u f }

  let show_s x = show.FixSgmap.call I.S x
  let show_t x = show.FixSgmap.call I.T x
  let show_u x = show.FixSgmap.call I.U x
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


module GMap = struct
  module I : (IndexResult2 with type ('a,'b) result = 'a -> 'b) =
    Index2(struct type ('a,'b) result = 'a -> 'b end)
  module Fix_gmap = FixV(I)

  let map0_s {Fix_gmap.call} fa (SS a) = SS (fa a)
  let map0_t {Fix_gmap.call}           = call I.S (fun (x:int) -> x)
  let map0_u {Fix_gmap.call}           = call I.S (fun (x:float) -> x)

  let map = Fix_gmap.fixv @@ fun f ->
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

(* ********************************************************************************** *)
module SSSS = struct
  type 'a ssss = 'a s
  let gcata_ssss = gcata_s
  module IndexSSSS (S: sig type 'a result end) = struct
    type 'a i = SSSS : ('a S.result -> 'a ssss S.result) i
  end

  module IshowSSSS = IndexSSSS(struct type 'a result = 'a -> string end)
  module Fix_show_SSSS = FixV(IshowSSSS)

  class ['a] show_ssss call fa = object
    inherit ['a] Show.show_s Show.show fa
  end
  let show0 call fa = gcata_ssss (new show_ssss call fa) ()
  let map = Fix_show_SSSS.fixv @@ fun f ->
   { call = fun (type a) (sym : a IshowSSSS.i) : a -> match sym with
     | IshowSSSS.SSSS -> show0 f
   }

  let show_ssss x = map.Fix_show_SSSS.call IshowSSSS.SSSS x


end
(* ********************************************************************************** *)
module X = struct
(* Now let's try multiparameter data types *)
type ('a,'b) x = XX of 'a * 'b
and y          = (int,float) x
and z          = (float,int) x

(* should be generated *)
module Index1 (S: sig type 'a result end) = struct
  type _ i =
    | X : ('a S.result -> 'b S.result -> ('a,'b) x S.result) i
    | Y : (y S.result) i
    | Z : (z S.result) i
end
module type IndexResult2 = sig
  type (_,_) result
  type _ i =
    | X : (('a, 'a2) result -> ('b,'b2) result -> (('a,'b) x, ('a2,'b2) x) result) i
    | Y : (y,y) result i
    | Z : (z,z) result i
end
module Index2 (S: sig type ('a,'b) result end) = struct
  type ('a,'b) result = ('a,'b) S.result
  type _ i =
    | X : (('a, 'a2) S.result -> ('b,'b2) S.result -> (('a,'b) x, ('a2,'b2) x) S.result) i
    | Y : (y,y) S.result i
    | Z : (z,z) S.result i
end

module Show2 =
struct
  module I = Index1(struct type 'a result = 'a -> string end)
  include FixV(I)

  let show0_x {call} fa fb  (XX (a,b)) = sprintf "XX(%s,%s)" (fa a) (fb b)
  let show0_y {call}           = call I.X (sprintf "%d") (sprintf "%f")
  let show0_z {call}           = call I.X (sprintf "%f") (sprintf "%d")

  let show = fixv @@ fun f ->
   { call = fun (type a) (sym : a I.i) : a -> match sym with
     | I.X -> show0_x f
     | I.Y -> show0_y f
     | I.Z -> show0_z f }

  let show_x w = show.call I.X w
  let show_y w = show.call I.Y w
  let show_z w = show.call I.Z w
end

module GMap2 = struct
  module I : (IndexResult2 with type ('a,'b) result = 'a -> 'b) =
    Index2(struct type ('a,'b) result = 'a -> 'b end)
  module Fix_gmap = FixV(I)

  let map0_x {Fix_gmap.call} fa fb (XX (a,b)) = XX (fa a, fb b)
  let map0_y {Fix_gmap.call}                  = call I.Y
  let map0_z {Fix_gmap.call}                  = call I.Z

  let map = Fix_gmap.fixv @@ fun f ->
   { call = fun (type a) (sym : a I.i) : a -> match sym with
     | I.X -> map0_x f
     | I.Y -> map0_y f
     | I.Z -> map0_z f }

  let map_x x = map.call I.X x
  let map_y x = map.call I.Y x
  let map_z x = map.call I.Z x
end
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
    type _ i =
      | QWE : ('a S.result -> 'a qwe S.result) i
      | WTF : ('a S.result -> 'a wtf S.result) i
  end

  module I = Index(struct type 'a result = 'a -> string end)
  include FixV(I)

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
   { call = fun (type a) (sym : a I.i) : a -> match sym with
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

  let show0_zz {call} fa =
    let rec obj = lazy (let () = printf "new ZZ\n" in new show_zz_t {call} fa fself)
    and fself s =  gcata_zz (Lazy.force obj) () s
    in
    fself

  module PhysHash = Hashtbl.Make(struct
      type t = Obj.t
      let compare x y = compare (Obj.magic x) (Obj.magic y)
      let equal = (==)
      let hash = Hashtbl.hash
    end)

  let show =
    fixv @@ fun f ->
      { call = fun (type a) ->
          fun (sym : a I.i) -> (match sym with
            | I.ZZ -> show0_zz f : a)

      }

  let show_zz w = show.call I.ZZ w

  let () =
    print_endline @@ show_zz (sprintf "%f") (C (C (C (C (A 1.8)))));

end

end



module PVar1 = struct
  type a = [ `A ]

  let gcata tr inh = function `A -> tr#c_A inh ()
  class virtual ['inh, 'self, 'syn] a_t = object
    method virtual c_A : 'inh -> unit -> 'syn
  end
  module Index (S: sig type 'a result end) = struct
    type 'a i = PA : a S.result i
  end
  module Index2 (S: sig type ('a,'b) result end) = struct
    type 'a i = PA : (a,a) S.result i
  end

  module Ishow_a = Index(struct type 'a result = 'a -> string end)
  module Fix_show_a = FixV(Ishow_a)
  class ['self] show_a_t {Fix_show_a.call} fself = object
    inherit [unit, 'self, string] a_t
    method c_A () () = "`A"
  end
  let show_a_0 call = Utils.fix0 (fun fself -> gcata (new show_a_t call fself) ())
  let show_fix =
    Fix_show_a.fixv (fun f ->
        {call = fun (type w) (sym : w Ishow_a.i) : w ->
            match sym with Ishow_a.PA -> show_a_0 f })

  module Igmap_a = Index2(struct type ('a,'b) result = 'a -> 'b end)
  module Fix_gmap_a = FixV(Igmap_a)
  class ['self] gmap_a_t {Fix_gmap_a.call} fself = object
    inherit [unit, 'self, 'self] a_t
    constraint 'self = [> a ]
    method c_A () () = `A
  end
  let gmap_a_0 call = Utils.fix0 (fun fself -> gcata (new gmap_a_t call fself) ())
  let gmap_fix =
    Fix_gmap_a.fixv (fun f ->
        {call = fun (type w) (sym : w Igmap_a.i) : w ->
            match sym with Igmap_a.PA -> gmap_a_0 f })

  module Ieq_a = Index(struct type 'a result = 'a -> 'a -> bool end)
  module Fix_eq_a = FixV(Ieq_a)
  class ['self] eq_a_t {Fix_eq_a.call} fself = object
    inherit ['self, 'self, bool] a_t
    method c_A _ () = true
  end
  let eq_a_0 call = Utils.fix0 (fun fself -> gcata (new eq_a_t call fself))
  let eq_fix =
    Fix_eq_a.fixv (fun f ->
        {call = fun (type w) (sym : w Ieq_a.i) : w ->
            match sym with Ieq_a.PA -> eq_a_0 f })


end

module PVar2 = struct
  type b = [ `B ]

  let gcata tr inh = function `B -> tr#c_B inh ()
  class virtual [ 'inh, 'self, 'syn ] b_t = object
    method virtual c_B : 'inh -> unit -> 'syn
  end
  module Index (S: sig type 'a result end) = struct
    type 'a i = PB : b S.result i
  end
  module Index2 (S: sig type ('a,'b) result end) = struct
    type 'a i = PB : (b,b) S.result i
  end

  module Ishow_b = Index(struct type 'a result = 'a -> string end)
  module Fix_show_b = FixV(Ishow_b)
  class ['self] show_b_t {Fix_show_b.call} fself = object
    inherit [unit, 'self, string] b_t
    method c_B () () = "`B"
  end
  let show_b_0 call = Utils.fix0 (fun fself -> gcata (new show_b_t call fself) ())
  let show_fix =
    Fix_show_b.fixv (fun f ->
        {call = fun (type w) (sym : w Ishow_b.i) : w ->
            match sym with Ishow_b.PB -> show_b_0 f })

  module Igmap_b = Index2(struct type ('a,'b) result = 'a -> 'b end)
  module Fix_gmap_b = FixV(Igmap_b)
  class ['self] gmap_b_t {Fix_gmap_b.call} fself = object
    inherit [unit, 'self, 'self] b_t
    constraint 'self = [> b ]
    method c_B () () = `B
  end
  let gmap_b_0 call = Utils.fix0 (fun fself -> gcata (new gmap_b_t call fself) ())
  let gmap_fix =
    Fix_gmap_b.fixv (fun f ->
        {call = fun (type w) (sym : w Igmap_b.i) : w ->
            match sym with Igmap_b.PB -> gmap_b_0 f })

  module Ieq_b = Index(struct type 'a result = 'a -> 'a -> bool end)
  module Fix_eq_b = FixV(Ieq_b)
  class ['self] eq_b_t {Fix_eq_b.call} fself = object
    inherit ['self, 'self, bool] b_t
    method c_B _ () = true
  end
  let eq_b_0 call = Utils.fix0 (fun fself -> gcata (new eq_b_t call fself))
  let eq_fix =
    Fix_eq_b.fixv (fun f ->
        {call = fun (type w) (sym : w Ieq_b.i) : w ->
            match sym with Ieq_b.PB -> eq_b_0 f })


end

module PVar3 = struct
  type c = [ PVar1.a | PVar2.b ]

  let gcata tr inh = function
    | #PVar1.a as x -> PVar1.gcata tr inh x
    | #PVar2.b as x -> PVar2.gcata tr inh x
  class virtual [ 'inh, 'self, 'syn ] c_t = object
    inherit ['inh, 'self, 'syn ] PVar1.a_t
    inherit ['inh, 'self, 'syn ] PVar2.b_t
  end

  module Index (S: sig type 'a result end) = struct
    type 'a i = PC : c S.result i
  end
  module Index2(S: sig type ('a,'b) result end) = struct
    type 'a i = PC : (c,c) S.result i
  end

  module Ishow_c = Index(struct type 'a result = 'a -> string end)
  module Fix_show_c = FixV(Ishow_c)

  class ['self] show_c_t {Fix_show_c.call} fself = object
    inherit [unit, 'self, string] c_t
    inherit ['self] PVar1.show_a_t PVar1.show_fix fself
    inherit ['self] PVar2.show_b_t PVar2.show_fix fself
  end
  let show_c_0 call = Utils.fix0 (fun fself -> gcata (new show_c_t call fself) ())
  let show_fix =
    Fix_show_c.fixv (fun f ->
        {call = fun (type w) (sym : w Ishow_c.i) : w ->
            match sym with Ishow_c.PC -> show_c_0 f })


  module Igmap_c = Index2(struct type ('a,'b) result = 'a -> 'b end)
  module Fix_gmap_c = FixV(Igmap_c)
  class ['self] gmap_c_t {Fix_gmap_c.call} fself = object
    inherit [unit, 'self, 'self] c_t
    constraint 'self = [> c ]
    inherit ['self] PVar1.gmap_a_t PVar1.gmap_fix fself
    inherit ['self] PVar2.gmap_b_t PVar2.gmap_fix fself
  end
  let gmap_c_0 call = Utils.fix0 (fun fself -> gcata (new gmap_c_t call fself) ())
  let gmap_fix =
    Fix_gmap_c.fixv (fun f ->
        {call = fun (type w) (sym : w Igmap_c.i) : w ->
            match sym with Igmap_c.PC -> gmap_c_0 f })

  module Ieq_c = Index(struct type 'a result = 'a -> 'a -> bool end)
  module Fix_eq_c = FixV(Ieq_c)
  class ['self] eq_c_t {Fix_eq_c.call} fself = object
    inherit [[> c ] as 'self, 'self, bool] c_t
    inherit ['self] PVar1.eq_a_t PVar1.eq_fix
        (fun inh -> function #PVar1.a as subj -> fself inh subj)
    inherit ['self] PVar2.eq_b_t PVar2.eq_fix
        (fun inh -> function #PVar2.b as subj -> fself inh subj)

  end
  let eq_c_0 call inh = Utils.fix (fun fself -> gcata (new eq_c_t call fself)) inh
  let eq_fix =
    Fix_eq_c.fixv (fun f ->
        {call = fun (type w) (sym : w Ieq_c.i) : w ->
            match sym with Ieq_c.PC -> eq_c_0 f })

end

(* module Stateful = struct
 * module A = struct
 *   type 'a t = [`A]
 *
 *   class virtual ['ia, 'a, 'sa, 'inh, 'extra, 'syn] t_t =
 *     object
 *       method virtual c_A : 'inh -> 'a t -> 'syn
 *     end
 *
 *   let gcata_t tr inh subj = match subj with `A -> tr#c_A inh subj
 *
 *   module type IndexResult_stateful_t = sig
 *     type ('env, 'a, 'b) result
 *
 *     type 'dummy0 i =
 *       | T : (('env, 'a, 'a2) result -> ('env, 'a t, 'a2 t) result) i
 *   end
 *
 *   module Index_stateful_t (S : sig
 *     type ('env, 'a, 'b) result
 *   end) =
 *   struct
 *     type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result
 *
 *     type 'dummy0 i =
 *       | T : (('env, 'a, 'a2) result -> ('env, 'a t, 'a2 t) result) i
 *   end
 *
 *   module Istateful_t = Index_stateful_t (struct
 *     type ('env, 'a, 'b) result = 'env -> 'a -> 'env * 'b
 *   end)
 *
 *   module Fix_stateful_t = FixV (Istateful_t)
 *
 *   class ['a, 'a_2, 'env, 'extra_t] stateful_t_t _ fa fself_t =
 *     object
 *       inherit ['env, 'a, 'env * 'a_2, 'env, 'extra_t, 'env * 'extra_t] t_t
 *       (\* constraint 'extra_t = [> 'a_2 t ] *\)
 *       method c_A inh___001_ _ =
 *         (\* (inh___001_, match `A with #t as s -> s) *\)
 *         (inh___001_, `A )
 *     end
 *
 *   let stateful_t_0 call fa inh0 subj =
 *     Utils.fix (fun fself ->  gcata_t (new stateful_t_t call fa fself)) inh0 subj
 *
 *   let stateful_t_fix =
 *     Fix_stateful_t.fixv (fun f ->
 *         { call=
 *             (fun (type a) (sym : a Istateful_t.i) ->
 *               (match sym with Istateful_t.T -> stateful_t_0 f : a) ) } )
 *
 *   let t =
 *     { Utils.gcata = gcata_t
 *     }
 * end
 *
 * module B = struct
 *   type 'b t = [`B]
 *
 *   class virtual ['ib, 'b, 'sb, 'inh, 'extra, 'syn] t_t =
 *     object
 *       method virtual c_B : 'inh -> 'b t -> 'syn
 *     end
 *
 *   let gcata_t tr inh subj = match subj with `B -> tr#c_B inh subj
 *
 *
 *   module type IndexResult_stateful_t = sig
 *     type ('env, 'a, 'b) result
 *
 *     type 'dummy0 i =
 *       | T : (('env, 'a, 'a2) result -> ('env, 'a t, 'a2 t) result) i
 *   end
 *
 *   module Index_stateful_t (S : sig
 *     type ('env, 'a, 'b) result
 *   end) =
 *   struct
 *     type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result
 *
 *     type 'dummy0 i =
 *       | T : (('env, 'a, 'a2) result -> ('env, 'a t, 'a2 t) result) i
 *   end
 *
 *   module Istateful_t = Index_stateful_t (struct
 *     type ('env, 'a, 'b) result = 'env -> 'a -> 'env * 'b
 *   end)
 *
 *   module Fix_stateful_t = FixV (Istateful_t)
 *
 *   class ['b, 'b_2, 'env, 'extra_t] stateful_t_t _ fb fself_t =
 *     object
 *       inherit ['env, 'b, 'env * 'b_2, 'env, 'extra_t, 'env * 'extra_t] t_t
 *       (\* constraint 'extra_t = [> 'b_2 t ] *\)
 *       method c_B inh___002_ _ =
 *         (\* (inh___002_, match `B with #t as s -> s) *\)
 *         (inh___002_, `B)
 *     end
 *
 *   let stateful_t_0 call fb inh0 subj =
 *     Utils.fix (fun fself ->  gcata_t (new stateful_t_t call fb fself)) inh0 subj
 *
 *   let stateful_t_fix =
 *     Fix_stateful_t.fixv (fun f ->
 *         { call=
 *             (fun (type a) (sym : a Istateful_t.i) ->
 *               (match sym with Istateful_t.T -> stateful_t_0 f : a) ) } )
 *
 *   let t = { Utils.gcata = gcata_t }
 * end
 *
 * let stateful_string inh subj = (inh,subj)
 * let stateful_int    inh subj = (inh,subj)
 *
 * module Z = struct
 *   type 'x t = [ int A.t | string B.t]
 *
 *   class virtual ['ix, 'x, 'sx, 'inh, 'extra, 'syn] t_t =
 *     object
 *       inherit [ int, int, int,          'inh, 'extra, 'syn] A.t_t
 *       inherit [ string, string, string, 'inh, 'extra, 'syn] B.t_t
 *     end
 *
 *   let gcata_t tr inh subj =
 *     match subj with
 *     | #A.t as subj -> A.gcata_t tr inh subj
 *     | #B.t as subj -> B.gcata_t tr inh subj
 *
 *   module type IndexResult_stateful_t = sig
 *     type ('env, 'a, 'b) result
 *
 *     type 'dummy0 i =
 *       | T : (('env, 'a, 'a2) result -> ('env, 'a t, 'a2 t) result) i
 *   end
 *
 *   module Index_stateful_t (S : sig    type ('env, 'a, 'b) result  end) =
 *   struct
 *     type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result
 *
 *     type 'dummy0 i =
 *       | T : (('env, 'a, 'a2) result -> ('env, 'a t, 'a2 t) result) i
 *   end
 *
 *   module Istateful_t = Index_stateful_t (struct
 *     type ('env, 'a, 'b) result = 'env -> 'a -> 'env * 'b
 *   end)
 *
 *   module Fix_stateful_t = FixV (Istateful_t)
 *
 *   class ['x, 'x_2, 'env, 'extra_t] stateful_t_t _ fx fself_t =
 *     object
 *       inherit ['env, 'x, 'env * 'x_2, 'env, 'extra_t, 'env * 'x_2 t] t_t
 *
 *       inherit
 *         [string, string, 'env, 'extra_t] B.stateful_t_t
 *           B.stateful_t_fix
 *           (fun inh subj -> stateful_string inh subj)
 *           (fun inh subj -> match subj with #B.t as subj -> fself_t inh subj)
 *       inherit
 *         [int, int, 'env, 'extra_t] A.stateful_t_t
 *           A.stateful_t_fix
 *           (fun inh subj -> stateful_int inh subj)
 *           (fun inh subj -> match subj with #A.t as subj -> fself_t inh subj)
 *
 *     end
 *
 *   let stateful_t_0 call fa inh0 subj =
 *     Utils.fix (fun fself ->  gcata_t (new stateful_t_t call fa fself)) inh0 subj
 *
 *
 *   let stateful_t_fix =
 *     Fix_stateful_t.fixv (fun f ->
 *         { call=
 *             (fun (type a) (sym : a Istateful_t.i) ->
 *               (match sym with Istateful_t.T -> stateful_t_0 f : a) ) } )
 *
 *   let t =
 *     { Utils.gcata= gcata_t
 *     }
 * end
 *
 * end *)


let eval_string inh subj = subj
let eval_int    inh subj = subj

(* module Eval =  struct
 *
 * module A = struct
 *   type 'a t = [`A]
 *
 *   class virtual ['ia, 'a, 'sa, 'inh, 'extra, 'syn] t_t =
 *     object
 *       method virtual c_A : 'inh -> 'a t -> 'syn
 *     end
 *
 *   let gcata_t tr inh subj = match subj with `A -> tr#c_A inh subj
 *
 *   module type IndexResult2_t = sig
 *     type ('a, 'b) result
 *
 *     type 'dummy0 i = T : (('a, 'a2) result -> ('a t, 'a2 t) result) i
 *   end
 *
 *   module Index2_t (S : sig
 *     type ('a, 'b) result
 *   end) =
 *   struct
 *     type ('a, 'b) result = ('a, 'b) S.result
 *
 *     type 'dummy0 i = T : (('a, 'a2) result -> ('a t, 'a2 t) result) i
 *   end
 *
 *   module Ieval_t = Index2_t (struct
 *     type ('a, 'b) result = unit -> 'a -> 'b
 *   end)
 *
 *   module Fix_eval_t = FixV (Ieval_t)
 *
 *   class ['a, 'a_2, 'env, 'extra_t] eval_t_t _ fa fself_t =
 *     object
 *       inherit ['env, 'a, 'a_2, 'env, 'extra_t, 'extra_t] t_t
 *       method c_A inh___001_ _ = `A
 *     end
 *
 *   let eval_t_0 call fa inh0 subj =
 *     Utils.fix (fun fself ->  gcata_t (new eval_t_t call fa fself)) inh0 subj
 *
 *   let eval_t_fix =
 *     Fix_eval_t.fixv (fun f ->
 *         { call=
 *             (fun (type a) (sym : a Ieval_t.i) ->
 *               (match sym with Ieval_t.T -> eval_t_0 f : a) ) } )
 *
 *   let t =
 *     { Utils.gcata= gcata_t
 *     }
 * end
 *
 * module B = struct
 *   type 'b t = [`B]
 *
 *   class virtual ['ib, 'b, 'sb, 'inh, 'extra, 'syn] t_t =
 *     object
 *       method virtual c_B : 'inh -> 'b t -> 'syn
 *     end
 *
 *   let gcata_t tr inh subj = match subj with `B -> tr#c_B inh subj
 *
 *   module type IndexResult2_t = sig
 *     type ('a, 'b) result
 *
 *     type 'dummy0 i = T : (('a, 'a2) result -> ('a t, 'a2 t) result) i
 *   end
 *
 *   module Index2_t (S : sig
 *     type ('a, 'b) result
 *   end) =
 *   struct
 *     type ('a, 'b) result = ('a, 'b) S.result
 *
 *     type 'dummy0 i = T : (('a, 'a2) result -> ('a t, 'a2 t) result) i
 *   end
 *
 *   module Ieval_t = Index2_t (struct
 *     type ('a, 'b) result = unit -> 'a -> 'b
 *   end)
 *
 *   module Fix_eval_t = FixV (Ieval_t)
 *
 *   class ['b, 'b_2, 'env, 'extra_t] eval_t_t _ fb fself_t =
 *     object
 *       inherit ['env, 'b, 'b_2, 'env, 'extra_t, 'extra_t] t_t
 *
 *       method c_B inh___002_ _ = `B
 *     end
 *
 *   let eval_t_0 call fa inh0 subj =
 *     Utils.fix (fun fself ->  gcata_t (new eval_t_t call fa fself)) inh0 subj
 *
 *   let eval_t_fix =
 *     Fix_eval_t.fixv (fun f ->
 *         { call=
 *             (fun (type a) (sym : a Ieval_t.i) ->
 *               (match sym with Ieval_t.T -> eval_t_0 f : a) ) } )
 *
 *   let t =
 *     { Utils.gcata= gcata_t
 *     }
 * end
 *
 * module Z = struct
 *   type 'x t = [int A.t | string B.t]
 *
 *   class virtual ['ix, 'x, 'sx, 'inh, 'extra, 'syn] t_t =
 *     object
 *       inherit [int, int, int, 'inh, 'extra, 'syn] A.t_t
 *
 *       inherit [string, string, string, 'inh, 'extra, 'syn] B.t_t
 *     end
 *
 *   let gcata_t tr inh subj =
 *     match subj with
 *     | #A.t as subj -> A.gcata_t tr inh subj
 *     | #B.t as subj -> B.gcata_t tr inh subj
 *
 *   module type IndexResult_t = sig
 *     type 'a result
 *
 *     type 'dummy0 i = T : ('a result -> 'a t result) i
 *   end
 *
 *   module Index2_t (S : sig
 *     type ('a, 'b) result
 *   end) =
 *   struct
 *     type ('a, 'b) result = ('a, 'b) S.result
 *
 *     type 'dummy0 i = T : (('a, 'a2) result -> ('a t, 'a2 t) result) i
 *   end
 *
 *   module Ieval_t = Index2_t (struct
 *     type ('a, 'b) result = unit -> 'a -> 'b
 *   end)
 *
 *   module Fix_eval_t = FixV (Ieval_t)
 *
 *   class ['x, 'x_2, 'env, 'extra_t] eval_t_t _ fx fself_t =
 *     object
 *       inherit ['env, 'x, 'x_2, 'env, 'extra_t, 'extra_t] t_t
 *
 *       inherit
 *         [int, int, 'env, 'extra_t] A.eval_t_t
 *           A.eval_t_fix
 *           (fun inh subj -> eval_int inh subj)
 *           (fun inh subj -> match subj with #A.t as subj -> fself_t inh subj)
 *
 *       inherit
 *         [string, string, 'env, 'extra_t] B.eval_t_t
 *           B.eval_t_fix
 *           (fun inh subj -> eval_string inh subj)
 *           (fun inh subj -> match subj with #B.t as subj -> fself_t inh subj)
 *     end
 *
 *   let eval_t_0 call fa inh0 subj =
 *     Utils.fix (fun fself ->  gcata_t (new eval_t_t call fa fself)) inh0 subj
 *
 *   let eval_t_fix =
 *     Fix_eval_t.fixv (fun f ->
 *         { call=
 *             (fun (type a) (sym : a Ieval_t.i) ->
 *               (match sym with Ieval_t.T -> eval_t_0 f : a) ) } )
 *
 *   let t =
 *     { Utils.gcata= gcata_t }
 * end
 *
 * end *)



module WTF = struct

type 'a s = SS of 'a
and t = int s
and u = float s

let gcata_s tr inh = function SS a -> tr#c_SS inh a
let gcata_t = gcata_s
let gcata_u = gcata_s

(* should be generated *)
module type IndexResult = sig
  type _ result
  type _ i =
    | S : ('a result -> 'a s result) i
    | T : t result i
    | U : u result i
end
module Index (S: sig type 'a result end) =
struct
  type 'a i =
    | S : ('a S.result -> 'a s S.result) i
    | T : t S.result i
    | U : u S.result i
end

(* should be generated *)
(* module type IndexResult2 = sig
 *   type (_,_) result
 *   type _ i =
 *     | S : (('a,'b) result -> ('a s,'b s) result) i
 *     | T : (t,t) result i
 *     | U : (u,u) result i
 * end
 * module Index2 (S : sig type ('a, 'b) result end) = struct
 *   type ('a,'b) result = ('a,'b) S.result
 *   type _ i =
 *     | S : (('a,'b) result -> ('a s,'b s) result) i
 *     | T : (t,t) result i
 *     | U : (u,u) result i
 * end *)

module Show =
struct
  module I = Index(struct type 'a result = 'a -> string end)
  module FixSgmap = FixV(I)

  class ['a] show_s (call: FixSgmap.fn) fa = object
    method c_SS () (a: 'a) = sprintf "SS (%s)" (fa a)
  end
  class ['a] show_t call = object
    method do_t () x : string = call.FixSgmap.call I.S (sprintf "%d") x
  end
  class ['a] show_u call = object
    method do_u () x = call.FixSgmap.call I.S (sprintf "%f") x
  end
  let show0_s call fa s =
    gcata_s (new show_s call fa) () s

  let show0_t {FixSgmap.call}           = call I.S (sprintf "%d")
  let show0_u {FixSgmap.call}           = call I.S (sprintf "%f")

  let show = FixSgmap.fixv @@ fun f ->
   { call = fun (type a) (sym : a I.i) : a -> match sym with
     | I.S -> show0_s f
     | I.T -> show0_t f
     | I.U -> show0_u f }

  let show_s x = show.FixSgmap.call I.S x
  let show_t x = show.FixSgmap.call I.T x
  let show_u x = show.FixSgmap.call I.U x
end

end
