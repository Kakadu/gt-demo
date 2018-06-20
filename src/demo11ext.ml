open Utils

let ith m n = fix (fun me i -> function
                   | []                 -> raise Not_found
                   | x :: tl when x = n -> i
                   | _ :: tl            -> me (i+1) tl
  ) 0 m

module E = struct
  type (_,_,_) expr = ..

  let gcata _ _ _ = failwith "Not defined yet"
  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] expr_t = object end
end

module Lam = struct
  type ('name_abs, 'name, 'lam) E.expr += App of 'lam * 'lam | Var of 'name

  let gcata tr inh = function
    | App (l,r) -> tr#c_App inh l r
    | Var s -> tr#c_Var inh s
    | old -> E.gcata tr inh old

  class virtual [ 'iname_a, 'name_a, 'sname_a
                , 'iname, 'name, 'sname
                , 'ilam, 'lam, 'slam
                , 'inh, 'self, 'syn
                ] expr_t = object
    method virtual c_App : 'inh -> 'lam -> 'lam -> 'syn
    method virtual c_Var : 'inh -> 'name -> 'syn
  end

  class ['name_a, 'name, 'lam, 'self ] show_expr_t _fself fname flam = object
    inherit [ unit, 'name_a, string
            , unit, 'name, string
            , unit, 'lam, string
            , unit, 'self, string ] expr_t
    method c_App () l r = Printf.sprintf "App (%s, %s)" (flam l) (flam r)
    method c_Var () name = Printf.sprintf "Var (%s)" (fname name)
  end
  class ['name_a, 'name_a2, 'name, 'name2, 'lam, 'lam2, 'inh, 'self ] eval_expr_t
      _fself fdecl_var fuse_var flam = object
    inherit [ 'inh, 'name_a, 'name_a2
            , 'inh, 'name, 'name2
            , 'inh, 'lam, 'lam2
            , 'inh, 'self, ('name_a2, 'name2, 'lam2) E.expr
            ] expr_t
    method c_App e l r = App (flam e l, flam e r)
    method c_Var e name = Var (fuse_var e name)
  end
  class ['me, 'me2] de_bruijn fuse_var fterm = object
    inherit [string, unit, string, int, 'me, 'me2, string list, 'me2] eval_expr_t
        unused (fun _ (_: string) -> ()) fuse_var fterm
  end
end

module Abs = struct
  type ('name_a, 'name, 'lam) E.expr += Abs of 'name_a * 'lam
  class virtual [ 'iname_a, 'name_a, 'sname_a
                , 'iname, 'name, 'sname
                , 'ilam, 'lam, 'slam
                , 'inh, 'self, 'syn
                ] expr_t = object
    inherit [ 'iname_a, 'name_a, 'sname_a
            , 'iname, 'name, 'sname
            , 'ilam, 'lam, 'slam
            , 'inh, 'self, 'syn
            ] Lam.expr_t
    method virtual c_Abs : 'inh -> 'name_a -> 'lam -> 'syn
  end

  let gcata tr inh = function
    | Abs (l,r) -> tr#c_Abs inh l r
    | old -> Lam.gcata tr inh old

  class ['name_a, 'name, 'lam, 'self ] show_expr_t _fself fname_a fname flam = object
    inherit [ unit, 'name_a, string
            , unit, 'name, string
            , unit, 'lam, string
            , unit, 'self, string ] expr_t
    inherit ['name_a, 'name, 'lam, 'self ] Lam.show_expr_t _fself fname flam
    method c_Abs () name term = Printf.sprintf "Abs (%s, %s)" (fname_a name) (flam term)
  end
  class ['name_a, 'name_a2, 'name, 'name2, 'lam, 'lam2, 'inh, 'self ] eval_expr_t
      _fself fdeclvar fname flam = object
    inherit [ 'inh, 'name_a, 'name_a2
            , 'inh, 'name, 'name2
            , 'inh, 'lam, 'lam2
            , 'inh, 'self, ('name_a2, 'name2, 'lam2) E.expr
            ] expr_t
    inherit ['name_a, 'name_a2, 'name, 'name2, 'lam, 'lam2, 'inh, 'self ]
        Lam.eval_expr_t _fself fdeclvar fname flam
    method c_Abs e l r = Abs (fdeclvar e l, flam e r)
  end

  class ['me, 'me2] de_bruijn fuse_var ft = object
    inherit [ 'inh, 'name_a, 'name_a2
            , 'inh, 'name, 'name2
            , 'inh, 'lam, 'lam2
            , 'inh, 'self, ('name_a2, 'name2, 'lam2) E.expr
            ] expr_t
    inherit ['me, 'me2] Lam.de_bruijn fuse_var ft
    method c_Abs env name term = Abs ((), ft (name :: env) term)
  end

end

module Let = struct
  type ('name_a, 'name, 'lam) E.expr += Let of 'name_a * 'lam * 'lam
  let gcata tr inh = function
    | Let (l,r,in_) -> tr#c_Let inh l r in_
    | old -> Abs.gcata tr inh old

  class virtual [ 'iname_a, 'name_a, 'sname_a
                , 'iname, 'name, 'sname
                , 'ilam, 'lam, 'slam
                , 'inh, 'self, 'syn
                ] expr_t = object
    method virtual c_Let : 'inh -> 'name_a -> 'lam -> 'lam -> 'syn
  end
  class ['name_a, 'name, 'lam, 'self ] show_expr_t _fself fname_a fname flam = object
    inherit [ unit, 'name_a, string
            , unit, 'name, string
            , unit, 'lam, string
            , unit, 'self, string ] expr_t
    inherit ['name_a, 'name, 'lam, 'self ] Abs.show_expr_t _fself fname_a fname flam
    method c_Let () name l in_ =
        Printf.sprintf "Let (%s, %s, %s)"
          (fname_a name)
          (* (assert false) *)
          (flam l) (flam in_)
  end

  class ['me, 'me'] de_bruijn fuse_var ft = object
    inherit [ string list, string, unit
            , string list, string, int
            , string list, 'me, 'me'
            , string list, 'me, 'me'] expr_t
    inherit ['me, 'me'] Abs.de_bruijn fuse_var ft
    method c_Let env (name: string) bnd term =
      Let ((), ft env bnd, ft (name :: env) term)
  end
end

module LetRec = struct
  type ('name_a, 'name, 'lam) E.expr += LetRec of 'name_a * 'lam * 'lam
  let gcata tr inh = function
    | LetRec (l,r,in_) -> tr#c_LetRec inh l r in_
    | old -> Let.gcata tr inh old

  class virtual [ 'iname_a, 'name_a, 'sname_a
                , 'iname, 'name, 'sname
                , 'ilam, 'lam, 'slam
                , 'inh, 'self, 'syn
                ] expr_t = object
    method virtual c_LetRec : 'inh -> 'name_a -> 'lam -> 'lam -> 'syn
  end
  class ['name_a, 'name, 'lam, 'self ] show_expr_t _fself fname_a fname flam = object
    inherit [ unit, 'name_a, string
            , unit, 'name, string
            , unit, 'lam, string
            , unit, 'self, string ] expr_t
    inherit ['name_a, 'name, 'lam, 'self ] Let.show_expr_t _fself fname_a fname flam
    method c_LetRec () name l in_ =
        Printf.sprintf "Let (%s, %s, %s)"
          (fname_a name)
          (* (assert false) *)
          (flam l) (flam in_)
  end

  class ['me, 'me'] de_bruijn fuse_var ft = object
    inherit [ string list, string, unit
            , string list, string, int
            , string list, 'me, 'me'
            , string list, 'me, 'me'] expr_t
    inherit ['me, 'me'] Let.de_bruijn fuse_var ft
    method c_LetRec env name bnd term =
      let env2 = name::env in
      LetRec ((), ft env2 bnd, ft env2 term)
  end
end


type ('var_abs, 'n, 'b) t = ('var_abs, 'n, ('var_abs, 'n, 'b) t) E.expr
let show_t fname fterm lam =
  let rec helper lam =
    Let.gcata (new Let.show_expr_t helper fname fname helper) () lam
  in
  helper lam

type named = (string, string, string) t
type nameless = (unit, int, unit) t

let show_string = Printf.sprintf "%S"
let show_int = Printf.sprintf "%d"
let show_unit _ = Printf.sprintf "()"

let show_named (t: named) =
  Utils.fix0 (fun fself ->
      LetRec.gcata (new LetRec.show_expr_t fself show_string show_string fself) ()
    ) t
let show_nameless (t: nameless) =
  Utils.fix0 (fun fself ->
      LetRec.gcata (new LetRec.show_expr_t fself show_unit show_int fself) ()
    ) t

(* to de Bruijn *)
let convert (lam: named) : nameless =
  let rec helper env lam =
      LetRec.gcata (new LetRec.de_bruijn ith helper) env lam
  in
  helper [] lam


let () =
  let l : named =
    let open Lam in let open Abs in
    App (Abs ("x", Var "x"), Abs ("y", Var "y"))
  in
  let l2 : named =
    let open Lam in let open Abs in let open Let in
    Let ("z", Abs ("x", Var "x"), Abs ("x", Abs ("y", App (Var "x", Var "z"))))
  in
  let l3 =
    let open Lam in let open Abs in let open Let in  let open LetRec in
    LetRec ("z", App (Abs ("x", Var "x"), Var "z"), Abs ("x", Abs ("y", App (Var "x", Var "z"))))
  in
  Printf.printf "Original: %s\n" (show_named l);
  Printf.printf "Converted: %s\n" (show_nameless @@ convert l);
  Printf.printf "Converted: %s\n" (show_nameless @@ convert l2);
  Printf.printf "Converted: %s\n" (show_nameless @@ convert l3);
  ()
