open Utils

module E = struct
  type (_,_) expr = ..

  let gcata _ _ _ = failwith "Not defined yet"
  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] expr_t = object end
end

module Lam = struct
  type ('name, 'lam) E.expr += App of 'lam * 'lam | Var of 'name (* with show, eval *)

  let gcata tr inh = function
    | App (l,r) -> tr#c_App inh l r
    | Var s -> tr#c_Var inh s
    | old -> E.gcata tr inh old

  class virtual [ 'iname, 'name, 'sname
                , 'ilam, 'lam, 'slam
                , 'inh, 'self, 'syn
                ] expr_t = object
    method virtual c_App : 'inh -> 'lam -> 'lam -> 'syn
    method virtual c_Var : 'inh -> 'name -> 'syn
  end

  class ['name, 'lam, 'self ] show_expr_t _fself fname flam = object
    inherit [ unit, 'name, string, unit, 'lam, string, unit, 'self, string ] expr_t
    method c_App () l r = Printf.sprintf "App (%s, %s)" (flam l) (flam r)
    method c_Var () name = Printf.sprintf "Var (%s)" (fname name)
  end
  class ['name, 'name2, 'lam, 'lam2, 'inh, 'self ] eval_expr_t _fself fname flam = object
    inherit [ 'inh, 'name, 'name2
            , 'inh, 'lam, 'lam2
            , 'inh, 'self, ('name2, 'lam2) E.expr
            ] expr_t
    method c_App e l r = App (flam e l, flam e r)
    method c_Var e name = Var (fname e name)
  end
  class ['me, 'me2] de_bruijn ft = object
    inherit [string, unit, 'me, 'me2, string list, 'me2] eval_expr_t
        unused (fun _ _ -> ()) ft
  end

end


module Abs = struct
  type ('name, 'lam) E.expr += Abs of 'name * 'lam  (* with show, eval *)
  class virtual [ 'iname, 'name, 'sname
                , 'ilam, 'lam, 'slam
                , 'inh, 'self, 'syn
                ] expr_t = object
    method virtual c_Abs : 'inh -> 'name -> 'lam -> 'syn
  end

  let gcata tr inh = function
    | Abs (l,r) -> tr#c_Abs inh l r
    | old -> Lam.gcata tr inh old

  class ['name, 'lam, 'self ] show_expr_t _fself fname flam = object
    inherit [ unit, 'name, string, unit, 'lam, string, unit, 'self, string ] expr_t
    inherit ['name, 'lam, 'self ] Lam.show_expr_t _fself fname flam
    method c_Abs () name term = Printf.sprintf "Abs (%s, %s)" (fname name) (flam term)
  end
  class ['name, 'name2, 'lam, 'lam2, 'inh, 'self ] eval_expr_t _fself fname flam = object
    inherit [ 'inh, 'name, 'name2
            , 'inh, 'lam, 'lam2
            , 'inh, 'self, ('name2, 'lam2) E.expr
            ] expr_t
    inherit ['name, 'name2, 'lam, 'lam2, 'inh, 'self ] Lam.eval_expr_t _fself fname flam
    method c_Abs e l r = Abs (fname e l, flam e r)
  end

  class ['me, 'me2] de_bruijn ft = object
    inherit ['me, 'me2] Lam.de_bruijn ft
    method c_Abs env name term = Abs ((), ft (name :: env) term)
  end

end

module Let = struct
  type ('name, 'lam) E.expr += Let of 'name * 'lam * 'lam  (* with show, eval *)
  let gcata tr inh = function
    | Let (l,r,in_) -> tr#c_Let inh l r in_
    | old -> Abs.gcata tr inh old

  class virtual [ 'iname, 'name, 'sname
                , 'ilam, 'lam, 'slam
                , 'inh, 'self, 'syn
                ] expr_t = object
    method virtual c_Let : 'inh -> 'name -> 'lam -> 'lam -> 'syn
  end
  class ['name, 'lam, 'self ] show_expr_t _fself fname flam = object
    inherit [ unit, 'name, string, unit, 'lam, string, unit, 'self, string ] expr_t
    inherit ['name, 'lam, 'self ] Abs.show_expr_t _fself fname flam
    method c_Let () name l in_ =
      Printf.sprintf "Let (%s, %s, %s)" (fname name) (flam l) (flam in_)
  end
  class ['me, 'me'] de_bruijn ft = object
    inherit [ string list, string, unit
            , string list, 'me, 'me'
            , string list, 'me, 'me'] expr_t
    inherit ['me, 'me'] Abs.de_bruijn ft
    method c_Let env name bnd term = Let ((), ft env bnd, ft (name :: env) term)
  end

end

type ('n, 'b) t = ('n, ('n, 'b) t) E.expr
let show_t fname fterm lam =
  let rec helper lam =
    Let.gcata (new Let.show_expr_t helper fname helper) () lam
  in
  helper lam

type named = (string, string) t
type nameless = (int, unit) t

(* let (_:int) = Let.gcata (new Let.show_expr_t fself show_string fself) () *)

let show_string = Printf.sprintf "%S"
let show_int = Printf.sprintf "%d"
let show_unit _ = Printf.sprintf "()"

let show_named (t: named) =
  Utils.fix0 (fun fself ->
      Let.gcata (new Let.show_expr_t fself show_string fself) ()
    ) t
let show_nameless (t: nameless) =
  Utils.fix0 (fun fself ->
      Let.gcata (new Let.show_expr_t fself show_int show_unit) ()
    ) t

(* to de Bruijn *)
let convert (lam: named) : nameless =
  let rec helper env lam =
      Let.gcata (new Let.de_bruijn helper) env lam
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
  let _l3 =
    let open Lam in let open Abs in
    1
  in
  Printf.printf "Original: %s\n" (show_named l);
  Printf.printf "Converted: %s\n" (show_nameless @@ convert l);
  Printf.printf "Converted: %s\n" (show_nameless @@ convert l2);
  (* Printf.printf "Converted: %s\n" (show_nameless @@ convert (`LetRec ("z", `App (`Abs ("x", `Var "x"), `Var "z"), `Abs ("x", `Abs ("y", `App (`Var "x", `Var "z")))))); *)
  ()
