(* Remake from the paper `Code reuse through polymorphic variants`
 * https://www.math.nagoya-u.ac.jp/~garrigue/papers/mixev3.04.ml.txt
 * use visitors library (kind of, the same ideas actually because
 * visitors seems not to support polymorphic variants yet)
 * *)

let rec to_string = function
| `Var   s     -> s
| `Abs  (s, x) -> "(\\" ^ s ^ " -> " ^ to_string x ^ ")"
| `App  (x, y) -> "(" ^ to_string x ^ " " ^ to_string y ^ ")"
| `Num   i     -> string_of_int i
| `Add  (x, y) -> "(" ^ to_string x ^ " + " ^ to_string y ^ ")"
| `Mult (x, y) -> "(" ^ to_string x ^ " * " ^ to_string y ^ ")"

let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n

type var = [`Var of string ]

class ['self] var_eval = object(self: 'self)
  method visit_Var env name = try List.assoc name env with Not_found -> `Var name

  method visit_var env = function `Var s -> self#visit_Var env s
end

type 'a lambda = [var | `Abs of string * 'a | `App of 'a * 'a]

class ['self] lambda_eval = object(self: 'self)
  inherit ['self] var_eval

  method visit_Abs fa env name l1 =
    let s' = gensym () in
    `Abs (s', fa ((name, `Var s')::env) l1)

  method visit_App fa s l1 l2 =
    let l2' = fa s l2 in
    match fa s l1 with
    | `Abs (s, body) -> fa [s, l2'] body
    | l1' -> `App (l1', l2')

  method visit_lambda fa env = function
    | `Abs (name,body) -> self#visit_Abs fa env name body
    | `App (l,r)    -> self#visit_App fa env l r
    | #var  as subj -> self#visit_var env subj
 end

let rec eval1 env l =
  (new lambda_eval)#visit_lambda eval1 env l

let () =
  Printf.printf "%s\n%!" @@ to_string @@
  eval1 ["x", `Var "y"] (`Abs ("y", `Var "x"))

(* 2nd extension *)
type 'a var_expr = [var | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a]

class ['self] var_expr_eval = object(self: 'self)
  inherit ['self] lambda_eval

  method visit_Num fa s i   = `Num i
  method visit_Add fa s l r =
    match self#visit_var_expr fa s l, self#visit_var_expr fa s r with
    | `Num x, `Num y -> `Num (x+y)
    | x, y -> `Add (x, y)
  method visit_Mult fa s l r =
    match self#visit_var_expr fa s l, self#visit_var_expr fa s r with
    | `Num x, `Num y -> `Num (x*y)
    | x, y -> `Mult (x, y)

  method visit_var_expr fa env = function
    | `Num n -> self#visit_Num fa env n
    | `Add (l,r) -> self#visit_Add fa env l r
    | `Mult (l,r) -> self#visit_Mult fa env l r
    | #var as v -> self#visit_var env v
 end

let rec eval2 env l =
  (new var_expr_eval)#visit_var_expr eval2 env l

(* 3rd extension *)
type 'a expr = ['a lambda | 'a var_expr]

class ['self] expr_eval = object(self: 'self)
  inherit ['self] lambda_eval
  inherit ['self] var_expr_eval

  method visit_expr fa env = function
    | #lambda as l   -> self#visit_lambda fa env l
    | #var_expr as v -> self#visit_var_expr fa env v
end

(* Error here *)
(* let rec eval3 env (l: _ expr) =
 *   (new expr_eval)#visit_expr eval3 env l
 *
 * let _ =
 *   Printf.printf "%s\n" @@ to_string @@
 *   eval3 ["x", `Num 5; "y", `Num 6] (`Add (`Var "x", `Mult (`Num 2, `Var "y"))) *)
