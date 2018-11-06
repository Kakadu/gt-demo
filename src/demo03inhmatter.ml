open Utils
open Printf

(* We will implement a show-to-Format.t there *)

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_tree =
  object
    method virtual c_Leaf : 'inh -> 'a -> 'syn
    method virtual c_Node : 'inh -> 'a tree -> 'a -> 'a tree -> 'syn
  end

class ['a] prettify_tree fa fself = object
  inherit ['a, Format.formatter, unit, Format.formatter, unit] class_tree
  method c_Leaf fmt x = Format.fprintf fmt "Leaf %a" fa x
  method c_Node fmt l x r =
    Format.fprintf fmt "@[Node@ (%a,@ %a,@ %a)@]" fself l fa x fself r
end

let rec gcata_tree tr inh t =
  match t with
  | Node (l,x,r) -> tr#c_Node inh l x r
  | Leaf x       -> tr#c_Leaf inh x

let tree = { gcata = gcata_tree; plugins = object end }
let prettify_tree fmt fa subj =
  transform1(tree) (new prettify_tree fa) fmt subj
(* fix (fun self fmt tree ->
 *     gcata_tree (new prettify_tree self fa) fmt tree
 *   ) fmt t *)


let () =
  let fmt = Format.std_formatter in
  let () =
    let t1 = Node (Leaf 0, 1, Leaf 0) in
    let t2 = Node (t1, 2, t1) in
    let t3 = Node (t2, 3, t2) in
    prettify_tree fmt (fun fmt -> Format.fprintf fmt "%d")  t3
  in
  Format.printf "\n\n%!"


(* Now I want to try show-with-type example *)

open Demo07lists (* logic values are here *)

class ['a] wtfo_logic self typ_a fa = object
  inherit ['a, unit, string, unit, string] class_logic
  method c_Var   fmt n = sprintf "_.%d is `%s`" n typ_a
  method c_Value fmt x = fa x
end

let wtfo_logic typ_a fa t =
  (* let typ_self = sprintf "%s logic" typ_a in *)
  fix0 (fun self ->
    gcata_logic (new wtfo_logic self typ_a fa)) () t

let () =
  printf "show-with-type: %s\n"  (wtfo_logic "int" id (Var 1));
  printf "show-with-type: %s\n"  (wtfo_logic "int" (sprintf "%f") (Value 3.14));
  ()

(* ********************************** Now for logic lists ******************** *)
class ['a, 'b] wtfo_alist self typ_a fa typ_b fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_alist
    method c_Nil  ()     = "Nil"
    method c_Cons () a b = sprintf "Cons (%s, %s)" (fa a) (fb b)
  end

let wtfo_alist typ_a fa typ_b fb t =
  fix0 (fun self ->
    gcata_alist (new wtfo_alist self typ_a fa typ_b fb) ()
  ) t

(*  Now for llist *)
class ['a] wtfo_llist (fself: 'a llist -> string)
    (typ_a:string) (fa: 'a -> string)
  = object
  inherit ['a, unit, string, unit, string] class_llist
  inherit [('a logic, 'b logic) alist as 'b] wtfo_logic
      (fun _ -> "3")
      (sprintf "(%s logic, 'b logic) alist as 'b" typ_a)
      (
         wtfo_alist
           (sprintf "%s logic" typ_a) (wtfo_logic typ_a fa)
           (sprintf "(%s logic, %s llist) alist logic" typ_a typ_a) fself
      )
end

let wtfo_llist typ_a (fa: 'a -> string) (t: 'a llist) =
  fix0 (fun self ->
    gcata_logic (new wtfo_llist self typ_a fa ) ()
  ) t

let () =
  printf "%s\n%!" @@ wtfo_llist "int" (sprintf "%d") @@
    Value (Cons (Value 1, Value (Cons (Var 11, Value (Cons (Value 2, Var 12))))))
