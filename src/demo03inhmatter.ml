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

let tree = { gcata = gcata_tree
           ; fix = (fun c -> GT.transform_gc gcata_tree c)
           }
let prettify_tree fmt fa subj =
  transform1(tree) (new prettify_tree fa) fmt subj

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

class ['a] wtfo_logic typ_a fa fself = object
  inherit ['a, unit, string, unit, string] class_logic
  method c_Var   fmt n = sprintf "_.%d is `%s`" n typ_a
  method c_Value fmt x = fa () x
end

let wtfo_logic typ_a fa s = GT.transform(logic) (new wtfo_logic typ_a fa) () s

class ['a] fmt_logic typ_a fa fself = object
  inherit ['a, 'inh, unit, 'inh, unit] class_logic
  constraint 'inh = Format.formatter
  method c_Var   fmt n = Format.fprintf fmt "_.%d is `%s`" n typ_a
  method c_Value fmt x = fa fmt x
end

let fmt_logic typ_a fa fmt s =
  Utils.transform1(logic) (new fmt_logic typ_a fa) fmt s

let () =
  printf "show-with-type: %s\n"  (wtfo_logic "int" (lift id) (Var 1));
  printf "show-with-type: %s\n"  (wtfo_logic "int" (lift@@sprintf "%f") (Value 3.14));
  Format.printf "fmt-with-type: %a\n" (fmt_logic "int" fmt_float) (Var 1);
  Format.printf "fmt-with-type: %a\n" (fmt_logic "int" fmt_float) (Value 3.14);
  ()

(* ********************************** Now for logic lists ******************** *)
class ['a, 'b] wtfo_alist typ_a fa typ_b fb fself =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_alist
    method c_Nil  ()     = "Nil"
    method c_Cons () a b = sprintf "Cons (%s, %s)" (fa () a) (fb () b)
  end

let wtfo_alist typ_a fa typ_b fb t =
  GT.transform(alist) (new wtfo_alist typ_a fa typ_b fb) () t

(*  Now for llist *)
class ['a] wtfo_llist (typ_a:string) (fa: unit -> 'a -> string)
     (fself: unit -> 'a llist -> string)
  = object
  inherit ['a, unit, string, unit, string] class_llist
  inherit [('a logic, 'b logic) alist as 'b] wtfo_logic
      (sprintf "(%s logic, 'b logic) alist as 'b" typ_a)
      (fun () ->
         wtfo_alist
           (sprintf "%s logic" typ_a)
           (fun () -> wtfo_logic typ_a fa)
           (sprintf "(%s logic, %s llist) alist logic" typ_a typ_a)
           fself
      )
      "3"
end

let wtfo_llist typ_a (fa: unit -> 'a -> string) (t: 'a llist) =
  GT.transform(llist) (new wtfo_llist typ_a fa) () t

let () =
  printf "%s\n%!" @@ wtfo_llist "int" (lift @@ sprintf "%d") @@
    Value (Cons (Value 1, Value (Cons (Var 11, Value (Cons (Value 2, Var 12))))))


(* ****************** Fromatting for logic lists ******************** *)
class ['a, 'b] fmt_alist typ_a fa typ_b fb fself = object
  inherit ['a, 'inh, unit, 'b, 'inh, unit, 'inh, unit] class_alist
  constraint 'inh = Format.formatter
  method c_Nil  fmt     = Format.fprintf fmt "Nil"
  method c_Cons fmt a b = Format.fprintf fmt "Cons (%a, %a)" fa a fb b
end

let fmt_alist typ_a fa typ_b fb fmt s =
  transform1(alist) (new fmt_alist typ_a fa typ_b fb) fmt s

(*  Now for llist *)
class ['a] fmt_llist typ_a fa (fself: 'inh -> 'a llist -> unit)
  = object
  inherit ['a, 'inh, unit, 'inh, unit] class_llist
  constraint 'inh = Format.formatter
  inherit [('a logic, 'b logic) alist as 'b] fmt_logic
      (sprintf "(%s logic, 'b logic) alist as 'b" typ_a)
      (
         fmt_alist
           (sprintf "%s logic" typ_a)
           (fmt_logic typ_a fa)
           (sprintf "(%s logic, %s llist) alist logic" typ_a typ_a)
           fself
      )
      "3"
end

let fmt_llist typ_a (fa: 'inh -> 'a -> unit) fmt (t: 'a llist) =
  transform1(llist) (new fmt_llist typ_a fa) fmt t

let () =
  Format.printf "Formated output:\n%!";
  fmt_llist "int" fmt_int Format.std_formatter @@
  Value (Cons (Value 1, Value (Cons (Var 11, Value (Cons (Value 2, Var 12))))));
  Format.printf "\n%!"
