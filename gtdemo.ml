open Printf
(* Type of augmented value *)
type ('i, 'a, 's) a = {x : 'a; fx : 'i -> 's; f : 'i -> 'a -> 's}

let make x f = {x; f; fx = fun i -> f i x}

(* ========= Example starts here =========== *)

(* The type *)
type ('a, 'b) l = Nil | Cons of 'a * 'b

(* Generic transformer class *)
class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'i, 's, 'self, 'aa, 'bb] l_class =
  object
    method virtual c_Nil  : 'i -> 'self -> 's
    method virtual c_Cons : 'i -> 'self -> 'aa -> 'bb -> 's
  end

(* Class coercion *)
let l_coerce (x  : ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'i, 's, 'self, 'aa, 'bb) #l_class)
             (fs : 'self' -> 'self)
             (fa : 'aa2   -> 'aa)
             (fb : 'bb2   -> 'bb) :
             ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'i, 's, 'self', 'aa2, 'bb2) #l_class
  = object
      method c_Nil  i s     = x#c_Nil  i (fs s)
      method c_Cons i s a b = x#c_Cons i (fs s) (fa a) (fb b)
    end

(* Generic transformer function *)
let rec l_gcata
    (fa : 'ia -> 'a -> 'sa)
    (fb : 'ib -> 'b -> 'sb)
    (tr : ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'i, 's, ('i, ('a, 'b) l, 's) a, ('ia, 'a, 'sa) a, ('ib, 'b, 'sb) a) #l_class)
    (i  : 'i)
    (s  : ('a, 'b) l) =
  match s with
  | Nil         -> tr#c_Nil  i (make s (l_gcata fa fb tr))
  | Cons (a, b) -> tr#c_Cons i (make s (l_gcata fa fb tr)) (make a fa) (make b fb)

(* Show transformer *)
class ['a, 'b, 'self, 'aa, 'bb] l_meta_show (fa : 'self -> 'aa -> string) (fb : 'self -> 'bb -> string) =
  object
    inherit [unit, 'a, string, unit, 'b, string, unit, string, 'self, 'aa, 'bb] l_class
    method c_Nil  _ _     = "Nil"
    method c_Cons _ s a b = Printf.sprintf "Cons (%s, %s)" (fa s a) (fb s b)
  end

(*let l (fa : 'self -> 'aa -> string) = new l_meta_show  fa*)

class ['a, 'b] l_show =
  object
    inherit ['a, 'b, (unit, ('a, 'b) l, string) a, (unit, 'a, string) a, (unit, 'b, string) a] l_meta_show (fun _ a -> a.fx ()) (fun _ b -> b.fx ())
  end

class ['a, 'a_syn, 'b, 'b_syn, 'self, 'aa, 'bb] l_meta_gmap (fa : 'self -> 'aa -> 'a_syn) (fb : 'self -> 'bb -> 'b_syn) =
  object
    inherit [unit, 'a, 'a_syn, unit, 'b, 'b_syn, unit, ('a_syn,'b_syn) l, 'self, 'aa, 'bb] l_class
    method c_Nil  _ _     = Nil
    method c_Cons _ s a b = Cons (fa s a, fb s b)
  end

class ['a, 'a_syn, 'b, 'b_syn] l_show =
  object
    inherit ['a, 'b, (unit, ('a, 'b) l, string) a, (unit, 'a, string) a, (unit, 'b, string) a] l_meta_show
      (fun _ a -> a.fx ())
      (fun _ b -> b.fx ())
  end

class ['a, 'a_syn, 'b, 'b_syn, 'self] l_gmap = object
  inherit ['a, 'a_syn, 'b, 'b_syn, (unit, ('a, 'b) l, 'syn_r) a, (unit, 'a, 'a_syn) a, (unit, 'b, 'b_syn) a] l_meta_gmap
    (fun _ a -> a.fx ())
    (fun _ b -> b.fx ())
  constraint 'syn_r = ('a_syn,'b_syn) l
end

let lift f = fun () x -> f x
let id x = x

let _ =
  let rec s () x = l_gcata (lift string_of_int) s (new l_show) () x in
  let a = (Cons (1, Cons (2, Nil))) in
  printf "Test show: %s\n" @@ s () a;
  let rec foo ona x = l_gcata ona (fun () -> foo ona) (new l_gmap) () x in
  printf "Test gmap: %s\n" @@ s () (foo (fun () -> (+)1) a)

(* ======================== First application ================================= *)
(* Type application *)
type 'a list = ('a, 'a list) l

class virtual ['ia, 'a, 'sa, 'i, 's, 'self, 'aa] list_class =
  object
    inherit ['ia, 'a, 'sa, 'i, 'a list, 's, 'i, 's, 'self, 'aa, 'a list] l_class
  end

let list_coerce (x : ('ia, 'a, 'sa, 'i, 's, 'self, 'aa) #list_class)
                (fs : 'self' -> 'self)
                (fa : 'aa'   -> 'aa) :
                ('ia, 'a, 'sa, 'i, 's, 'self', 'aa') #list_class =
                                          (* maybe l_class there *)
   l_coerce x fs fa (fun x -> x)

(* Transformation function *)
let rec list_gcata
    (fa : 'ia -> 'a -> 'sa)
    (tr : ('ia, 'a, 'sa, 'i, 's, ('i, 'a list, 's) a, ('ia, 'a, 'sa) a) #list_class)
    (i  : 'i)
    (s  : 'a list) =
  let convert tr =
    let cache = ref None in
    let rec inner () =
      match !cache with
      | Some c -> c
      | None ->
          Printf.printf "Here\n";
    let c =
      l_coerce tr
              (fun a -> make a.x (l_gcata fa (list_gcata fa tr) (inner ())))
              (fun x -> x)
              (fun l -> l.x)
    in
    cache := Some c;
          c
    in
    inner ()
  in
  l_gcata fa (list_gcata fa tr) (convert tr) i s

class ['a, 'self, 'aa] list_meta_show fa (* fb *) =
  object
    inherit ['a, 'a list, 'self, 'aa, 'a list] l_meta_show (fa : 'self -> 'aa -> string) (fun s t -> s.f () t)
      (*fb : 'self -> 'a list -> string*)
  end

class ['a] list_show =
  object
    inherit ['a, (unit, 'a list, string) a, (unit, 'a, string) a] list_meta_show (fun _ h -> h.fx ()) (*fun s t -> s.f () t*)
  end

let _ = Printf.printf "Test: %s\n" (list_gcata (lift string_of_int) (new list_show) () (Cons (1, Cons (2, Nil))))


(* =============================== Logics ================================== *)

open Printf

type 'a logic = Var of int | Value of 'a

class virtual ['ia, 'a, 'sa, 'i, 's, 'self, 'aa] logic_class =
  object
    method virtual c_Var   : 'i -> 'self -> int -> 's
    method virtual c_Value : 'i -> 'self -> 'aa -> 's
  end

let logic_coerce
      (x  : ('ia, 'a, 'sa, 'i, 's, 'self, 'aa) #logic_class)
      (fs : 'self2 -> 'self)
      (fa : 'aa2   -> 'aa) :
      (('ia, 'a, 'sa, 'i, 's, 'self2, 'aa2) #logic_class)
  = object
      method c_Var   i s n    = x#c_Var   i (fs s) n
      method c_Value i s v    = x#c_Value i (fs s) (fa v)
    end

let rec logic_gcata
    (fa : 'ia -> 'a -> 'sa)
    (tr : ('ia, 'a, 'sa, 'i, 's, ('i, 'a logic, 's) a, ('ia, 'a, 'sa) a) #logic_class)
    (i  : 'i)
    (s  : 'a logic) =
  match s with
  | Var n   -> tr#c_Var   i (make s (logic_gcata fa tr)) n
  | Value a -> tr#c_Value i (make s (logic_gcata fa tr)) (make a fa)

class ['a, 'self, 'aa] logic_meta_show (fa : 'self -> 'aa -> string) =
  object
    inherit [unit, 'a, string, unit, string, 'self, 'aa] logic_class
    method c_Var   _ _ n = sprintf "_.%d" n
    method c_Value _ s a = fa s a
  end

class ['a] logic_show =
  object
    inherit ['a, (unit, 'a logic, string) a, (unit, 'a, string) a] logic_meta_show (fun _ a -> a.fx ())
  end

let _ =
  let rec s () x = logic_gcata (lift string_of_int) (new logic_show) () x in
  printf "Test: %s\n" @@ s () (Var 1);
  printf "Test: %s\n" @@ s () (Value 18)

(* ==================== Almost logic lists ============================ *)
type ('a,'b) maybel = ('a, 'b) l logic

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'i, 's, 'self, 'aa, 'bb] maybel_class =
  object
    inherit ['ia, ('a, 'b) l, 'sa, 'i, 's, 'self, 'inner_aa] logic_class
    (* constraint 'inner_aa = ('i, ('a, 'b) l, 's) a (* maybe aa and bb here *) *)
    constraint 'inner_aa = ('aa, 'bb) l
  end

(* coerce-like functions will be used in aliases of maybe l to cast less general type
  to _ maybel *)
let maybel_coerce (x : ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'i, 's, 'self, 'aa, 'bb) #maybel_class)
                  (fs : 'self2 -> 'self)
                  (fa : 'aa2   -> 'aa)
                  (fb : 'bb2   -> 'bb) :
                  ('ia * 'ib, ('a, 'b) l, 'sa * 'sb, 'i, 's, 'self2, ('aa2, 'bb2) l) #logic_class =
(*   Obj.magic x*)
  logic_coerce x fs (fun (thel: ('aa2, 'bb2) l) -> l_gcata (fun () -> fa) (fun () -> fb) (new l_gmap) () thel)

let rec maybel_gcata
    (fa : 'ia -> 'a -> 'sa)
    (fb : 'ib -> 'b -> 'sb)
    (tr : ( 'ia, 'a, 'sa, 'ib, 'b, 'sb, 'i, 's
          , ('i, ('a,'b) maybel, 's) a
          , ('ia, 'a, 'sa) a, ('ib, 'b, 'sb) a) #maybel_class)
    (i  : 'i)
    (s  : ('a, 'b) maybel) =
  logic_gcata
    (invalid_arg "")
(*    (fun (ia, ib) (a, b) -> fa ia a, fb ib b)*)
     (maybel_coerce tr (invalid_arg "") (invalid_arg "") (invalid_arg ""))
(*    (invalid_arg "")*)
    i s

(* last error is about that logic_gcata expects last type argument [...,'aa] #logic_class
  to be a _ GT.a but maybel_coerce puts concrete type there *)

class ['a, 'b, 'self, 'aa, 'bb] maybel_meta_show (fa : 'self -> 'aa -> string) (fb : 'self -> 'bb -> string) =
  object
(*    inherit [('a,'b) l, 'self, (unit, ('aa,'bb) l, string) a ] logic_meta_show*)
    inherit [('a,'b) l, 'self, ('aa, 'bb) l ] logic_meta_show
(*      (fun _ p -> l_gcata fa fb (new l_show) () p.x)*)
      (fun _ _ -> assert false)
      (* (fa : 'self -> 'aa -> string) *)
      (* (fun s t -> s.f () t) (*fb : 'self -> 'a list -> string*) *)
  end

class ['a, 'b] maybel_show =
  object
    inherit ['a, 'b, (unit, ('a, 'b) maybel, string) a, (unit, 'a, string) a, (unit, 'b, string) a] maybel_meta_show
      (fun _ a -> a.fx ())
      (fun _ b -> b.fx ())
  end

let _ =
  let rec s x = maybel_gcata (lift string_of_int) (fun _ x -> x) (new maybel_show) () x in
  Printf.printf "Test: %s\n" @@ s (Value (Cons (1, "xxxx")));
  Printf.printf "Test: %s\n" @@ s (Var 256)

(*

(* ========================= Third application ========================= *)
type ('a, 'b) plist = ('a * 'b) list

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'i, 's, 'self, 'aa, 'bb] plist_class =
  object
    inherit ['ia * 'ib, 'a * 'b, 'sa * 'sb, 'i, 's, 'self, 'aa * 'bb] list_class
  end

let plist_coerce (x : ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'i, 's, 'self, 'aa, 'bb) #plist_class)
                 (fs : 'self' -> 'self)
                 (fa : 'aa' -> 'aa)
                 (fb : 'bb' -> 'bb) :
                 ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'i, 's, 'self', 'aa', 'bb') #plist_class =
  list_coerce x fs (fun (a, b) -> (fa a, fb b))

let plist_gcata fa fb tr i s =
 list_gcata (fun (ia, ib) (a, b) -> fa ia a, fb ib b)
    (list_coerce tr (fun x -> x) (fun a ->  let (a', b') = a.x in
                                            make a' fa, make b' fb))
    i
    s

(* Test for plugin *)
class ['a, 'b, 'self, 'aa, 'bb] plist_meta_show fa fb =
  object
    inherit ['a * 'b, 'self, 'aa * 'bb] list_meta_show (fun s (x, y) -> "(" ^ fa s x ^ ", " ^ fb s y ^ ")")
  end

class ['a, 'b] plist_show =
  object
    inherit ['a, 'b, (unit, ('a, 'b) plist, string) a, (unit, 'a, string) a, (unit, 'b, string) a] plist_meta_show
       (fun s a -> a.fx ())
       (fun s b -> b.fx ())
  end

let _ = Printf.printf "Test: %s\n" (plist_gcata (lift string_of_int) (lift (fun x -> x)) (new plist_show) () (Cons ((1, "a"), Cons ((2, "b"), Cons ((5, "c"), Nil)))))
*)


(*
(* =================== And now logic lists ============================ *)

type 'a llist = ('a, 'b) l logic as 'b

class virtual ['ia, 'a, 'sa, 'i, 's, 'self, 'aa] llist_class =
  object
    inherit ['ia, ('a, 'a llist) l, ('sa, 'sa llist) l, 'i, 's, 'self, 'aa] logic_class
  end

(* In coerce function cast the transformer that deals with left type to the transformers
  that deals with right most outer type.
  NOTE: This goes in conflict with stuff written above for previous types
*)

let rec llist_coerce (x  : ('ia, 'a, 'sa, 'i, 's, 'self, 'aa) #llist_class)
                     (fs : 'self' -> 'self)
                     (fa : 'aa'   -> 'aa) :
                     ('ia, ('a, 'a llist) l, ('sa, 'sa llist) l, 'i, 's, 'self', 'aa') #logic_class
  = logic_coerce x fs fa

let rec llist_gcata
    (fa : 'ia -> 'a -> 'sa)
    (tr : ('ia, 'a, 'sa, 'i, 's, ('i, 'a llist, 's) a, ('ia, 'a, 'sa) a) #llist_class)
    (i  : 'i)
    (s  : 'a llist) =
  logic_gcata (l_gcata fa @@ llist_gcata fa tr) (llist_coerce tr (invalid_arg "") (invalid_arg "")) i s

class ['a, 'self, 'aa, 'aaxxxx] llist_meta_show (fa : 'self -> 'aa -> string) =
  object
    inherit [ ('a, 'a llist) l, 'self, 'aaxxxx] logic_meta_show
      (fun s arg ->
        (* invalid_arg "" *)

        (* And what to write here? *)
        (* let l = new l_meta_show in *)

        let tr = new l_meta_show fa  (invalid_arg "") in
        l_gcata (invalid_arg "") (invalid_arg "")  tr () arg.x
        (* s.f () arg *)


      )

  end

class ['a] llist_show =
  object
    inherit ['a, (unit, 'a llist, string) a, (unit, 'a, string) a] llist_meta_show  (fun _ a -> a.fx ())
  end

let _ =
  let rec s () x = llist_gcata (lift string_of_int) (new llist_show) () x in
  printf "Test: %s\n" @@ s () (Value (Cons (1, Value Nil)));
  printf "Test: %s\n" @@ s () (Value (Cons (18, Value Nil)))
*)

(*
(* ========================= Second application ============================= *)
(* Yet another type constructor application *)
type ilist = int list

(* Class for instantiated type *)
class virtual ['i, 's, 'self] ilist_class =
  object
    inherit ['i, int, 's, 'i, 's, 'self, int] list_class
  end

(* Class coercion *)
let ilist_coerce (x  : ('i, 's, 'self) #ilist_class)
                 (fs : 'self' -> 'self) :
                 ('i, 's, 'self') #ilist_class =
  list_coerce x fs (fun x -> x)

(* Generic transformation function *)
let rec ilist_gcata
    (tr : ('i, 's, ('i, ilist, 's) a) #ilist_class)
    (i  : 'i)
    (s  : ilist) = list_gcata (fun x -> x) (list_coerce tr (fun x -> x) (fun a -> a.x)) i s

(* Test for plugin *)
class ['self] ilist_meta_show (*fa fb*) =
  object
    inherit [int, 'self, int] list_meta_show (fun _ -> string_of_int) (*fa fb*)
  end

class ilist_show =
  object
    inherit [(unit, ilist, string) a] ilist_meta_show (*(fun _ -> string_of_int) (fun s t -> s.f () t) *)
  end

let _ = Printf.printf "Test: %s\n" (ilist_gcata (new ilist_show) () (Cons (1, Cons (2, Nil))))
*)
