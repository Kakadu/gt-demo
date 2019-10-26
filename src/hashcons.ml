module H :
sig
  type t

  val hc : t -> 'a -> t * 'a
  val create : unit -> t
end =
  struct
    module H = Hashtbl.Make (struct
        type t = Obj.t
        let hash = Hashtbl.hash
        let equal new_ old =
          (* Printf.printf "%s %d\n%!" __FILE__ __LINE__; *)
          if Hashtbl.hash new_ <> Hashtbl.hash old
          then false
          else if Obj.(tag @@ repr new_) <> Obj.(tag @@ repr old)
          then false
          else if Obj.(size @@ repr new_) <> Obj.(size @@ repr old)
          then false
          else
            List.fold_left (fun acc n ->
                (Obj.field (Obj.repr new_) n == Obj.field (Obj.repr old) n) && acc
              ) true (List.init Obj.(size @@ repr old) (fun n -> n))

      end)
    type t = Obj.t H.t

    let create () = H.create 37
    let hc h x =
      (* Printf.printf "%s %d\n%!" __FILE__ __LINE__; *)
      if Obj.(is_int @@ repr x) then (h,x)
      else
        let o = Obj.repr x in
        try
          let old = Obj.magic @@ H.find h o in
          print_endline "use old value";
          (h,old)
        with Not_found -> H.add h o o;
          print_endline "use new value";
          h, x
  end

type h = {hc : 'a . H.t -> 'a -> H.t * 'a}

let hf = {hc = H.hc}

type expr = Var of string | Const of int | Binop of expr * expr

let rec hc h = function
| Var    x        -> let h', x' = hf.hc h x in
                     hf.hc h' (Var x')

| Const  b        -> let h', b' = hf.hc h b in
                     hf.hc h' (Const b')

| Binop (l, r)    -> let h'', l' = hc h l in
                     let h''', r' = hc h'' r in
                     hf.hc h''' (Binop (l', r'))


let h = H.create ()
let (h,e1) = hc h (Const 10)
let (h,e2) = hc h (Const 10)

let h = H.create ()
let (_,e3) =
  print_endline  "e3";
  hc h @@ Binop (Binop (e2,e2), Binop (e2,e2))
