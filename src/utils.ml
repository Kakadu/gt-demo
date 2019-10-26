let lift f _ = f
let id x     = x

let fix0 f t =
  let knot = ref (fun _ -> assert false) in
  let recurse t = f !knot t in
  knot := recurse;
  recurse t

let fix f inh t =
  let knot = ref (fun _ -> assert false) in
  let recurse inh t = f !knot inh t in
  knot := recurse;
  recurse inh t

let show_int = string_of_int
let show_string = Printf.sprintf "%S"
let fmt_string fmt = Format.fprintf fmt "%S"
let fmt_int   fmt = Format.fprintf fmt "%d"
let fmt_float fmt = Format.fprintf fmt "%f"

let unused _ _ = failwith "*** Using the unused ***"

type ('a, 'b) ttt = {gcata : 'a; fix: 'b }

let transform1 bundle make_obj inh subj =
  let rec obj = lazy (make_obj fself)
  and fself inh x = bundle.gcata (Lazy.force obj) inh x in
  fself inh subj

let transform_gc gcata make_obj subj =
  let rec obj = lazy (make_obj fself)
  and fself x = gcata (Lazy.force obj) () x in
  fself subj


module GT = struct
  let transform_gc gcata make_obj i subj =
    let rec obj = lazy (make_obj fself)
    and fself i x = gcata (Lazy.force obj) i x in
    fself i subj

  let transform bundle  = transform_gc bundle.gcata
  let fix c = c.fix
  let lift x () = x
end
