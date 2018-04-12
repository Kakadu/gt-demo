(* Opening runtime module to avoid explicit qualification *)

(* Set/map of strings *)
module S = Set.Make (String)
module M = Map.Make (String)

(* Name generator for alpha-conversion *)
let generator s =
  let s = ref s in
  let rec gen x =
    let n = x ^ "'" in
    if S.mem n !s
    then gen n
    else (s := S.add n !s; n)
  in gen

(* Helper class for alpha-conversion; parameterized
   by name-generating function ``g'' and a set of ``prohibited''
   free variables ``fvs''
*)
class substitutor gen fvs =
  object (this)
    val s = (M.empty : string M.t)
    method subst  x = try M.find x s with Not_found -> x
    method rename x = if S.mem x fvs
                      then let x' = gen x in x', {< s = M.add x x' s >}
                      else x, {< >}
  end

(* Type of lambda expression; enables ``show'' and ``foldl'' transformations *)
type lam =
| Var of string
| App of lam * lam
| Abs of string * lam
(* [@@deriving gt ~show ~foldl] *)

let gcata_lam tr inh = function
  | Var s     -> tr#c_Var inh s
  | App (l,r) -> tr#c_App inh l r
  | Abs (s,l) -> tr#c_Abs inh s l

class virtual ['inh, 'syn] class_lam = object
  method virtual c_Var : 'inh -> string -> 'syn
  method virtual c_App : 'inh -> lam -> lam -> 'syn
  method virtual c_Abs : 'inh -> string -> lam -> 'syn
end

class show_lam = object (self)
  inherit ['inh, 'syn] class_lam
  constraint 'inh = unit
  constraint 'syn = string
  method c_Var _ s = s
  method c_App _ l r =
    Printf.sprintf "(%s) (%s)" (gcata_lam self () l)  (gcata_lam self () r)
  method c_Abs _ s l = Printf.sprintf "(fun %s -> %s)" s (gcata_lam self () l)
end

class ['syn] foldl_lam = object (self)
  inherit ['inh, 'syn] class_lam
  constraint 'inh = 'syn
  method c_Var acc _ = acc
  method c_App acc _ _ = acc
  method c_Abs acc _ _ = acc
end


(* ``show'' function *)
let show l = gcata_lam (new show_lam) () l

(* Transformation class to collect variables; reuses ``foldl'' *)
class var = object
  inherit [S.t] foldl_lam
  method c_Var s x = S.add x s
end

(* let transform_lam = (let (module Op) = lam in Op.gcata) *)

(* Function to collect variable names *)
let rec vars (l: lam) = gcata_lam (new var) S.empty l

(* Context --- function to generate ``fresh'' names *)
let context l = generator (vars l)

(* Transformation class to collect free variables; reuses ``var'' *)
let fv =
  gcata_lam
    (object(self)
      inherit var
      method c_Lam s x l = S.union s (S.remove x (gcata_lam self S.empty l ))
    end)
    S.empty

(* Substitution function (generic as well) *)
let subst g x new_term =
  gcata_lam
    (object(self)
      inherit [substitutor, lam] class_lam
      method c_Var s y =
        if y = x then new_term else Var (s#subst y)
      method c_Abs s y l =
        if y = x
        then gcata_lam self s l
        else
          let y', s' = s#rename y in
          Abs (y', gcata_lam self s l)
      method c_App s l r = App (gcata_lam self s l, gcata_lam self s r)
    end)
    (new substitutor g (fv new_term))

(* Module type to abstract base class for implementing reduction orders *)
module type Reducer =
  sig

    (* Abstract type for inherited attribute *)
    type context
    (* Shortcut for augmented type of lambda-expression *)
    type aug    = lam
    (* Shortcut for type of supplementary methods *)
    type mtype  = context -> aug -> lam

    (* Abstract function to provide ``default'' inherited attribute *)
    val default : lam -> context

    (* Template base class for reduction trnsformation *)
    class virtual reducer :
      object inherit [context, lam] class_lam
        method virtual arg       : mtype
        method virtual subst_arg : mtype
        method         head      : mtype
        method         c_Var     : context ->  string -> lam
        method         c_App     : context ->  aug -> aug -> lam
      end

    (* Template class for the only trait which is sensitive to
       the ``context'' type
    *)
    class virtual reduce_under_abstractions :
      object inherit reducer
      method c_Abs : context -> string -> aug -> lam
    end

  end

(* Functor to implement concrete reduction orders *)
module Reductions (R : Reducer) =
  struct
    (* Opening R to avoid qualifications *)
    open R

    (* Top-level reduction function: applies reduction
       order ``r'' to lambda-term ``l''
    *)
    let reduce r l = r (default l) l

    (* Basic reduction order traits *)
    class virtual dont_reduce_under_abstractions = object(self)
      inherit reducer
      method c_Abs _ name body = Abs (name, body)
    end

    class virtual reduce_arguments = object(self)
      inherit reducer
      method arg c l = gcata_lam self c l
    end

    class virtual dont_reduce_arguments = object
      inherit reducer
        method arg _ l = l
      end

    class virtual non_strict =
      object
        inherit reducer
        method subst_arg _ m = m
      end

    class virtual strict = object (self)
      inherit reducer
      method subst_arg c m = gcata_lam self c m
    end

    (* Reduction orders *)
    class call_by_name = object
      inherit dont_reduce_under_abstractions
      inherit dont_reduce_arguments
      inherit non_strict
    end

    let bn = gcata_lam (new call_by_name)

    class normal = object
      inherit reduce_under_abstractions
      inherit reduce_arguments
      inherit non_strict
      method  head c x = bn c x
    end
    let nor = gcata_lam (new normal)

    class call_by_value = object
      inherit dont_reduce_under_abstractions
      inherit reduce_arguments
      inherit strict
    end
    let bv = gcata_lam (new call_by_value)

    class applicative = object
      inherit call_by_value
      inherit reduce_under_abstractions
    end
    let ao = gcata_lam (new applicative)

    class hybrid_applicative = object
      inherit applicative
      method head c x = bv c x
    end
    let ha = gcata_lam (new hybrid_applicative)

    class head_spine = object
      inherit call_by_name
      inherit reduce_under_abstractions
    end

    let he = gcata_lam (new head_spine)

    class hybrid_normal = object
      inherit normal
      method  head c x = he c x
    end
    let hn = gcata_lam (new hybrid_normal)

    (* Top-level definitions *)
    let sample r l =
      Printf.printf "%s ----> %s\n" (show l) (show (r l))

    let main () =
      let run n r =
        Printf.printf "\n========== %s ================\n\n" n;
        List.iter (sample r) [
          Abs ("x", App (Abs ("y", Var "y"), Var "z"));
          App (Abs ("x", App (Abs ("y", Var "y"), Var "z")), Var "y");
          App (Var "x", App (Abs ("x", Var "x"), Var "y"));
          App (Abs ("x", App (Var "x", Var "y")), App (Abs ("x", Var "x"), Var "y"));
          App (Abs ("x", App (Var "y", Var "x")), App (Abs ("x", Var "x"), Var "y"));
        ]
      in
      run "Call-by-name"        (reduce bn);
      run "Normal Order"        (reduce nor);
      run "Call-by-value"       (reduce bv);
      run "Applicative"         (reduce ao);
      run "Hybrid Applicative"  (reduce ha);
      run "Head Spine"          (reduce he);
      run "Hybrid Normal Order" (reduce hn)

  end

(* Top-level definition *)
let _ =
  (* Simple case --- reduction with no context tracing *)
  let module Simple = Reductions (
    struct
      (* Inherited attribute: name-generation function *)
      type context = string -> string
      type aug    = lam
      type mtype  = context -> aug -> lam

      let default = context

      (* Base reducer for the simple case *)
      class virtual reducer =
        object(self)
          inherit [context, lam] class_lam
          method virtual arg       : mtype
          method virtual subst_arg : mtype
          method         head      : mtype = fun c x -> gcata_lam self c x
          method c_Var _ s   = (Var s)
          method c_App c l m =
            match self#head c l with
            | Abs (x, l') -> gcata_lam self c (subst c x (self#subst_arg c m) l')
            | l'          -> let l'' = gcata_lam self c l' in
                             App (l'', self#arg c m)
         end

      (* Context-type-sensitive trait for the simple case *)
      class virtual reduce_under_abstractions = object(self)
        inherit reducer
        method c_Abs c x l = Abs (x, gcata_lam self c l)
      end
    end
  )
  in
  (* Advanced case with context reconstruction; the definitions
     of all but one trait and all reduction orders completely reused
  *)
  let module WithContext = Reductions (
    struct
      (* Inherited attribute: name-generating function and term with a hole *)
      type context = (string -> string) * (lam -> lam)
      type aug     = lam
      type mtype   = context -> aug -> lam

      (* Combinators to manipulate terms with holes *)
      let (@@) f g x = f (g x)
      let id   x     = x
      let abst x  e  = Abs (x, e)
      let appl e1 e2 = App (e1, e2)
      let appr e2 e1 = App (e1, e2)

      let default l = (context l, id)

      (* Base reducer with context reconstruction *)
      class virtual reducer = object(self)
        inherit [context, lam] class_lam
        method virtual arg       : mtype
        method virtual subst_arg : mtype
        method         head      : mtype = fun c x -> gcata_lam self c x
        method c_Var _ s = Var s
        method c_App ((g, c) as i) l m =
            match self#head (g, c @@ appl l) l with
            | Abs (x, l') -> gcata_lam self i (subst g x (self#subst_arg (g, c @@ appr l') m) l')
            | l'          -> let l'' = gcata_lam self  (g, c @@ appl l) l' in
                             App (l'', self#arg (g, c @@ appr l'') m)
         end

      (* Context-type-sensitive trait with context reconstruction *)
      class virtual reduce_under_abstractions = object(self)
        inherit reducer
        method c_Abs (g, c) x l = Abs (x, gcata_lam self (g, c @@ abst x) l)
      end

    end
  )
  in
  (* Running both cases *)
  Simple.main ();
  WithContext.main ()
