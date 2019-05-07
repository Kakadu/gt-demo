open Utils
open Printf

module L : sig

  type 'a list = [ `Nil  | `Cons of ('a * 'a list) ]

  class virtual ['ia,'a,'sa,'inh,'extra,'syn] list_t :
    object
      method  virtual c_Nil  : 'inh -> 'extra -> 'syn
      method  virtual c_Cons : 'inh -> 'extra -> 'a -> 'a list -> 'syn
    end
  val gcata_list :
    (_,'a,'sa,'inh, [< 'a list] as 'e, 'syn)#list_t -> 'inh -> 'e -> 'syn
  class ['a, 'a_2, 'e, 's] gmap_list_t :
    (unit -> 'a -> 'a_2) ->
    (unit -> 'e -> 's) ->
    object
      constraint 'e = [> 'a list]
      constraint 's = [> `Cons of 'a_2 * 's | `Nil ]
      method c_Cons : unit -> 'e -> 'a -> 'a list -> 's
      method c_Nil  : unit -> 'e -> 's
    end

  val gmap_list :
    (unit -> 'a -> 'b) ->
    unit -> 'a list -> ([> `Cons of 'b * 'c | `Nil ] as 'c)

end = struct
  type 'a list = [`Nil | `Cons of 'a * 'a list ]
  class virtual ['ia,'a,'sa, 'inh,'extra,'syn] list_t =
    object
      method virtual  c_Nil  : 'inh -> 'extra -> 'syn
      method virtual  c_Cons : 'inh -> 'extra -> 'a -> 'a list -> 'syn
    end

  let gcata_list (tr : (_,_,_,_,_, _)#list_t) inh subj =
    match subj with
    | `Nil -> tr#c_Nil inh subj
    | `Cons (___001_, ___002_) -> tr#c_Cons inh subj ___001_ ___002_

  class ['a,'a_2,'extra,'s] gmap_list_t fa (fself_list: unit -> 'extra -> 's) =
    object
      inherit  [unit,'a,'a_2, unit,'extra,'s] list_t
      constraint 'extra = [> 'a list ]
      constraint 's = [> `Cons of 'a_2 * 's | `Nil ] as 's
      method c_Nil  () _  = `Nil
      method c_Cons () _ x xs =
        `Cons (fa () x, fself_list () (match xs with #list as xs -> xs))
    end
  let gmap_list fa inh0 subj =
    GT.transform_gc gcata_list ((new gmap_list_t) fa) inh0 subj

    (* class ['a,'a_2, 'extra] show_list_t fa fself =
     *   object
     *     inherit  [unit,'a,'a_2, unit,'extra,string] list_t
     *     method c_Nil inh___003_ _ = "`Nil"
     *     method c_Cons inh___004_ _ x xs =  Printf.sprintf "`Const (%a, %a)" fa x fself xs
     *   end
     * let show_list fa inh0 subj =
     *   GT.transform_gc gcata_list ((new show_list_t) fa) inh0 subj *)

    (* let () =
     *   print_endline @@ show_list (fun () -> Printf.sprintf "%d") () (`Cons (1, `Nil)) *)
end

(* module L2 = struct
 *   type 'a t = [`T | 'a L.list]
 *   class virtual ['ia,'a,'sa, 'inh,'extra,'syn] t_t =
 *     object
 *       inherit ['ia,'a,'sa, 'inh,'extra,'syn] L.list_t
 *       method virtual  c_T  : 'inh -> 'a t -> 'syn
 *     end
 *   class ['a,'a_2,'extra,'syn] gmap_t_t fa (fself: unit -> 'extra -> 'syn) =
 *     object
 *       inherit  [unit,'a,'a_2, unit,'extra,'syn] t_t
 *       inherit  ['a,'a_2, 'extra,'syn] L.gmap_list_t fa fself
 *       method c_T inh___003_ _ = `T
 *     end
 *     class ['a,'a_2, 'extra] show_t_t fa fself =
 *       object
 *         inherit  [unit,'a,'a_2, unit,'extra,string] t_t
 *         inherit  ['a,'a_2, 'extra] L.show_list_t fa fself
 *         method c_T inh___003_ _ = "`T"
 *       end
 *
 * end *)

module L3 = struct
  type t = [ `L3 ]
  class virtual ['inh,'extra,'syn] t_t =
    object
      method virtual c_L3  : 'inh -> 'extra -> 'syn
    end
  let gcata_t (tr : (_,_,_)#t_t) inh subj =
    match subj with
    | `L3 -> tr#c_L3 inh subj

  class ['extra,'s] gmap_t_t (fself_list: unit -> 'extra -> 's) =
    object
      inherit  [unit,'extra,'s] t_t
      constraint 'extra = [> t ]
      constraint 's = [> t ]
      method c_L3  () _  = `L3
    end
    let gmap_t inh0 subj =
      GT.transform_gc gcata_t (new gmap_t_t) inh0 subj
  end

module Sum = struct
  type 'a t = [ L3.t | 'a L.list]
  class virtual ['ia,'a,'sa, 'inh,'extra,'syn] t_t =
    object
      inherit ['ia,'a,'sa, 'inh,'extra,'syn] L.list_t
      inherit ['inh,'extra,'syn] L3.t_t
    end

  let gcata_t tr inh subj =
    match subj with
    | #L.list as xs -> L.gcata_list tr inh xs
    | #L3.t as l -> L3.gcata_t tr inh l

  class ['a,'a_2,'extra,'s] gmap_t_t fa (fself: unit -> 'extra -> 's) =
    object
      constraint 'extra = [> 'a t ]
      constraint 's = [> 'a_2 t ]
      inherit  [unit,'a,'a_2, unit, L3.t, L3.t] t_t
      inherit ['a,'a_2, 'a L.list, 'a_2 L.list] L.gmap_list_t fa
          (fself: (unit -> 'extra -> 's) :> (unit -> 'a L.list -> 's))
      inherit ['extra,'s] L3.gmap_t_t fself
    end
  let gmap_list fa inh0 subj =
    GT.transform_gc gcata_t (new gmap_t_t fa) inh0 subj

end
