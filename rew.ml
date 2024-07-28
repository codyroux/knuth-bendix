open Types

type precedence = string -> string -> int

(* Turns a list into a precedence, (a well-founded partial order on
   function symbols) e.g. the precendence [["a"; "b"]; ["f"; "g"];
   ["h"]] denotes a ~ b < f ~ g < h *)
let list_to_prec ll =
  fun s1 s2 ->
  let i1 = List.find_index (List.mem s1) ll in
  let i2 = List.find_index (List.mem s2) ll in
  match i1, i2 with
  | Some i1, Some i2 -> compare i1 i2
  | _ -> raise (Invalid_argument "symbol not found in precedence list")

let rec lpo (prec : precedence) t1 t2 =
  match t1 with
  | Var _ -> false
  | App (f, ts) ->
     List.exists (fun ti -> lpo_eq prec ti t2) ts
     || begin
        match t2 with
        (* Handled by the lpo_eq case*)
        | Var _ -> false
        | App (g, us) ->
           if prec g f < 0 then
             List.for_all (fun ui -> lpo prec t1 ui) us
           else if prec g f = 0 then (* hard case! *)
             lex_lpo prec ts us
           else false
        end
and lpo_eq prec t1 t2 =
  t1 = t2
  || lpo prec t1 t2
and lex_lpo prec ts us =
  match ts, us with
  | [], _ -> false
  | _, [] -> false
  | t::ts, u::us ->
     if lpo prec t u then true
     else if t = u then lex_lpo prec ts us
     else false

let rec var_apply map t =
  match t with
  | Var v -> map v
  | App (f, ts) -> App (f, List.map (var_apply map) ts)

let rec fold_var base merge var t =
  match t with
  | Var v -> var v
  | App (_, ts) -> fold_var_list base merge var ts
and fold_var_list base merge var ts =
  match ts with
  | [] -> base
  | t::ts ->
     let vt = fold_var base merge var t in
     let vts = fold_var_list base merge var ts in
     merge vt vts

let rec lift_opt l =
  match l with
  | [] -> Some []
  | x::xs ->
     match x, lift_opt xs with
     | Some x, Some xs -> Some (x::xs)
     | _, _ -> None

(* Returns the list with the "first rewrite" applied if any, and None
   otherwise *)
let rec first_of_list rew ts =
    match ts with
    | [] -> None
    | t::ts ->
       match rew t with
       | Some t -> Some (t::ts)
       | None -> Option.map (fun ts -> t::ts) (first_of_list rew ts)

(* Applies a "single pass" of top down rewrites, which stops recursing
   as soon as a rule fires *)
let rec top_down rew t =
  match t with
  | Var _ -> None (* no rewrites at variables! *)
  | App (f, ts) ->
     match rew t with
     | None ->
        let ts = top_down_list rew ts in
        Option.map (fun ts -> App (f, ts)) ts
     | Some t' -> Some t'
and top_down_list rew ts =
  (* Succeed if at least one rewrite succeeds *)
  first_of_list (top_down rew) ts

let rec bottom_up rew t =
  match t with
  | Var _ -> None (* no rewrites at variables! *)
  | App (f, ts) ->
     let ts = bottom_up_list rew ts in
     match ts with
     | None -> rew t
     | Some ts' -> Some (App (f, ts'))
and bottom_up_list rew ts =
  first_of_list (bottom_up rew) ts

let rec saturate_step step_funs x =
  match step_funs with
  | f::fs ->
     begin
       match f x with
       | None -> saturate_step fs x
       | Some trs' -> Some trs'
     end
  | [] -> None

let set_var_tag i t =
  let apply v =
    Var { v with level = i }
  in
  var_apply apply t

let term_subst subst t =
  let apply v =
    if VarMap.mem v subst then VarMap.find v subst
    else Var v
  in
  var_apply apply t

let add_binding subst v t =
  match VarMap.find_opt v subst with
  | None -> Some (VarMap.add v t subst)
  | Some u ->
     if t = u then Some subst
     else None  

let rec term_match subst p t =
  match p, t with
  | Var v, _ -> add_binding subst v t
  | App (f, ps), App (g, ts) when f = g ->
     begin
       try
         List.fold_left2
           (fun substOpt p t ->
             match substOpt with
             | None -> None
             | Some subst -> term_match subst p t
           )
           (Some subst)
           ps
           ts
       with
       (* FIXME: Potential footgun*)
       | Invalid_argument _ -> None
     end
  | _ -> None


let apply_head rule t =
  let lhs = rule.r_lhs in
  let rhs = rule.r_rhs in
  match term_match VarMap.empty lhs t with
  | None -> None
  | Some sigma -> Some (term_subst sigma rhs)


(* opportunities for optimization here *)
let rec saturate step_funs trs =
  match saturate_step step_funs trs with
  | None -> trs
  | Some trs' -> saturate step_funs trs'

(* opportunities for optimization here *)
let rec saturate_n n step_funs trs =
  match saturate_step step_funs trs with
  | None -> trs
  | Some trs' ->
     if n <= 0 then trs'
     else saturate_n (n-1) step_funs trs'

(* Try applying all rules recursively until one fires *)
let norm_step rules t =
  let all_rules = List.map (fun r -> top_down (apply_head r)) rules in
  saturate_step all_rules t

(* normalize a term wrt rules top-down *)
let norm rules t =
  let all_rules = List.map (fun r -> top_down (apply_head r)) rules in
  saturate all_rules t

(* A very rough attempt to guess an LPO precedence: basically try to take the equations left-to-right, and order the head symbol greater than the right symbols if possible, or equivalent if possible, otherwise incomparable. *)

module FSet = Set.Make(String)

let rec all_syms t =
  match t with
  | Var _ -> FSet.empty
  | App (f, ts) ->
     let all_ts =
       List.fold_left
         (fun s t -> FSet.union s @@ all_syms t)
         FSet.empty ts
     in
     FSet.add f all_ts

let reaches f eqn =
  match eqn.eq_lhs with
  | Var _ -> FSet.empty
  | App (g, _) when g = f ->
     all_syms eqn.eq_rhs
  | _ -> FSet.empty

let reaches_all f eqns =
  List.fold_left
    (fun s e -> FSet.union s @@ reaches f e)
    FSet.empty eqns

let get_all_syms vars eqns =
  let vars = FSet.of_list vars in
  let all_syms =
    List.fold_left
      (fun s e ->
        FSet.union s @@
          FSet.union
            (all_syms e.eq_lhs)
            (all_syms e.eq_rhs))
      FSet.empty eqns
  in
  FSet.to_list @@ FSet.diff all_syms vars


module SMap = Hashtbl.Make(String)

(* Just stolen from
   https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm *)
(* The vertices here are just symbol names, and the edges are of the
   form f -> g if f(t1,..., tn) == u and g occurs in u, for some
   equation in the list. *)
let tarjan vertices eqns =
  let index = ref 0 in
  let stack = ref [] in
  let sccs = ref [] in
  let v_index = SMap.create (List.length vertices) in
  let lowlink = SMap.create (List.length vertices) in
  let on_stack = SMap.create (List.length vertices) in
  let rec strong_connect v =
    SMap.add v_index v !index;
    SMap.add lowlink v !index;
    incr index;
    stack := v :: !stack;
    SMap.add on_stack v ();
    let next = FSet.to_list @@ reaches_all v eqns in
    for i = 0 to List.length next - 1 do
      let w = List.nth next i in
      if not @@ SMap.mem v_index w then
        strong_connect w
      else if SMap.mem on_stack w then
        let v_low = SMap.find lowlink v in
        let w_index = SMap.find v_index w in
        SMap.add lowlink v (min v_low w_index);
    done;
    if SMap.find lowlink v = SMap.find v_index v then
      let new_scc = ref [] in
      while List.hd !stack <> v do
        let w = List.hd !stack in
        new_scc := w :: !new_scc;
        SMap.remove on_stack w;
        stack := List.tl !stack;
      done;
      let w = List.hd !stack in
      new_scc := w :: !new_scc;
      SMap.remove on_stack w;
      stack := List.tl !stack;
      sccs := !new_scc :: !sccs;
  in
  let rec go vertices =
    match vertices with
    | [] -> !sccs
    | v :: vs -> 
       if not @@ SMap.mem v_index v then
         strong_connect v;
       go vs
  in
  go vertices
