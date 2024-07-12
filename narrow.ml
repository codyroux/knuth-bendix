open Types
open Rew
open Print

let get_max_var t =
  fold_var 0 (fun i j -> Int.max i j) (fun v -> v.level) t

let freshen max t =
  var_apply (fun v -> Var { v with level = v.level + max + 1 }) t

let occurs v t =
  fold_var false (fun b c -> b || c) (fun v' -> v = v') t

let add_binding subst v t =
  (* Sometimes a new var requires recomputing the old terms *)
  let new_subst =
    VarMap.map
      (var_apply
         (fun v' -> if v = v' then t else Var v'))
      subst
  in
  VarMap.add v t new_subst

(* Can this loop on occurs check? *)
let rec unify subst t u =
  match t, u with
  | Var v, Var v' when v = v' -> Some subst
  | Var v, _ when VarMap.mem v subst ->
     unify subst (VarMap.find v subst) u
  | Var v, _ when not (occurs v u) ->
     Some (add_binding subst v u)
  | _, Var v -> unify subst u t
  | App (f, ts), App (g, us) when f = g -> unify_list subst ts us
  | Var v, _ -> None
  | App _, App _ -> None
and unify_list subst ts us =
  match ts, us with
  | [], [] -> Some subst
  | t::ts, u::us ->
     Option.bind
       (unify subst t u)
       (fun subst ->
         unify_list subst ts us)
  | _, _ -> None

let narrow_head t rule =
  let lhs = rule.r_lhs in
  let rhs = rule.r_rhs in
  Option.map
    (fun subst -> (term_subst subst rhs, subst))
    (unify VarMap.empty t lhs)

let rec crit_rule_aux l r rule2 =
  match l with
  | Var _ -> []
  | App (f, ts) ->
     let cs = List.concat_map (fun t -> crit_rule_aux t r rule2) ts in
     match narrow_head l rule2 with
     | None -> cs
     | Some (r', subst) -> (term_subst subst r, r') :: cs

(* Find all the criticial pairs of rule2 "against" rule1 *)
let crit_rule rule1 rule2 =
  let lhs1 = rule1.r_lhs in
  let rhs1 = rule1.r_rhs in
  let lhs2 = rule2.r_lhs in
  let rhs2 = rule2.r_rhs in
  let max = Int.max (get_max_var lhs1) (get_max_var rhs1) in
  let rule2 = { r_lhs = freshen max lhs2; r_rhs = freshen max rhs2 } in
  crit_rule_aux lhs1 rhs1 rule2

(* the list of all (ordered) pairs in a list *)
let list_pairs l =
  let rec list_all_pairs l1 l2 =
    match l1 with
    | [] -> []
    | x::xs -> (List.map (fun y -> (x, y)) l2) @ list_all_pairs xs l2
  in
  list_all_pairs l l

let crit_all_rules rules =
  let all_pairs = list_pairs rules in
  List.concat_map (fun (r1, r2) -> crit_rule r1 r2) all_pairs
