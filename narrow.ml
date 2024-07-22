open Types
open Rew
open Print

type zip_loc =
  { sym : string;
    left_rev : term list;
    (* here is the term! *)
    right : term list;
    parent : term_zip }
and term_zip =
  | Here
  | There of zip_loc

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

(* "close" a zipper z with t in the hole, applying the substitution as
   you go up *)
let rec zip_with subst z t =
  match z with
  | Here -> t
  | There {sym; left_rev; right; parent} ->
     let left = List.map (term_subst subst) left_rev in
     let right = List.map (term_subst subst) right in
     let t = App (sym, List.rev_append left (t::right)) in
     zip_with subst parent t 

let rec splay z t =
  match t with
  | Var _ -> []
  | App (f, ts) ->
     let here = (z, t) in
     let z = { sym = f; left_rev = []; right = []; parent = z } in
     let there = splay_list z ts in
     here :: there
and splay_list z ts =
  match ts with
  | [] -> []
  | t::ts ->
     let hd = splay (There {z with right = ts }) t in
     let tail = splay_list {z with left_rev = t::z.left_rev } ts in
     hd @ tail
     

let crit_rule_aux l r rule2 =
  assert false

(* Find all the criticial pairs of rule2 "against" rule1 *)
let crit_rule rule1 rule2 =
  assert false

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
