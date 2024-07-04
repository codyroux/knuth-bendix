open Types
open Rew

let get_max_var t =
  fold_var 0 (fun i j -> Int.max i j) (fun v -> v.level) t

let freshen max t =
  var_apply (fun v -> Var { v with level = v.level + max + 1 }) t

let occurs v t =
  fold_var false (fun b c -> b || c) (fun v' -> v = v')

(* Can this loop on occurs check? *)
let rec unify subst t u =
  match t, u with
  | Var v, Var v' when v = v' -> Some VarMap.empty
  | Var v, _ when VarMap.mem v subst ->
     unify subst (VarMap.find v subst) u
  | Var v, _ -> Some (VarMap.add v u subst)
  | _, Var v -> unify subst u t
  | App (f, ts), App (g, us) when f = g -> unify_list subst ts us
  | App _, App _ -> None
and unify_list subst ts us =
  match ts, us with
  | [], [] -> Some VarMap.empty
  | t::ts, u::us ->
     Option.bind
       (unify subst t u)
       (fun subst ->
         unify_list subst ts us)
  | _, _ -> None
