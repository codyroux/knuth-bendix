open Printf
open Types
open Print
open Parse

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

module Var =
  struct
    type t = bool * string
    let compare (b1, s1) (b2, s2) =
      match Bool.compare b1 b2 with
      | 0 -> String.compare s1 s2
      | x -> x
  end

module VarMap = Map.Make(Var)

let rec var_apply map t =
  match t with
  | Var (b, s) -> map b s
  | App (f, ts) -> App (f, List.map (var_apply map) ts)

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

let set_var_tag b t =
  let apply _ s =
    Var (b, s)
  in
  var_apply apply t

let term_subst subst t =
  let apply b s =
    if VarMap.mem (b, s) subst then VarMap.find (b, s) subst
    else Var (b, s)
  in
  var_apply apply t

let rec term_match subst p t =
  match p, t with
  | Var (b, s), _ ->
     begin
       match VarMap.find_opt (b, s) subst with
       | None -> Some (VarMap.add (b, s) t subst)
       | Some u ->
          if t = u then Some subst
          else None
     end
  | App (f, ts), App (g, us) when f = g ->
     begin
       try
         List.fold_left2
           (fun substOpt p t ->
             match substOpt with
             | None -> None
             | Some subst -> term_match subst p t
           )
           (Some VarMap.empty)
           ts
           us
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
  
(* E + {s = s}, R  ~> E, R *)
let delete trs =
  let eqs = List.filter (fun e -> e.eq_lhs <> e.eq_rhs) trs.eqns in
  if List.length eqs <> List.length trs.eqns then
    Some { trs with eqns = eqs }
  else None

(* E, R + {s -> t} ~> E, R + {s -> u} if t ->* u *)
let compose trs = assert false

(* E + {s = t}, R ~> E + {s = u}, R if t ->* u *)
let simplify trs = assert false

(* E + {s = t}, R ~> E, R + {s -> t} if s > t *)
let orient trs = assert false

(* E, R + {s -> t} ~> E + {v = t}, R if s "collapses to" v
   see https://homepage.divms.uiowa.edu/~astump/papers/thesis-wehrman.pdf
*)
let collapse trs = assert false

(* E, R ~> E + {s = t}, R if s <- . -> t is a critical pair of R*)
let deduce trs = assert false

(* opportunities for optimization here *)
let rec saturate step_funs trs =
  (* we assume step_funs are in rough order of cost *)
  let rec saturate_step step_funs =
    match step_funs with
    | f::fs ->
       begin
         match f trs with
         | None -> saturate_step fs
         | Some trs' -> Some trs'
       end
    | [] -> None
  in
  match saturate_step step_funs with
  | None -> trs
  | Some trs' -> saturate step_funs trs'

let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
(* Run with: *)
(* ocamlbuild -r main.native && ./main.native *)
let () =
  glob_in := read_whole_file "group.trs";
  glob_cursor := 0;
  let trs = parse_spec () in
  print_trs stdout trs;
  (* We magically know that this order will do *)
  let test_prec = list_to_prec [["1"];["m"];["i"]] in
  for i = 0 to List.length trs.eqns - 1 do
    let eq = List.nth trs.eqns i in
    let lhs = eq.eq_lhs in
    let rhs = eq.eq_rhs in
    let is_gt = lpo test_prec lhs rhs in
    printf "%a > %a   %B\n" print_term lhs print_term rhs is_gt;
  done;
  let trs = saturate [delete] trs in
  print_trs stdout trs;
  glob_in := "f(X,Y) -> g(Y,X)";
  glob_cursor := 0;
  let vars = ["X"; "Y"] in
  let rule = parse_rule vars () in
  printf "%a\n" print_rule rule;
  glob_in := "f(f(a,b),f(c,d))";
  glob_cursor := 0;
  let term = parse_term vars () in
  printf "%a\n" print_term term;
  let term0 = apply_head rule term in
  let bottom_up = bottom_up (apply_head rule) in
  let term1 = bottom_up term in
  let top_down = top_down (apply_head rule) in
  let term2 = top_down term in
  let rec repeat f t =
    match f t with
    | None -> t
    | Some t' -> repeat f t'
  in
  printf "%a\n" print_term (Option.get term0);
  printf "%a\n" print_term (Option.get term1);
  printf "%a\n" print_term (Option.get term2);
  printf "%a\n" print_term (repeat bottom_up term);
  printf "%a\n" print_term (repeat top_down term);
  ()
