open Printf
open Types
open Print
open Parse
open Rew
open Narrow

(* E + {s = s}, R  ~> E, R *)
let delete trs =
  let eqs = List.filter (fun e -> e.eq_lhs <> e.eq_rhs) trs.eqns in
  if List.length eqs <> List.length trs.eqns then
    Some { trs with eqns = eqs }
  else None

(* E, R + {s -> t} ~> E, R + {s -> u} if t ->* u *)
let compose trs =
  let rules = trs.rules in
  let comp r =
    let rhs = r.r_rhs in
    let rhs = norm_step rules rhs in
    Option.map (fun rhs -> { r with r_rhs = rhs }) rhs
  in
  let rules = first_of_list comp rules in
  Option.map (fun rules ->
      { trs with rules = rules })
    rules

(* E + {s = t}, R ~> E + {s = u}, R if t ->* u *)
let simplify trs =
  let rules = trs.rules in
  let eqns = trs.eqns in
  let rec simp e =
    let lhs = e.eq_lhs in
    let rhs = e.eq_rhs in
    match norm_step rules lhs, norm_step rules rhs with
    | Some lhs, Some rhs -> Some { eq_lhs = lhs; eq_rhs = rhs }
    | Some lhs, None -> Some { eq_lhs = lhs; eq_rhs = rhs }
    | None, Some rhs -> Some { eq_lhs = lhs; eq_rhs = rhs }
    | None, None -> None
  in
  let eqns = first_of_list simp eqns in
  Option.map (fun eqns ->
      { trs with eqns = eqns })
    eqns

(* We keep a global precedence *)
let global_prec = ref (fun f g -> 0) (* This is the most permissive
                                        precendence, it only strictly
                                        smaller rhs *)

let remove_first f l =
  let rec remove_first_acc l acc =
    match l with
    | [] -> None
    | e::es ->
       match f e with
       | None -> remove_first_acc es (e::acc)
       | Some r -> Some (r, List.rev_append acc es)
  in
  remove_first_acc l []

(* E + {s = t}, R ~> E, R + {s -> t} if s > t *)
let orient trs =
  let eqns = trs.eqns in
  let orient e =
    let lhs = e.eq_lhs in
    let rhs = e.eq_rhs in
    if lpo !global_prec e.eq_lhs e.eq_rhs then
      Some { r_lhs = lhs; r_rhs = rhs }
    else if lpo !global_prec e.eq_rhs e.eq_lhs then
      Some { r_lhs = rhs; r_rhs = lhs }
    else None
  in
  match remove_first orient eqns with
  | None -> None
  | Some (r, eqns) ->
     Some { trs with eqns = eqns; rules = r::trs.rules }

(* E, R + {s -> t} ~> E + {v = t}, R if s "collapses to" v, that is,
   there is a rule l -> r that reduces s but s does not match any
   subterm of l.  see
   https://homepage.divms.uiowa.edu/~astump/papers/thesis-wehrman.pdf
*)
let collapse trs =
  let rules = trs.rules in
  (* get the first rule that fires along with the reduction *)
  let rec red_first rs t =
    match rs with
    | [] -> None
    | r::rs ->
       match top_down (apply_head r) t with
       | Some u -> Some (r, u)
       | None -> red_first rs t
  in
  let col r =
    let lhs = r.r_lhs in
    match red_first rules lhs with
    | None -> None
    | Some (r', v) ->
       (* this is the collapse side condition *)
       if Option.is_none (top_down (apply_head r) r'.r_lhs) then
         Some { eq_lhs = v; eq_rhs = r.r_rhs }
       else None
  in
  match remove_first col rules with
  | None -> None
  | Some (eq, rules) ->
     Some { trs with eqns = eq::trs.eqns; rules = rules }


(* E, R ~> E + {s = t}, R if s <- . -> t is a critical pair of R,
   which is not joinable. *)
let deduce trs =
  let rules = trs.rules in
  let crits = crit_all_rules rules in
  let crits = filter_all rules crits in
  match crits with
  | [] -> None
  | _ ->
     let new_eqns =
       List.map (fun (t, u) -> { eq_lhs = t; eq_rhs = u }) crits
     in
     Some { trs with eqns = trs.eqns @ new_eqns }

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
  global_prec := test_prec;

  let trs = saturate [delete; simplify; orient; compose; deduce; collapse] trs in
  print_trs stdout trs;
  ()
