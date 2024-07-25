type precedence = string -> string -> int
val list_to_prec : 'a list list -> 'a -> 'a -> int
val lpo : precedence -> Types.term -> Types.term -> bool
val lpo_eq : precedence -> Types.term -> Types.term -> bool
val lex_lpo : precedence -> Types.term list -> Types.term list -> bool
val var_apply : (Types.var -> Types.term) -> Types.term -> Types.term
val fold_var :
  'a -> ('a -> 'a -> 'a) -> (Types.var -> 'a) -> Types.term -> 'a
val fold_var_list :
  'a -> ('a -> 'a -> 'a) -> (Types.var -> 'a) -> Types.term list -> 'a
val lift_opt : 'a option list -> 'a list option
val first_of_list : ('a -> 'a option) -> 'a list -> 'a list option
val top_down :
  (Types.term -> Types.term option) -> Types.term -> Types.term option
val top_down_list :
  (Types.term -> Types.term option) ->
  Types.term list -> Types.term list option
val bottom_up :
  (Types.term -> Types.term option) -> Types.term -> Types.term option
val bottom_up_list :
  (Types.term -> Types.term option) ->
  Types.term list -> Types.term list option
val saturate_step : ('a -> 'b option) list -> 'a -> 'b option
val set_var_tag : int -> Types.term -> Types.term
val term_subst : Types.term Types.VarMap.t -> Types.term -> Types.term
val add_binding :
  'a Types.VarMap.t -> Types.VarMap.key -> 'a -> 'a Types.VarMap.t option
val term_match :
  Types.term Types.VarMap.t ->
  Types.term -> Types.term -> Types.term Types.VarMap.t option
val apply_head : Types.rule -> Types.term -> Types.term option
val saturate : ('a -> 'a option) list -> 'a -> 'a
val saturate_n : int -> ('a -> 'a option) list -> 'a -> 'a
val norm_step : Types.rule list -> Types.term -> Types.term option
val norm : Types.rule list -> Types.term -> Types.term
