type ('nonterminal, 'terminal) symbol = 
| T of 'terminal 
| N of 'nonterminal

let rec convert_grammar_func rules = 
  match rules with
    | [] -> ( function | _ -> [] )
    | h::t ->
      let f = convert_grammar_func t and h1 = fst h and h2 = snd h in
      if f h1 = [] 
      then (function sym -> if sym = h1 then [h2] else f sym)
      else (function sym -> if sym = h1 then (h2::f h1) else f sym)
;;

let convert_grammar g = 
((fst g), convert_grammar_func (snd g))
;;

let rec match_and lsym rules_f accept deri frag = 
  match lsym with 
  | [] -> (accept deri frag)
  | symh::symt ->   if frag = [] then None else
    match symh with 
    | T ts -> 
       if ts = List.hd frag 
       then match_and symt rules_f accept deri (List.tl frag)
       else None
    | N nts -> match_or nts (rules_f nts) rules_f (match_and symt rules_f accept) deri frag

and match_or start_sym l_lsym rules_f accept deri frag = 
  match l_lsym with
  | [] -> None
  | lsymh::lsymt -> let retval_and = match_and lsymh rules_f accept (deri@[(start_sym, lsymh)]) frag in 
    match retval_and with 
    | None -> match_or start_sym lsymt rules_f accept deri frag
    | x -> x
;;

let parse_prefix gram accept frag = 
let start_sym = fst gram and gram_rules = snd gram in
match_or start_sym (gram_rules start_sym) gram_rules accept [] frag
;;


