type ('nonterminal, 'terminal) symbol = 
| N of 'nonterminal
| T of 'terminal

let rec subset_aux a b =
match b with
[] -> false
| h::t -> (a = h || subset_aux a t)
;;

let rec subset a b =
match a with
[] -> true
| h::t -> (subset_aux h b && subset t b)
;;

let equal_sets a b =
subset a b && subset b a
;;

let rec set_uniq a =
match a with
[] -> []
| h::t -> 
if subset_aux h t
then set_uniq t
else h::set_uniq t
;;

let set_union a b =
set_uniq (a @ b)
;;

let rec set_intersection a b =
let ua = set_uniq a in
match ua with
[] -> []
| h::t -> 
if subset_aux h b
then h::set_intersection t b
else set_intersection t b
;;

let rec set_diff a b =
let ua = set_uniq a in
match ua with
[] -> []
| h::t -> 
if subset_aux h b
then set_diff t b
else h::set_diff t b
;;

let rec computed_fixed_point eq f x = 
  if eq x (f x)
  then x
  else computed_fixed_point eq f (f x)
;;

let rec computed_periodic_point eq f p x =
match p with 
| 0 -> x
| _ ->
if eq x (f (computed_periodic_point eq f (p-1) (f x)))
then x
else (computed_periodic_point eq f p (f x))
;;


let rec all_good_symbols l gs = 
match l with 
| [] -> true
| h::t -> 
match h with 
| T _ -> all_good_symbols t gs
| N _ -> subset [h] gs && all_good_symbols t gs
;;

let rec update_good_symbols r gs = 
match r with 
| [] -> gs
| h::t -> 
if all_good_symbols (snd h) gs
then update_good_symbols t ((N(fst h))::gs)
else update_good_symbols t gs
;;

let rec get_good_symbols r gs =
let new_gs = update_good_symbols r gs in 
if equal_sets new_gs gs
then gs
else get_good_symbols r new_gs
;;

let is_good_rule r gs = 
subset [N(fst r)] gs && all_good_symbols (snd r) gs
;;

let rec get_good_rules r gs = 
match r with
| [] -> []
| h::t -> 
if is_good_rule h gs
then h::get_good_rules t gs
else get_good_rules t gs
;;

let filter_blind_alleys g =
let gs = get_good_symbols (snd g) [] in
((fst g), get_good_rules (snd g) gs)
;;



