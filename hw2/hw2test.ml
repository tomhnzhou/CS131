let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num


let awksub_rules =
   [Expr, [N Term; N Binop; N Expr];
    Expr, [N Term];
    Term, [N Num];
    Term, [N Lvalue];
    Term, [N Incrop; N Lvalue];
    Term, [N Lvalue; N Incrop];
    Term, [T"("; N Expr; T")"];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awkish_grammar = convert_grammar (Expr, awksub_rules)
let awkish_grammar_rules_f = snd awkish_grammar

let convert_grammar_test0 = 
     awkish_grammar_rules_f Expr =
         [[N Term; N Binop; N Expr];
          [N Term]] &&
     awkish_grammar_rules_f Term =
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]] &&
     awkish_grammar_rules_f Lvalue =
	 [[T"$"; N Expr]] &&
     awkish_grammar_rules_f Incrop =
	 [[T"++"];
	  [T"--"]] &&
     awkish_grammar_rules_f Binop =
	 [[T"+"];
	  [T"-"]] &&
     awkish_grammar_rules_f Num =
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]


let test0 =
  ((parse_prefix awkish_grammar accept_all ["++"]) = None)

let test1 =
  ((parse_prefix awkish_grammar accept_all []) = None)

let test2 =
  (parse_prefix awkish_grammar accept_all ["0"])
   = Some ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "0"])], [])

let test3 =
  (parse_prefix awkish_grammar accept_all ["(";"0";")"])
   = Some ([(Expr, [N Term]); (Term, [T"("; N Expr; T")"]); 
           (Expr, [N Term]); (Term, [N Num]); (Num, [T "0"])], [])

let test4 = 
  (parse_prefix awkish_grammar accept_all ["(";"$"; "0"; "++"; ")"])
  = Some ([(Expr, [N Term]); (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]);
   (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
   (Term, [N Num]); (Num, [T "0"]); (Incrop, [T "++"])],
  [])

let test5 = 
    parse_prefix awkish_grammar accept_all 
    ["(";"$"; "$"; "$"; "++"; "$"; "5"; "++"; "--"; ")"; "-"; "5"]
    = Some ([(Expr, [N Term; N Binop; N Expr]); (Term, [T "("; N Expr; T ")"]);
   (Expr, [N Term]); (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
   (Expr, [N Term]); (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]);
   (Expr, [N Term]); (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]);
   (Expr, [N Term]); (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
   (Num, [T "5"]); (Incrop, [T "++"]); (Incrop, [T "--"]); (Binop, [T "-"]);
   (Expr, [N Term]); (Term, [N Num]); (Num, [T "5"])],
  [])
