let subset_test0 = subset [] []
let subset_test1 = not(subset [1;2] [])
let subset_test2 = subset [] [1]
let subset_test3 = subset [1;3] [1;2;3;4]
let subset_test4 = not(subset [1;5] [1;2;3;4])

let equal_sets_test0 = equal_sets [] []
let equal_sets_test1 = equal_sets [1;2;3] [3;2;1]
let equal_sets_test2 = not (equal_sets [1;2] [1;2;3])
let equal_sets_test3 = not (equal_sets [3;2;1] [3])

let set_union_test0 = equal_sets (set_union [] []) []
let set_union_test1 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [1;2;3] [1;2;3]) [1;2;3]
let set_union_test3 = equal_sets (set_union [1;2;3] [4;5;6]) [1;2;3;4;5;6]
let set_union_test4 = equal_sets (set_union [1;1;1] [2;2;2]) [1;2]

let set_intersection_test0 = equal_sets (set_intersection [] []) []
let set_intersection_test1 = equal_sets (set_intersection [1;2;3] []) []
let set_intersection_test2 = equal_sets (set_intersection [1;2;3] [2;3]) [2;3]
let set_intersection_test3 = equal_sets (set_intersection [1;1;1;2] [2;2;1]) [1;2]
let set_intersection_test4 = equal_sets (set_intersection [1;2;3] [1;2;3]) [1;2;3]

let set_diff_test0 = equal_sets (set_diff [] []) []
let set_diff_test1 = equal_sets (set_diff [1;2] [1;2;3]) []
let set_diff_test2 = equal_sets (set_diff [1;2;3;4] [2;3]) [1;4]
let set_diff_test3 = equal_sets (set_diff [3;3;2;2;1;1;4] [3;1;]) [2;4]
let set_diff_test4 = equal_sets (set_diff [1;2;3;2;1] []) [1;2;3]

let fixed_point_test0 = 
computed_fixed_point (=) (fun x -> 2. *. x +. 1.) 999. = infinity

let fixed_point_test1 = 
computed_fixed_point (=) (fun x -> x) 999 = 999

let periodic_point_test0 = 
computed_periodic_point (=) (fun x -> 0.5 *. x +. 1.) 1 999. = 2.

let periodic_point_test1 = 
computed_periodic_point (=) (fun x -> 0.5 *. x +. 1.) 2 999. = 2.

type nonterminals = 
| A | B | C | X | Y

let fba_test0 = 
filter_blind_alleys (A, 
[A, [T"0"; T"1"];
B, [T"2"]])
= (A, 
[A, [T"0"; T"1"];
B, [T"2"]])
					  
let fba_test1 = 
filter_blind_alleys (A, 
[A, [N B];
A, [T"0"; N X];
X, [T"8"; T"4"]])
= (A, 
[A, [T"0"; N X];
X, [T"8"; T"4"]])

let fba_test2 = 
filter_blind_alleys (A, 
[A, [N B];
B, [N A];
A, [T"0"; N X];
X, [T"8"; T"4"]])
= (A, 
[A, [N B];
B, [N A];
A, [T"0"; N X];
X, [T"8"; T"4"]])


let fba_test3 = 
filter_blind_alleys (A,  
[A, [N B];
B, [N A];
A, [T"0"; N X; N B];
X, [T"8"; T"4"]])
= (A, 
[X, [T"8"; T"4"]])

let fba_test4 = 
filter_blind_alleys (A, 
[A, [N B];
B, [N A];
B, [N Y; T"wow"];
A, [T"0"; N X; N B];
X, [T"8"; T"4"];
Y, [T"wow"; N X]])
= (A, 
[A, [N B];
B, [N A];
B, [N Y; T"wow"];
A, [T"0"; N X; N B];
X, [T"8"; T"4"];
Y, [T"wow"; N X]])

let fba_test5 = 
filter_blind_alleys (A, []) = (A, [])

let fba_test6 = 
filter_blind_alleys (A, [X, []; Y, []]) = (A, [X, []; Y, []])
