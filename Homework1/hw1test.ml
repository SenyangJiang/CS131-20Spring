let my_subset_test0 = subset [] []
let my_subset_test1 = subset [1;1;2;2] [3;2;1]
let my_subset_test2 = not (subset [1;5;3] [])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1;2;3;3;5;5] [1;2;3;5]
let my_equal_sets_test2 = not (equal_sets [1;3;2] [2;3;4])
                        
let my_set_union_test0 = equal_sets (set_union [] [1;3;5]) [1;3;5]
let my_set_union_test1 = equal_sets (set_union [1;3;3] [1;3;5]) [1;3;5]
let my_set_union_test2 = equal_sets (set_union [1;1;2;3;4] [1;3;5]) [1;3;2;4;5]

let my_set_intersection_test0 = equal_sets (set_intersection [] [1;2;3]) []
let my_set_intersection_test1 = equal_sets (set_intersection [1;2;3] [2;3;4]) [2;3]
let my_set_intersection_test2 = equal_sets (set_intersection [1;2;3] [1;2;3]) [1;2;3]                         
let my_set_diff_test0 = equal_sets (set_diff [] [1;3;5]) []
let my_set_diff_test1 = equal_sets (set_diff [1;2;3] [2;3;1]) []
let my_set_diff_test2 = equal_sets (set_diff [3;4;4;5;1] [1;2;3]) [4;5]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x) 10 = 10
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x/2) 1000000 = 0
let my_computed_fixed_point_test2 =
  computed_fixed_point (fun x y -> x - y > 1000) (fun x -> x * 2) 1 = 1024

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
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

let my_filter_reachable_test0 =
  filter_reachable (Num, awksub_rules) =
    (Num,
    [Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]
)

let my_filter_reachable_test1 =
  filter_reachable (Incrop, awksub_rules) =
    (Incrop,
    [Incrop, [T"++"];
     Incrop, [T"--"]])

let my_filter_reachable_test2 =
  filter_reachable (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    (Expr,
    [Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];])
