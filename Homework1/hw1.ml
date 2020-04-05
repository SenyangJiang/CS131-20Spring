let rec subset a b =
  match a with
    | [] -> true
    | head::rest -> List.mem head b && subset rest b

let equal_sets a b =
  subset a b && subset b a

let set_diff a b =
  match a with
    | [] -> []
    | head::rest -> if List.mem head b then set_diff rest b
                    else head::set_diff rest b

let set_union a b =
  a@set_diff b a

let set_intersection a b =
  match a with
    | [] -> []
    | head::rest -> if List.mem head b then head::set_intersection rest b
                    else set_intersection rest b

let computed_fixed_point eq f x =
  match x with
    | eq (f x) x -> x
    | _ -> computed_fixed_point eq f (f x)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let type_of a =
  match a with
    | N _ -> "N"
    | T _ -> "T"

let equal_second_elem_sets a b =
  equal_sets (snd a) (snd b)

let rec get_reachable_symbols params =
  let rules = fst params
  and reachable_symbols = snd params in
  match rules with
    | [] -> (rules, reachable_symbols)
    | (tmp_symbol, right_hand_side)::rest_rules -> if List.mem tmp_symbol reachable_symbols then (let nonterminal = List.filter (fun s -> (type_of s) = "N") right_hand_side in; get_reachable_symbols rest_rules (set_union reachable_symbols nonterminal) )
                                                   else get_reachable_symbols rest_rules reachable_symbols

let filter_reachable g =
  let start_symbol = fst g
  and rules = snd g in
  let reachable_symbols = computed_fixed_point equal_second_elem_sets get_reachable_symbols (rules, [start_symbol]) in
  filtered_rules = List.filter (fun r -> List.mem (fst r) reachable_symbols) rules
  (start_symbol, filtered_rules)
