type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let is_some x = (x != None)

let is_none x = (x = None)
              
let convert_grammar gram1 = match gram1 with
  | (symbol, rules) -> let production_function target = List.map (fun r -> snd r) (List.filter (fun r -> (fst r = target)) rules) in
                       (symbol, production_function)
            
let rec parse_tree_leaves tree = match tree with
  | Leaf value -> [value]
  | Node (value, tree_list) -> parse_tree_leaves_list tree_list
and parse_tree_leaves_list tree_list = match tree_list with
  | [] -> []
  | head::rest -> (parse_tree_leaves head)@(parse_tree_leaves_list rest)

let rec make_helper pf symbol_list accept frag trace =
  match symbol_list with
  | [] -> (accept frag, trace)
  | sym_head::sym_rest -> match sym_head with
                  | T value -> (match frag with
                               | [] -> (None, trace)
                               | frag_head::frag_rest -> if value = frag_head then make_helper pf sym_rest accept frag_rest trace else (None, trace))
                  | N value -> let rules = pf value in
                               try_rules rules pf sym_rest accept frag trace
and try_rules rules pf sym_rest accept frag trace = match rules with
  | [] -> (None, trace)
  | rules_head::rules_rest -> let (ret, trace) = make_helper pf (rules_head@sym_rest) accept frag trace in
                              if is_some ret then (ret, rules_head::trace)
                              else try_rules rules_rest pf sym_rest accept frag trace
                
let make_matcher gram = fun accept frag ->
  let symbol_list = [N (fst gram)] in
  fst (make_helper (snd gram) symbol_list accept frag [])

let accept_empty = function
   | _::_ -> None
   | x -> Some x
                             
let rec rhs2children_list rhs trace = match rhs with
  | [] -> []
  | head::rest -> match head with
                  | N value -> let (new_node, new_trace) = constructing_node value trace in
                               new_node::(rhs2children_list rest new_trace)
                  | T value -> (Leaf value)::(rhs2children_list rest trace)
and rhs2children_trace rhs trace = match rhs with
  | [] -> trace
  | head::rest -> match head with
                  | N value -> let (new_node, new_trace) = constructing_node value trace in
                               rhs2children_trace rest new_trace
                  | T value -> rhs2children_trace rest trace
and constructing_node start_symbol trace =
  let children_list = rhs2children_list (List.hd trace) (List.tl trace)
  and new_trace = rhs2children_trace (List.hd trace) (List.tl trace) in
  (Node (start_symbol, children_list), new_trace)
  
let make_parser gram = fun frag ->
  let symbol_list = [N (fst gram)] in
  let (ret, trace) = make_helper (snd gram) symbol_list accept_empty frag [] in
  if is_none ret then None
  else Some (fst (constructing_node (fst gram) trace))

    
