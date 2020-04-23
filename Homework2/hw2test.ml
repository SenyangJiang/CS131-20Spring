let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type english_nonterminals =
  Sentence | Noun | Verbphrase | Verb | Nounphrase | Det

let english_grammar =
  (Sentence,
   function
   | Sentence ->
      [[N Noun; N Verbphrase]]
   | Verbphrase ->
      [[N Verb; N Verbphrase];
       [N Verb; N Nounphrase]]
   | Nounphrase ->
      [[N Det; N Noun]]
   | Noun ->
      [[T "Mike"];
       [T "John"];
       [T "water"];
       [T "work"]]
   | Det ->
      [[T "that"];
       [T "those"];
       [T "the"]]
   | Verb ->
      [[T "has"];
       [T "finished"]])

let test_frag = ["John"; "has"; "finished"; "the"; "work"]
              
let make_matcher_test =
  ((make_matcher english_grammar accept_empty_suffix test_frag) = Some [])

let make_parser_test =
  match make_parser english_grammar test_frag with
  | Some tree -> parse_tree_leaves tree = test_frag
  | _ -> false
