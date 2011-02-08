open Forest_parser
open Forest_lexer
open Parse_tree

let () = Printf.printf "
Print a parse forest for a small grammar

Grammar :
S -> E
E -> int
E -> E + E

example : 1+2+3 yields
((1+2)+3)
(1+(2+3))

'q' to quit

"
let () = flush stdout

let lexbuf = Lexing.from_channel stdin

let print_forest forest =
  let rec aux1 t = match t with
    | Int x -> print_int x
    | Node (t1,t2) -> (
        print_string "(";
        aux1 t1;
        print_string "+";
        aux1 t2;
        print_string ")")
  in
  let aux2 t = aux1 t; print_newline () in
  List.iter aux2 forest;
  print_newline ()

let _ =
  try
    while true do
      (Lexing.flush_input lexbuf;
      try
        let pf = Forest_parser.main Forest_lexer.token lexbuf in
        print_forest (List.map (fun (x,_) -> x) pf)
      with
        | Failure s -> Printf.printf "Failure - %s\n\n" s
        | Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
    done
  with Forest_lexer.Eof -> exit 0
