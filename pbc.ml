open List
open Stream
open Genlex
open Printf
open Str

(* compilação com stream e com Str:

ocamlc str.cma -pp camlp4o .......

*)

type variavel = string
type formula = 
  | Implica of formula*formula
  | Equivale of formula*formula
  | Ou of formula*formula
  | E of formula*formula
  | Nao of formula
  | Var of variavel
  | Verdade
  | Falso

let lexer = make_lexer ["("; ")"; "<->"; "->"; "|"; "&" ; "!"; "TRUE"; "FALSE"];;

let rec parse_expr = parser (* corresponde a entrada E da gramática *)
    [< e1 = parse_conj; e = parse_more_imps e1 >] -> e
and parse_more_imps e1 = parser (* corresponde a entrada E' da gramática *)
    [< 'Kwd "->"; e2 = parse_conj; e = parse_more_imps (Implica(e1, e2)) >] -> e
  | [< 'Kwd "<->"; e2 = parse_conj; e = parse_more_imps (Equivale(e1, e2)) >] -> e
  | [< >] -> e1
and parse_conj = parser (* corresponde a entrada T da gramática *)
    [< e1 = parse_simple; e = parse_more_conjs e1 >] -> e
and parse_more_conjs e1 = parser (* corresponde a entrada T' da gramática *)
    [< 'Kwd "&"; e2 = parse_simple; e = parse_more_conjs (E(e1, e2)) >] -> e
  | [< 'Kwd "|"; e2 = parse_simple; e = parse_more_conjs (Ou(e1, e2)) >] -> e
  | [< >] -> e1
and parse_simple = parser (* corresponde a entrada F da gramática *)
    [< 'Ident s >] -> Var s
  | [< 'Kwd "TRUE" >] -> Verdade
  | [< 'Kwd "FALSE" >] -> Falso
  | [< 'Kwd "!";  'Kwd "("; e = parse_expr; 'Kwd ")" >] -> Nao e 
  | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] -> e;;




let parse_expression = parser [< e = parse_expr; _ = Stream.empty >] -> e;;

(* função principal de leitura usando streams*)
let expression_of_string s = 
  parse_expression
    (lexer
       (Stream.of_string (Str.global_replace (Str.regexp "!") "! " s)
       ));;

let read_expression () =  expression_of_string (read_line ());;

let rec string_of_formula form =
match form with
  | Var v          ->  v
  | Verdade        -> "TRUE"
  | Falso          -> "FALSE"
  | Implica(f, g)  -> ("("^ string_of_formula f ^ " -> " ^ string_of_formula g ^")")
  | Equivale(f, g) -> ("("^ string_of_formula f ^ " <-> " ^ string_of_formula g ^")")
  | E(f, g)        -> ("("^ string_of_formula f ^ " & " ^ string_of_formula g ^")")
  | Ou(f, g)       -> ("("^ string_of_formula f ^ " | " ^ string_of_formula g ^")")
  | Nao (Var v)          ->  ("!("^ v ^")" )
  | Nao f          ->  ("!"^ string_of_formula f)

let rec transform form = 
	match form with
	|Verdade -> Implica (Falso,Falso)
	|Falso ->  Falso
	|Var x -> Var x
	|Nao (Var v)    -> (Implica (Var v , Falso))
	|Nao f -> transform (Implica(f,Falso))
	| Implica(f, g)  ->  Implica (transform f,transform g)
	| Equivale(f, g) -> transform (E(Implica(f,g),Implica(g,f)))
	| E(f, g)        -> transform (Nao(Ou(Nao f,Nao g)))
	| Ou(f, g)       -> transform (Implica(Nao f,g))

let x = read_expression ();;
print_string (string_of_formula (transform x));;
print_newline();;

