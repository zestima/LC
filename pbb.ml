open List
open Stream
open Genlex
open Printf
open Str
open Hashtbl
open Array

type variavel = string
type formula =
	|Implica of formula * formula
	|Equivale of formula * formula
	|Ou of formula * formula
	|E of formula * formula
	|Nao of formula
	|Var of variavel
	|Verdade
	|Falso

let lexer = make_lexer ["("; ")"; "<->"; "->"; "|"; "&" ; "!"; "TRUE"; "FALSE"];;

let rec parse_expr = parser (* corresponde a entrada E da gramÃ¡tica *)
    [< e1 = parse_conj; e = parse_more_imps e1 >] -> e
and parse_more_imps e1 = parser (* corresponde a entrada E' da gramÃ¡tica *)
    [< 'Kwd "->"; e2 = parse_conj; e = parse_more_imps (Implica(e1, e2)) >] -> e
  | [< 'Kwd "<->"; e2 = parse_conj; e = parse_more_imps (Equivale(e1, e2)) >] -> e
  | [< >] -> e1
and parse_conj = parser (* corresponde a entrada T da gramÃ¡tica *)
    [< e1 = parse_simple; e = parse_more_conjs e1 >] -> e
and parse_more_conjs e1 = parser (* corresponde a entrada T' da gramÃ¡tica *)
    [< 'Kwd "&"; e2 = parse_simple; e = parse_more_conjs (E(e1, e2)) >] -> e
  | [< 'Kwd "|"; e2 = parse_simple; e = parse_more_conjs (Ou(e1, e2)) >] -> e
  | [< >] -> e1
and parse_simple = parser (* corresponde a entrada F da gramÃ¡tica *)
    [< 'Ident s >] -> Var s
  | [< 'Kwd "TRUE" >] -> Verdade
  | [< 'Kwd "FALSE" >] -> Falso
  | [< 'Kwd "!";  'Kwd "("; e = parse_expr; 'Kwd ")" >] -> Nao e 
  | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] -> e;;

let parse_expression = parser [< e = parse_expr; _ = Stream.empty >] -> e;;

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
  | Nao (Var v)    ->  ("!("^ v ^")" )
  | Nao f          ->  ("!"^ string_of_formula f)

let rec eval formula tabela =
	match formula with
	|	Var x		-> (Hashtbl.find tabela x)
	| 	Verdade -> true
	|	Falso -> false
	|	Implica (g,h)	->  ((not( eval g tabela )) || ( eval h tabela ))
	|	Equivale (g,h)	-> (((not( eval g tabela )) || ( eval h tabela )) && ((not( eval h tabela )) || ( eval g tabela )))
	|	E (g,h)	-> ( eval g tabela ) && ( eval h tabela )
	|	Ou (g,h)	-> ( eval g tabela ) || ( eval h tabela )
	|	Nao (Var x)	-> if (( Hashtbl.find tabela x )=true) then false else true
	|	Nao (g) -> let valor = eval g tabela in not(valor)
	
let check var tabela = 
	try 
		let k = (Hashtbl.find tabela var) in
		if k then ()
		with Not_found ->(let value = (read_line()) in 
							if value = "TRUE" then (Hashtbl.add tabela var true) else 
									if value = "FALSE" then (Hashtbl.add tabela var false));;
	
let rec find form tabela = 
	match form with
	|Verdade -> check "true" tabela
	|Falso ->  check "false" tabela
	|Var x -> check x tabela
	|Nao (Var v)    -> check v tabela
	|Nao f -> find f tabela
	| Implica(f, g)  -> find (Ou(Nao(f),g)) tabela 
	| Equivale(f, g) -> find (E(Implica(f,g),Implica(g,f))) tabela
	| E(f, g)        -> find f tabela;find g tabela
	| Ou(f, g)       -> find f tabela;find g tabela
	
let rec getlit form =
	match form with
	|Var x -> [x]
	|Nao (Var x )->[x]
	|Verdade -> ["true"]
	|Falso -> ["false"]
	|Nao f -> getlit f
	| Implica(f, g)  -> getlit (Ou(Nao(f),g))
	| Equivale(f, g) -> getlit (E(Implica(f,g),Implica(g,f)))
	| E(f, g)        -> (getlit f) @ (getlit g) 
	| Ou(f, g)       -> (getlit f) @ (getlit g) 
	
let rec remove_duplicates lista= 
	match lista with 
	| [] -> []
	| head::tail -> head::(remove_duplicates (List.filter (fun x -> x<>head) tail))

let rec next v n =
	if v.(n) = false then v.(n)<- true
	else ( v.(n) <- false; next v (n-1););;

let rec solve form tabela vars array n = 
	let r = ref 0 in
	List.iter(fun x -> (Hashtbl.replace tabela x (array.(!r));r:=!r+1))vars;
	if(eval form tabela) = false then print_string "NO\n" else 
		let k = Array.fold_left (&&) true array in
			if k then print_string "YES\n" else (
				next array n;solve form tabela vars array n
	)
	
let x = read_expression();;
let lista = remove_duplicates (getlit x);;
let n = read_int();;
let tbl = Hashtbl.create n;;
let a = Array.make n false;;
find x tbl;;
let valor = eval x tbl;;
if valor = true then print_string "TRUE\n" else print_string "FALSE\n";;
solve x tbl lista a (n-1);;
