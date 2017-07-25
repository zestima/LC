open Big_int
let rec perrin x = 
	if x > 1000000 then failwith "Erro\n" else
    match x with 
    0 ->  big_int_of_int 3
    |1 ->  big_int_of_int 0
    |2 ->  big_int_of_int 2
    |_-> add_big_int (perrin (x-2)  ) (perrin (x-3) )
    
let value = read_int();; 
print_string(string_of_big_int (perrin value));;
print_newline();;
