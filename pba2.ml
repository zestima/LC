open Big_int
exception Erro;;
let rec perrin (x:big_int) (acc1:big_int) (acc2:big_int) (acc3:big_int) = 
	if compare_big_int x (big_int_of_int 1000000) = 1 then raise Erro else
	
	if compare_big_int x (big_int_of_int 0) = 0 then  acc1 else 
	if compare_big_int x (big_int_of_int 1) = 0 then  acc2 else 
	if compare_big_int x (big_int_of_int 2) = 0 then  acc3 else 
	perrin (sub_big_int x (big_int_of_int 1) ) acc2 acc3 (add_big_int acc2 acc1)


let value= big_int_of_int (read_int());;
print_string ( string_of_big_int (perrin value (big_int_of_int 3) (big_int_of_int 0) (big_int_of_int 2)));;
  print_newline();;
                
