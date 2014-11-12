type exp =
    | NUM of int
    | BOOL of bool
    | ADD of exp * exp
    ;;

let rec eval exp =
    match exp with
    | NUM (i) -> i
    | BOOL (b) ->
        if b = true then 1 else 0
    | ADD(exp1, exp2) -> (eval exp1) + (eval exp2)
;;

print_endline (string_of_int (eval (NUM 7)));;
print_endline (string_of_int (eval (BOOL true)));;
print_endline (string_of_int (eval (BOOL false)));;
print_endline (string_of_int (eval (ADD ((NUM 7), (BOOL true)))));;
print_endline (string_of_int (eval (ADD ((NUM 7), (BOOL false)))));;
