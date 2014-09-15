type nat = ZERO | SUCC of nat

let rec natadd nats =
    match nats with
    | (ZERO, nat1) -> nat1
    | (SUCC nat1, nat2) -> natadd (nat1, SUCC nat2)
    ;;

let rec natmul nats =
    match nats with
    | (ZERO, ZERO) -> ZERO
    | (nat1, ZERO) -> ZERO
    | (ZERO, nat1) -> ZERO
    | (SUCC ZERO, nat1) -> nat1
    | (SUCC nat1, nat2) -> natadd (nat2, natmul(nat1, nat2))
    ;;

let rec toInt nat =
    match nat with
    | (ZERO) -> 0
    | (SUCC lRat) -> 1 + toInt lRat
    ;;

(*
let zero =ZERO;;
let one = (SUCC ZERO);;
let two = (SUCC (SUCC ZERO));;
let three = (SUCC (SUCC (SUCC ZERO)));;
let four = (SUCC (SUCC (SUCC (SUCC ZERO))));;
let five = (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))));;

print_endline (string_of_int (toInt (natmul (zero, zero))));

print_endline (string_of_int (toInt (natmul (one, zero))));
print_endline (string_of_int (toInt (natmul (one, one))));
print_endline (string_of_int (toInt (natmul (one, two))));

print_endline (string_of_int (toInt (natmul (three, zero))));

print_endline (string_of_int (toInt (natmul (three, one))));
print_endline (string_of_int (toInt (natmul (three, five))));
print_endline (string_of_int (toInt (natmul (four, five))));
*)
