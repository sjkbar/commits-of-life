let rec sigma tuple =
    match tuple with
    | (a, b, f) ->
        if a = b then (f a)
        else (f a) + (sigma ((a+1), b, f))
;;

(*
let squre x =
    x * x
;;

let default x =
   x
;;

print_endline (string_of_int (sigma (0, 10, squre)))
*)
