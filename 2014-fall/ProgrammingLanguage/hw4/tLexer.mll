{ 
     open TParser 
      exception Eof 
       exception LexicalError 
        let verbose1 s =  (* (print_string s; print_newline(); s) *) s 
         let verbose2 s =  (* (print_string s; print_newline()) *) () 
} 

let blank = [' ' '\n' '\t' '\r']+ 
let name = ['a'-'z'] 

rule start = 
 parse blank { start lexbuf } 
      | name { let id = verbose1 (Lexing.lexeme lexbuf) in ID id } 
           | "*" { verbose2 "*"; STAR} 
                | "]" { verbose2 "]"; RB} 
                     | "[" { verbose2 "["; LB} 
                          | "|" { verbose2 "|"; BAR} 
                               | "(" { verbose2 "("; LP} 
                                    | ")" { verbose2 ")"; RP} 
                                         | eof { verbose2 "eof"; EOF} 
                                              | _ {raise LexicalError} 
