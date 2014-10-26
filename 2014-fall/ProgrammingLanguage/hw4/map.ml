let parse_map s = 
  let lexbuf = Lexing.from_string s in 
    TParser.parse_map TLexer.start lexbuf 

    let m1 = parse_map "a|b" 
    let m2 = (Branch (End (NameBox "a"), End (NameBox "b"))) 
    let _  = print_string (string_of_bool (m1 = m2)) 
    (* "true" 출력 *) 

