open K
open Unix
open Translate
open Sm5

let test_run pgm_str = 
  let pgm = Parser.program Lexer.start (Lexing.from_string pgm_str) in
  let sm5_pgm = Translator.trans pgm in
  Sm5.run sm5_pgm

let _ = test_run "let x :=0 in (read x; write (5 + 6 * x - 8 / 4))" 
