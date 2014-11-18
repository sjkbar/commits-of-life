(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * SM5
 *)
open K
open Sm5
module Translator = struct

let rec trans : K.program -> Sm5.command
= fun pgm -> 
  match pgm with
    | K.NUM n -> [Sm5.PUSH (Sm5.Val (Sm5.Z n))]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.VAR i -> [Sm5.PUSH (Sm5.Id i); Sm5.LOAD]
    | K.ADD (e1, e2) ->
        (trans e1) @ (trans e2) @ [Sm5.ADD]
    | K.SUB (e1, e2) ->
        (trans e1) @ (trans e2) @ [Sm5.SUB]
    | K.MUL (e1, e2) ->
        (trans e1) @ (trans e2) @ [Sm5.MUL]
    | K.DIV (e1, e2) ->
        (trans e1) @ (trans e2) @ [Sm5.DIV]
    | K.EQUAL (e1, e2) ->
        (trans e1) @ (trans e2) @ [Sm5.EQ]
    | K.LESS (e1, e2) ->
        (trans e1) @ (trans e2) @ [Sm5.LESS]
    | K.NOT e ->
        (trans e) @ [Sm5.NOT]
    | K.ASSIGN (i, e) ->
        (trans e) @ [Sm5.PUSH (Sm5.Id i);
                     Sm5.STORE;
                     Sm5.PUSH (Sm5.Id i);
                     Sm5.LOAD]
    | K.SEQ (e1, e2) ->
        (trans e1) @ [Sm5.POP] @ (trans e2)
    | K.IF (e1, e2, e3) ->
        (trans e1) @ [Sm5.JTR (trans e2, trans e3)]
    | K.WHILE (e1, e2) ->
        let iWhile = "$while" in
        let iNull = "$null" in
        [Sm5.MALLOC;
         (* l::S M E *)
         Sm5.BIND iNull;
         (* S M E('$null', l) *)
         Sm5.PUSH
           (Sm5.Fn (iNull, ([Sm5.BIND iWhile] @
                            (trans e1) @
                            [Sm5.JTR
                              ((trans e2) @
                               [Sm5.POP;
                                Sm5.PUSH (Sm5.Id iWhile);
                                Sm5.PUSH (Sm5.Id iWhile);
                                Sm5.PUSH (Sm5.Val Sm5.Unit);
                                Sm5.PUSH (Sm5.Id iNull);
                                Sm5.CALL],
                               [Sm5.PUSH (Sm5.Val Sm5.Unit)])])));
         Sm5.BIND iWhile;
         Sm5.PUSH (Sm5.Id iWhile);
         Sm5.PUSH (Sm5.Id iWhile);
         Sm5.PUSH (Sm5.Val Sm5.Unit);
         Sm5.PUSH (Sm5.Id iNull);
         Sm5.CALL;
         Sm5.UNBIND; Sm5.POP;
         Sm5.UNBIND; Sm5.POP]
    | K.FOR (i, e1, e2, e3) ->
        let iFor = "$for" in
        let forAux = "$foraux" in
        let forTemp = "$fortemp" in
        (trans e1) @                          (* v1::S M E *)
        (trans e2) @                          (* v2:v1::S M E *)
        [Sm5.PUSH (Sm5.Val (Sm5.Z 1));        (* 1::v2::v1::S M E *)
         Sm5.ADD;                             (* (v2+1)::v1::S M E *)
         Sm5.BIND forAux;                     (* v1::S M E(forAux,v2+1) *)
         Sm5.PUSH (Sm5.Id i);                 (* l::v1::S M E(forAux,v2+1) *)
         Sm5.STORE;                            (* S M(l,v1) E(forAux,v2+1) *)
         Sm5.PUSH (Sm5.Fn (i,
                           [Sm5.BIND iFor;
                            Sm5.PUSH (Sm5.Id i);
                            Sm5.LOAD;
                            Sm5.BIND forTemp;
                            Sm5.PUSH (Sm5.Id i);
                            Sm5.LOAD;
                            Sm5.PUSH (Sm5.Id forAux);
                            Sm5.LESS;
                            Sm5.JTR (
                              (trans e3) @
                              [Sm5.POP;
                               Sm5.PUSH (Sm5.Id iFor);
                               Sm5.PUSH (Sm5.Id iFor);
                               Sm5.PUSH (Sm5.Id forTemp);
                               Sm5.PUSH (Sm5.Val (Sm5.Z 1));
                               Sm5.ADD;
                               Sm5.PUSH (Sm5.Id i);
                               Sm5.UNBIND;
                               Sm5.POP;
                               Sm5.CALL],
                              [Sm5.PUSH (Sm5.Id i);
                               Sm5.LOAD;
                               Sm5.PUSH (Sm5.Val (Sm5.Z 1));
                               Sm5.SUB;
                               Sm5.PUSH (Sm5.Id i);
                               Sm5.STORE;
                               Sm5.PUSH (Sm5.Val Sm5.Unit)])]));
         Sm5.BIND iFor;
         Sm5.PUSH (Sm5.Id iFor);
         Sm5.PUSH (Sm5.Id iFor);
         Sm5.PUSH (Sm5.Id i);
         Sm5.LOAD;
         Sm5.PUSH (Sm5.Id i);
         Sm5.CALL;
         Sm5.UNBIND; Sm5.POP;
         Sm5.UNBIND; Sm5.POP]
    | K.LETV (i, e1, e2) ->
        (trans e1)            @  (* v1::S M E *)
        [Sm5.MALLOC;             (* l::v1::S M E *)
         Sm5.BIND i;             (* v1::S M E(i,l) *)
         Sm5.PUSH (Sm5.Id i);    (* l::v1::S M E(i,l) *)
         Sm5.STORE] @            (* S M(l,v1) E(i,l) *)
        (trans e2) @             (* v2::S M(l,v1) E(i,l) *)
        [Sm5.UNBIND;
         Sm5.POP] 
    | K.LETF (i1, i2, e1, e2) ->
        [Sm5.PUSH (Sm5.Fn (i2, [Sm5.BIND i1] @ (trans e1)));
         Sm5.BIND i1] @
        (trans e2) @
        [Sm5.UNBIND;
         Sm5.POP]
    | K.CALLV (i, e) ->
        [Sm5.PUSH (Sm5.Id i);           (* (x,C,E')::S M E *)
         Sm5.PUSH (Sm5.Id i)]         @ (* (x,C,E')::(x,C,E')::S M E *)
        (trans e)                     @ (* v::(x,C,E')::(x,C,E')::S M E *)
        [Sm5.MALLOC;                    (* l::v::(x,C,E')::(x,C,E')::S M E *)
         Sm5.CALL]                      (* (x,C,E')::S M(l,v) E'(x,l) *)
    | K.CALLR (i1, i2) ->
        [Sm5.PUSH (Sm5.Id i1);          (* (x,C,E')::S M E *)
         Sm5.PUSH (Sm5.Id i1);          (* (x,C,E')::(x,C,E')::S M E *)
         Sm5.PUSH (Sm5.Id i2);          (* l2::(x,C,E')::(x,C,E')::S M E *)
         Sm5.LOAD;                      (* v2::(x,C,E')::(x,C,E')::S M E *)
         Sm5.PUSH (Sm5.Id i2);          (* l2::v2::(x,C,E')::(x,C,E')::S M E *)
         Sm5.CALL]                      (* (x,C,E')::S M E *)
    | K.READ i ->
        [Sm5.GET;
         Sm5.PUSH (Sm5.Id i);
         Sm5.STORE;
         Sm5.PUSH (Sm5.Id i);
         Sm5.LOAD]
    | K.WRITE e ->
        let writeAux = "$writeAux" in
        (trans e) @
        [Sm5.BIND writeAux;
         Sm5.PUSH (Sm5.Id writeAux);
         Sm5.PUSH (Sm5.Id writeAux);
         Sm5.PUT;
         Sm5.UNBIND;
         Sm5.POP]
end
