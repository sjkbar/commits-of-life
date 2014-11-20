(*
 * SNU 4190.310 Programming Languages 
 *
 * Vanilla M: interpreter with dynamic type checking
 *)
open M

module M_Vanilla : M_Runner = struct
    open M

    (* domains *)
    type loc = int
    type value = Num of int
               | String of string
               | Bool of bool
               | Loc of loc
               | Pair of value * value
               | Closure of closure
    and closure = fexpr * env
    and fexpr = Fun of id * exp
              | RecFun of id * id * exp
    and env = id -> value
    type memory = int * (loc -> value)

    (* notations (see 5 page in M.pdf) *)
    (* f @+ (x, v)              f[x |-> v]
     * store M (l, v)           M[l |-> v]
     * fetch M l                M(l)
     *)
    let (@+) f (x, v) = (fun y -> if y = x then v else f y)
    let store (l, m) p = (l, m @+ p)        
    let fetch (_, m) l = m l                
    let malloc (l, m) = (l, (l+1, m))

    (* auxiliary functions *)
    let error msg = raise (RuntimeError msg)
    let getString = function (String s) -> s | _ -> error "not a string value"
    let getNum = function (Num n) -> n | _ -> error "not a number value"
    let getBool = function (Bool b) -> b | _ -> error "not a boolean value"
    let getLoc = function (Loc l) -> l | _ -> error "not a location value"
    let getPair = function (Pair (a,b)) -> (a, b) | _ -> error "not a pair"
    let getClosure = function (Closure c) -> c | _ -> error "not a function"
    let op2fn =
        function ADD -> (fun (v1,v2) -> Num (getNum v1 + getNum v2))
         | SUB -> (fun (v1,v2) -> Num (getNum v1 - getNum v2))
         | AND -> (fun (v1,v2) -> Bool (getBool v1 && getBool v2))
         | OR ->  (fun (v1,v2) -> Bool (getBool v1 || getBool v2))
         | EQ ->  (fun (v1,v2) ->
            (match (v1, v2) with
                | ((String s1), (String s2)) -> Bool(s1 = s2)
                | ((Num n1), (Num n2)) -> Bool(n1 = n2)
                | ((Bool b1), (Bool b2)) -> Bool(b1 = b2)
                | ((Loc l1), (Loc l2)) -> Bool(l1 = l2)
                | ((Pair (p1a, p1b)), (Pair (p2a, p2b))) -> Bool(p1a = p2a && p1b = p2b)
                | ((Closure c1), (Closure c2)) -> Bool(c1 = c2)
                | _ -> Bool(false)))

    let rec printValue =
        function Num n -> print_int n; print_newline()
         | Bool b -> print_endline (if b then "true" else "false")
         | String s -> print_endline s
         | _ -> error "unprintable"

    let rec eval env mem exp = match exp
        with CONST c ->
            ((match c with S s -> String s | N n -> Num n | B b -> Bool b), mem)
         | VAR x -> (env x, mem)
         | FN (x, e) -> (Closure (Fun (x, e), env), mem)
         | APP (e1, e2) ->
            let (v1, m') = eval env mem e1 in
            let (c, env') = getClosure v1 in
            let (v2, m'') = eval env m' e2 in
            	(match c with Fun (x, e) -> eval (env' @+ (x,v2)) m'' e
                        | RecFun (f, x, e) -> 
                            let newEnv = (env' @+ (x,v2)) @+ (f, (Closure (RecFun(f,x,e), env'))) in
                            eval newEnv m'' e)
         | LET (NREC (x, e1), e2) ->
            let (v1, m') = eval env mem e1 in
            	eval (env @+ (x,v1)) m' e2
         | LET (REC (f, e1), e2) ->
            let (v1, m') = eval env mem e1 in
            let (fexpr, env') = getClosure v1 in
            let (formalParam, functionBody) =
                (match fexpr with
                    | Fun (id, exp) -> (id, exp)
                    | RecFun (functionName, id, exp) -> (id, exp)
                ) in
            let newClosure = Closure (RecFun(f, formalParam, functionBody), env') in
            	eval (env @+ (f, newClosure)) m' e2
         | IF (e1, e2, e3) ->
            let (v1, m') = eval env mem e1 in
            	eval env m' (if getBool v1 then e2 else e3)
         | BOP (op, e1, e2) ->
            let (v1, m') = eval env mem e1 in
            let (v2, m'') = eval env m' e2 in
            	((op2fn op) (v1,v2), m'')
         | READ ->
            let n = try read_int () with _ -> error "read error" in  
				(Num n, mem)
         | WRITE e ->
            let (v, m') = eval env mem e in
				printValue v; (v, m')
         | MALLOC e ->
            let (v, m') = eval env mem e in
            let (l, m'') = malloc m' in  
				(Loc l, store m'' (l,v))
         | PAIR (e1, e2) ->
            let (v1, m') = eval env mem e1 in
            let (v2, m'') = eval env m' e2 in
                Pair(v1, v2), m''
         | ASSIGN (exp1, exp2) -> 
            let (l, m') = eval env mem exp1 in
            let (v, m'') = eval env m' exp2 in
                (match l with
                | Loc loc -> v, store m'' (loc, v)
                | _ -> raise (RuntimeError "ASSIGN need Loc loc"))

         | SEQ (exp1, exp2) ->
            let (v1, m1) = eval env mem exp1 in
            let (v2, m2) = eval env m1 exp2 in
                (v2, m2)
         | BANG (exp1) ->
            let (l, m') = eval env mem exp1 in
                (match l with
                | Loc loc -> (fetch m' loc), m'
                | _ -> raise (RuntimeError "BANG need Loc loc"))
         | SEL1 (exp1) ->
            let (pair, m') = eval env mem exp1 in
                (match pair with 
                |Pair (v1, v2) -> v1, m'
                | _ -> raise (RuntimeError "SEL1 need Loc loc"))
         | SEL2 (exp1) ->
            let (pair, m') = eval env mem exp1 in
                (match pair with 
                |Pair (v1, v2) -> v2, m'
                | _ -> raise (RuntimeError "SEL2 need Loc loc"))

    let emptyEnv = (fun x -> error ("unknown id: " ^ x))
    let emptyMem = (0, fun l -> error ("unknown loc: " ^ string_of_int l))

    let run exp = ignore (eval emptyEnv emptyMem exp)
end
