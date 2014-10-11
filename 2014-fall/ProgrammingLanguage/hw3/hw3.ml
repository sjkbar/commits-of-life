(*
   SNU 4190.310 Programming Languages (Fall 2013)
 
   K- Interpreter
*)
(* Location : don't mention it *)
module type LOC =
sig
	type t
	val base : t
	val equal : t -> t -> bool
	val diff : t -> t -> int
	val increase : t -> int -> t
	val read : t -> int
end;;

module Loc : LOC =
struct
	type t = Location of int
	let base = Location(0)
	let equal (Location(a)) (Location(b)) = (a = b)
	let diff (Location(a)) (Location(b)) = a - b
	let increase (Location(base)) n = Location(base+n)
	let read (Location n) = n
end;;

(* Memory Signature *)
module type MEM = 
sig
	type 'a t
	exception Not_allocated
	exception Not_initialized
	val empty : 'a t (* get empty memory *)
	val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
	val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
	val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end;;

(* Environment Signature *)
module type ENV =
sig
	type ('a, 'b) t
	exception Not_bound
	val empty : ('a, 'b) t (* get empty environment *)
	val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
	val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end;;

(* Memory Implementation *)
module Mem : MEM =
struct
	exception Not_allocated
	exception Not_initialized
	type 'a content = V of 'a | U
	type 'a t = M of Loc.t * 'a content list
	let empty = M(Loc.base,[])

	let rec replace_nth = fun l n c -> 
		match l with
		| h::t -> if n = 1 then c::t else h::(replace_nth t (n-1) c)
		| [] -> raise Not_allocated

	let load (M(boundary,storage)) loc =
		match (List.nth storage ((Loc.diff boundary loc) - 1)) with
		| V(v) -> v 
		| U -> raise Not_initialized

	let store (M(boundary,storage)) loc content =
		M(boundary, replace_nth storage (Loc.diff boundary loc) (V(content)))

	let alloc (M(boundary,storage)) = (boundary,M(Loc.increase boundary 1,U::storage))
end;;

(* Environment Implementation *)
module Env : ENV=
struct
	exception Not_bound
	type ('a, 'b) t = E of ('a -> 'b)
	let empty = E(fun x -> raise Not_bound)
	let lookup (E(env)) id = env id
	let bind (E(env)) id loc = E(fun x -> if x = id then loc else env x)
end;;

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
	exception Error of string
	type id = string
	type exp =
	| NUM of int | TRUE | FALSE | UNIT
	| VAR of id
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| EQUAL of exp * exp
	| LESS of exp * exp
	| NOT of exp
 	| SEQ of exp * exp            (* sequence *)
 	| IF of exp * exp * exp       (* if-then-else *)
  	| WHILE of exp * exp          (* while loop *)
  	| LETV of id * exp * exp      (* variable binding *)
  	| LETF of id * id list * exp * exp (* procedure binding *)
  	| CALLV of id * exp list      (* call by value *)
  	| CALLR of id * id list       (* call by referenece *)
  	| RECORD of (id * exp) list   (* record construction *)
  	| FIELD of exp * id           (* access record field *)
  	| ASSIGN of id * exp          (* assgin to variable *)
	| ASSIGNF of exp * id * exp   (* assign to record field *)
  	| READ of id
	| WRITE of exp
    
	type program = exp
	type memory
	type env
	type value
	val emptyMemory : memory
	val emptyEnv : env
	val run : memory * env * program -> value
    val runTest : int
end;;

module K : KMINUS =
struct
	exception Error of string

	type id = string
	type exp =
	| NUM of int | TRUE | FALSE | UNIT
	| VAR of id
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| EQUAL of exp * exp
	| LESS of exp * exp
  	| NOT of exp
  	| SEQ of exp * exp            (* sequence *)
  	| IF of exp * exp * exp       (* if-then-else *)
  	| WHILE of exp * exp          (* while loop *)
  	| LETV of id * exp * exp      (* variable binding *)
  	| LETF of id * id list * exp * exp (* procedure binding *)
  	| CALLV of id * exp list      (* call by value *)
  	| CALLR of id * id list       (* call by referenece *)
  	| RECORD of (id * exp) list   (* record construction *)
  	| FIELD of exp * id           (* access record field *)
  	| ASSIGN of id * exp          (* assgin to variable *)
	| ASSIGNF of exp * id * exp   (* assign to record field *)
	| READ of id
	| WRITE of exp

	type program = exp

	type value =
	| Num of int
	| Bool of bool
	| Unit
	| Record of (id -> Loc.t)
    
	type memory = value Mem.t
	type env = (id, env_entry) Env.t
	and  env_entry = Addr of Loc.t | Proc of id list * exp * env

	let emptyMemory = Mem.empty
	let emptyEnv = Env.empty

	let value_int v = 
		match v with 
		| Num n -> n
		| Bool _ -> raise (Error "Bool type is used as Num type")
		| Unit -> raise (Error "Unit type is used as Num type")
		| Record _ -> raise (Error "Unit type is used as Num type")

	let value_bool v =
		match v with
		| Bool b -> b
		| Num _ -> raise (Error "Num type is used as Bool type")
		| Unit -> raise (Error "Unit type is used as Bool type")
		| Record _ -> raise (Error "Unit type is used as Bool type")

    let value_unit v =
		match v with 
		| Unit -> ()
		| Num _ -> raise (Error "Num type is used as Unit type")
		| Bool _ -> raise (Error "Bool type is used as Unit type")
		| Record _ -> raise (Error "Bool type is used as Unit type")

	let value_record v =
		match v with
		| Record r -> r
		| Num _ -> raise (Error "Num type is used as Record type")
		| Unit -> raise (Error "Unit type is used as Record type")
		| Bool _ -> raise (Error "Bool type is used as Record type")

	let env_loc e x =
		try
			(match Env.lookup e x with
			| Addr l -> l
			| Proc _ -> raise (Error "not allowed"))
		with Env.Not_bound -> raise (Error (String.concat " " [x;"not bound"]))

	let env_proc e f =
		try
			(match Env.lookup e f with
  			| Addr _ -> raise (Error "not allowed")
			| Proc (id, exp, env) -> (id, exp, env))
		with Env.Not_bound -> raise (Error (String.concat " " [f;"not bound"]))


    let printId id =
        print_endline id

    let printIdList idList =
        (List.iter printId idList)

	let rec semantics : memory -> env -> exp -> (value * memory) =
		fun mem env e ->
            match e with
		        | NUM i -> (Num i, mem)
	            | TRUE -> ((Bool true), mem)
	            | FALSE -> ((Bool false), mem)
                | UNIT -> (Unit, mem)
                | VAR id -> ((Mem.load mem (env_loc env id)), mem)
	            | ADD (exp1, exp2) ->
                    let val1, mem1 = (semantics mem env exp1) in
                        let val2, mem2 = (semantics mem1 env exp2) in
                            (Num ((value_int val1) + (value_int val2)), mem2)
	            | SUB (exp1, exp2) ->
                    let val1, mem1 = (semantics mem env exp1) in
                        let val2, mem2 = (semantics mem1 env exp2) in
                            (Num ((value_int val1) - (value_int val2)), mem2)
	            | MUL (exp1, exp2) ->
                    let val1, mem1 = (semantics mem env exp1) in
                        let val2, mem2 = (semantics mem1 env exp2) in
                            (Num ((value_int val1) * (value_int val2)), mem2)
	            | DIV (exp1, exp2) ->
                    let val1, mem1 = (semantics mem env exp1) in
                        let val2, mem2 = (semantics mem1 env exp2) in
                            (Num ((value_int val1) / (value_int val2)), mem2)
	            | EQUAL (exp1, exp2) ->
                    let val1, mem1 = (semantics mem env exp1) in
                        let val2, mem2 = (semantics mem1 env exp2) in
                            let isSame = (function ((Num int1), (Num int2)) -> int1 = int2
                                |((Bool bool1), (Bool bool2)) -> bool1 = bool2
                                |((Unit), (Unit)) -> true
                                |(_, _) -> false) (val1, val2)
                                in (Bool isSame, mem2)
	            | LESS (exp1, exp2) ->
                    let val1, mem1 = (semantics mem env exp1) in
                        let val2, mem2 = (semantics mem1 env exp2) in
                            (Bool ((value_int val1) < (value_int val2)), mem2)
  	            | NOT exp1 ->
                    let val1, mem1 = (semantics mem env exp1) in
                            (Bool (not (value_bool val1)), mem1)
 	            | SEQ (exp1, exp2) ->
                    let (val1, mem1) = (semantics mem env exp1) in
                        let (val2, mem2) = (semantics mem1 env exp2) in
                            (val2, mem2)
  	            | IF (exp1, exp2, exp3) ->
                    let (val1, mem1) = (semantics mem env exp1) in
                        if (value_bool val1)
                            then (semantics mem1 env exp2)
                            else (semantics mem1 env exp3)
  	            | WHILE (exp1, exp2) ->
                    let (val1, mem1) = (semantics mem env exp1) in
                        if (value_bool val1)
                            then let (val2, mem2) = (semantics mem1 env exp2) in
                                (semantics mem2 env (WHILE (exp1, exp2)))
                            else (Unit, mem1)
                | LETV (id, exp1, exp2) ->
                    let (value, mem1)  = semantics mem env exp1 in
                        let (location, (memAlloc)) = (Mem.alloc mem1) in
                            let newMem = (Mem.store memAlloc location value) in
                                let newEnv = (Env.bind env id (Addr location)) in
                                    (semantics newMem newEnv exp2)
                | LETF (id, idList, exp1, exp2) ->
                    let newEnv = (Env.bind env id (Proc (idList, exp1, env))) in
                        (semantics mem newEnv exp2)
  	            | CALLV (id, expList) ->
                    let procedure = (env_proc env id) in
                        let rec expListResolve = function (expList, mem, valList) ->
                            (match expList with
                            | [] -> (mem, (List.rev valList))
                            | exp::tl -> (match (semantics mem env exp) with
                                | (newVal, newMem) -> (expListResolve (tl, newMem, newVal::valList)))) in
                            let rec argBind = function (idList, valList, env, mem) ->
                                (match valList with
                                | [] -> (env, mem)
                                | newVal::tl -> let (location, (memAlloc)) = (Mem.alloc mem) in
                                    let newMem = (Mem.store memAlloc location newVal) in
                                        let newEnv = (Env.bind env (List.hd idList) (Addr location)) in
                                            argBind ((List.tl idList), (List.tl valList), newEnv, newMem)) in
                                (match procedure with
                                | (idList, bodyExp, procEnv) ->
                                    let (mem1, valList) = expListResolve (expList, mem, []) in
                                        if (List.length idList) = (List.length valList)
                                            then let (newProcEnv1, newProcMem) = argBind (idList, valList, procEnv, mem1) in
                                                let newProcEnv2 = (Env.bind newProcEnv1 id (Proc (idList, bodyExp, procEnv))) in
                                                    (semantics newProcMem newProcEnv2 bodyExp)
                                            else raise (Error "Invalid_argument"))
                | CALLR (id, idList) ->
                    let procedure = (env_proc env id) in
                        let rec argBind = function (idList, envEntryList, env) ->
                            (match envEntryList with
                            | [] -> (env)
                            | envEntry::tl ->
                                let newEnv = (Env.bind env (List.hd idList) (List.hd envEntryList)) in
                                    argBind ((List.tl idList), (List.tl envEntryList), newEnv)) in
                            let envEntryList = (List.map (Env.lookup env) idList) in
                                (match procedure with
                                | (procIdList, bodyExp, procEnv) ->
                                    if (List.length procIdList) = (List.length envEntryList)
                                        then let procEnv1 = argBind (procIdList, envEntryList, procEnv) in
                                            let procEnv2 = (Env.bind procEnv1 id (Proc (procIdList, bodyExp, procEnv)))
                                            in (semantics mem procEnv2 bodyExp)
                                        else raise (Error "Invalid_argument"))
  	            | RECORD idExpList ->
                    if (List.length idExpList = 0) 
                        then (Unit, mem)
                        else let rec splitIdExpList = function (idExpList, revIdList, revExpList) ->
                            (match idExpList with
                            | [] -> ((List.rev revIdList), (List.rev revExpList))
                            | hd::tl -> splitIdExpList (tl, (fst hd)::revIdList, (snd hd)::revExpList)) in
                            let rec expListResolve = function (expList, mem, valList) ->
                                (match expList with
                                | [] -> (mem, (List.rev valList))
                                | exp::tl -> (match (semantics mem env exp) with
                                    | (newVal, newMem) -> (expListResolve (tl, newMem, newVal::valList)))) in
                            let rec recordBind = function (idList, valList, record, mem) ->
                                (match (List.rev idList) with
                                | [] -> (record, mem)
                                | hd::tl -> let (location, (mem1)) = (Mem.alloc mem) in
                                    let recordId = (List.hd idList) in
                                    let recordVal = (List.hd valList) in
                                        let mem2 = (Mem.store mem1 location recordVal) in
                                            let recordFunction = (value_record record) in
                                                recordBind (List.tl(idList), List.tl(valList), (Record (function id -> 
                                                if id = recordId then location else (recordFunction id))), mem2)) in
                            let (idList, expList) = splitIdExpList (idExpList, [], []) in
                            let (mem1, valist) = expListResolve(expList, mem, []) in
                                recordBind (idList, valist, (Record (function id -> raise (Error (String.concat " " ["no such record"; id])))), mem1)
  	            | FIELD (exp1, id) ->
                    let (record, mem1) = (semantics mem env exp1) in
                        let location = (value_record(record) id) in
                            let value = (Mem.load mem1 location) in
                                (value, mem1)
                | ASSIGNF(exp1, id,  exp2) ->
                    let (record, mem1) = (semantics mem env exp1) in
                        let (value, mem2) = (semantics mem1 env exp2) in
                            let location = (value_record(record) id) in
                                let mem3 = (Mem.store mem2 location value) in
                                    (value, mem3)
  	            | ASSIGN (id, exp1) ->
                    let (val1, mem1) = (semantics mem env exp1) in
                        let location = (env_loc env id) in
                            let newMem = (Mem.store mem1 location val1) in
                                    (val1, newMem)
  	            | READ id ->
                    let integer = read_int() in
                        let value = Num integer in
                            let location = (env_loc env id) in
                                let newMem = (Mem.store mem location value) in
                                        (value, newMem)
	            | WRITE exp ->
                    let (val1, mem1) = (semantics mem env exp) in
                        print_int(value_int(val1));
                        (val1, mem1)

	let run (mem, env, pgm) =
		let (v,_) = semantics mem env pgm in
		v

    let runTest =
        let exp1 = FIELD (RECORD([("x", (NUM 1)); ("y", (NUM 2)); ("z", (NUM 3))]), "z") in
        value_int (run (emptyMemory, emptyEnv, exp1))
end;;
(*
print_endline(string_of_int (K.runTest));;*)
