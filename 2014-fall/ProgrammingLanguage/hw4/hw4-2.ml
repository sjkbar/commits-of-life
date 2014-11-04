type treasure = StarBox | NameBox of string;;

type key = Bar
    | Node of key * key
;;

type map = End of treasure
    | Branch of map * map
    | Guide of string * map
;;

type operand = I
    | Fn of operand * operand
    | V of string
;;

type equation = Equation of operand * operand
;;

type solution = Map of operand * operand
;;

module Env =
struct
    type id =
        string
    type  env_entry =
        Value of operand | Null
	type env =
        E of (id -> env_entry)

	let empty = E(fun x -> Null)
	let lookup (E(env)) id = env id
	let bind (E(env)) id loc = E(fun x -> if x = id then loc else env x)
end;;

let counter =
  let count = ref (-1) in
    fun () -> incr count; "!@#^" ^ string_of_int(!count)

exception IMPOSSIBLE
;;

(* operand -> string *)
let rec operandToString operand =
    match operand with
    | I -> "I"
    | Fn (operand1, operand2) -> operandToString(operand1) ^ " -> " ^ operandToString(operand2)
    | V x -> "V " ^ x
;;

let rec mapToString (Map (operand1, operand2)) =
    (operandToString operand1) ^ " |->" ^ (operandToString operand2)

let rec equationToString (Equation (operand1, operand2)) =
    (operandToString operand1) ^ " = " ^ operandToString(operand2)

let rec equationListToString equationList =
    "[ " ^ (List.fold_left (fun result equation -> result ^ (equationToString equation) ^" , ") "" equationList) ^ " ]\n";;

let rec mapListToString mapList =
    "[ " ^ (List.fold_left (fun result equation -> result ^ (mapToString equation) ^ " , ") "" mapList) ^ " ]\n";;

let vars = ref []
;;
let newVars = 
    function var -> vars := var::(!vars);
;;
let clearVars = function () -> vars := []
;;

(* return (env equation) *)
let rec getEquationHelper (env, map, operand) =
    match map with
    | (End StarBox) -> 
        newVars(operand); 
        newVars(I); 
        [Equation (operand, I)]
    | (End (NameBox name)) ->
            newVars(operand); 
            newVars(V name); 
        (match (Env.lookup env name) with
            | (Env.Null) -> 
                [Equation (operand, (V name))]
            | (Env.Value operand1) -> [Equation (operand, operand1)])
    | (Branch (map1, map2)) ->
        let var1 = (V (counter())) in
        (List.append (getEquationHelper (env, map1, Fn(var1, operand))) (getEquationHelper (env, map2, var1)))
    | (Guide (name, map1)) ->
        let var1 = (V (counter())) in
        let var2 = (V (counter())) in 
        let newEnv = (Env.bind env name (Env.Value var1)) in
            newVars(var1); 
	        Equation(operand, (Fn (var1, var2)))::getEquationHelper(newEnv, map1, var2)
;;

let rec getEquations map =
    getEquationHelper(Env.empty, map, V "TAU")
;;

(*solution * operand) -> operand *)
let rec substituteOperand (solutionList : solution list) (operand : operand) : operand=
    let rec helper solution operand =
    match operand with
    | I -> I
    | (Fn (operand1, operand2)) -> Fn ((helper solution operand1), (helper solution operand2))
    | (V x) ->
    (match solution with
        | (Map (operand1, operand2)) -> if operand = operand1
            then operand2
            else operand) in
    List.fold_left (fun operand solution -> helper solution operand) operand solutionList
;;

let rec substituteEquation solutionList (Equation (operand1, operand2)) : equation =
    Equation((substituteOperand solutionList operand1), (substituteOperand solutionList operand2))
;;

let rec substituteEquations (solutionList:solution list) (equationList:equation list) : equation list=
    List.map (substituteEquation solutionList) equationList
;;

let rec isElementOf (operand1 : operand) (operand2 : operand) : bool =
  match operand2 with
  | Fn (newOp1, newOp2) -> (isElementOf operand1 newOp1) || (isElementOf operand1 newOp2)
  | x -> x = operand1
;;


let rec applySolution (solution, operand) =
    match operand with
    | I -> I
    | (Fn (operand1, operand2)) -> Fn ((applySolution (solution, operand1)), (applySolution (solution, operand2)))
    | (V x) ->
        (match solution with
        | (Map (operand1, operand2)) -> if operand = operand1
        then operand2
        else operand)
;;

let rec applySolutionList (solutionList, operand) =
    let helper operand solution = applySolution(solution, operand) in
    (List.fold_left
     helper
    operand
    solutionList)
;;

(* solutionList * solutionList -> solutionList *)
let rec applySolutionListToSolutionList (solutionList1, solutionList2) =
    let rec applySolutionToSolutionList (solution, solutionList) =
        solution::(List.map
            (function (Map (operand1, operand2)) ->
            Map (operand1, applySolution(solution, operand2)))
        solutionList) in

    let helper result solution = (List.append (applySolutionToSolutionList(solution, solutionList2)) result) in
        (List.fold_left
        helper
        []
        solutionList1)
;;



(* operand * operand -> solutionList *)
let rec unify ((operand1:operand), (operand2:operand)) : solution list =
    if (operand1 = operand2)
        then []
        else
            (match (operand1, operand2) with
            | (Fn (t1, t2), Fn (t3, t4)) ->
                let s1 = unify(t1, t3) in
                let s2 = unify((substituteOperand s1 t2), (substituteOperand s1 t4)) in
                applySolutionListToSolutionList(s2, s1)
           | (V a, t) -> 
            if isElementOf (V a) t 
                then raise IMPOSSIBLE
                else [Map ((V a), t)]
           | (t, V a) -> 
            if isElementOf (V a) t 
                then raise IMPOSSIBLE
                else [Map ((V a), t)]
           | _ -> raise IMPOSSIBLE)
;;
(* equationList * solutionList -> solutionList *)
let rec unifyAll(equationList, solutionList) =
    match equationList with
    | [] -> solutionList
    | Equation(operand1, operand2)::[] -> applySolutionListToSolutionList((unify(operand1, operand2)), solutionList)
    | equation::tl ->  
        let newSolutionList = unifyAll([equation], solutionList) in
        let newEqutionList = substituteEquations newSolutionList tl in
            unifyAll(newEqutionList, newSolutionList)
;;

let rec remove_dups lst = match lst with
    | [] -> []
    | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))
;;

let rec eval solutions =
	let rec makeKey operand =
	    match operand with
	    | I -> Bar
	    | (V x) -> Bar
	    | Fn (op1, op2) -> (Node ((makeKey op1), (makeKey op2))) in

    List.map (function (Map (op1, op2)) -> (makeKey op2)) solutions
;;

let getSol map = 
    let eq5 = getEquations(map) in
    let sol5 = unifyAll(eq5, []) in sol5
;;

let getReady map =
    clearVars();
    let eq5 = getEquations(map) in
    let sol5 = unifyAll(eq5, []) in 
    let dups = eval(List.filter (fun (Map (op1, op2)) -> ((not (op1 = (V "TAU"))) && (List.mem op1 !vars))) sol5) in
    remove_dups dups
;;

Branch (Guide ("1", Guide ("3", Branch (End (NameBox "3"), End (StarBox)))), End (NameBox "2")) 
