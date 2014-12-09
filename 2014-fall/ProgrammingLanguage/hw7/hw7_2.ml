(*
 * SNU 4190.310 Programming Languages (Fall 2012)
 *
 * Let-polymorphism with M algorithm
 *)
open M

module M_PolyChecker : M_PolyChecker = struct
(* Environment Implementation *)
    type type_var = TypeVar of string
    | TypeInt                     (* integer type *)
    | TypeBool                    (* boolean type *)
    | TypeString                  (* string type *)
    | TypePair of type_var * type_var   (* pair type *)
    | TypeLoc of type_var           (* location type *)
    | TypeArrow of type_var * type_var  (* function type *)

    let rec print_type_var (type_var:type_var) : string =
        (match type_var with
        | TypeVar str -> 
            let ret = (String.concat "" ["("; "TypeVar"; " ";  str; ")"]) in
            ret
        | TypeInt ->
            "TypeInt";
        | TypeBool ->
            "TypeBool";
        | TypeString ->
            "TypeString";
        | TypePair (tv1, tv2) ->
            let ret = (String.concat "" ["("; "TypePair"; "(";  (print_type_var tv1); ", "; (print_type_var tv2); "))"]) in
            ret
        | TypeLoc tv1 ->
            let ret = (String.concat "" ["("; "TypeLoc"; "(";  (print_type_var tv1); "))"]) in
            ret
        | TypeArrow (tv1, tv2) ->
            let ret = (String.concat "" ["("; "TypeArrow"; "(";  (print_type_var tv1); ", "; (print_type_var tv2); "))"]) in
            ret)

    type type_scheme = Type of type_var
    | TypeScheme of string list * type_scheme

    type type_equations =
    | TypeEquation of type_var *type_var
    | TypeAnd of type_equations * type_equations

    type substitution = Arrow of type_var * type_var
    type substitutions = substitution list

    let rec print_substitution ((Arrow (type_var1, type_var2)):substitution) : string =
        String.concat " " ["Arrow"; "("; (print_type_var type_var1); ","; (print_type_var type_var2); ")"]
    
    let rec print_substitutions (substitutions:substitutions) : string =
        match substitutions with
        | [] -> " "
        | hd::tl -> String.concat " " [(print_substitution hd); (print_substitutions tl)]

    let rec print_str_list (str_list :string list) : string=
        (match str_list with
        | [] -> " "
        | hd::tl -> String.concat " " [hd; (print_str_list tl)])

    let rec print_type_scheme (type_scheme:type_scheme) : string =
        (match type_scheme with
        | Type type_var -> (print_type_var type_var)
        | TypeScheme (str_list, type_scheme) ->
            (String.concat " " ["TypeScheme"; "("; (print_str_list str_list); ",";  (print_type_scheme type_scheme); ")"]))

    module Env =
    struct
        type t = (id, type_scheme) Hashtbl.t
        let empty = Hashtbl.create 11111;;
        let lookup env id = 
            try 
                (Hashtbl.find env id)
            with Not_found ->
                raise (RuntimeError "not bound")
        let bind env id type_scheme= (Hashtbl.replace env id type_scheme); env
        let fold f env init = Hashtbl.fold f env init
        let iter f tbl = Hashtbl.iter f tbl
    end;;

    module Env2 =
    struct
        type t = E of (id -> type_scheme)
        let empty = E(fun x -> raise (RuntimeError "not bound"))
        let lookup (E(env)) id = env id
        let bind (E(env)) id type_scheme = 
            E(fun x -> if x = id then type_scheme else env x)
    end;;

    let result_var = ref (TypeVar "__result")
    let eqTypeVarList = ref []
    let writeTypeVarList = ref []

    let counter =
    let count = ref (-1) in
      fun () -> incr count; "_" ^ string_of_int(!count)

    let rec free_type_var_of_type_var (type_var:type_var) : string list =
        (match type_var with
        | TypeVar str -> [str]
        | TypeInt -> []
        | TypeBool -> []
        | TypeString -> []
        | TypePair (type_var1, type_var2) -> 
            let list1 = (free_type_var_of_type_var type_var1) in
            let list2 = (free_type_var_of_type_var type_var2) in
            (List.append list1 list2)
        | TypeLoc type_var -> (free_type_var_of_type_var type_var)
        | TypeArrow (type_var1, type_var2) ->
            let list1 = (free_type_var_of_type_var type_var1) in
            let list2 = (free_type_var_of_type_var type_var2) in
            (List.append list1 list2))


    let rec free_type_var_of_type_scheme (type_scheme:type_scheme) : string list = 
        (match type_scheme with
        | Type type_var -> (free_type_var_of_type_var type_var)
        | TypeScheme (str_list,  type_scheme) -> 
            let new_str_list = (free_type_var_of_type_scheme type_scheme) in
            (List.filter (fun str -> not (List.mem str str_list)) new_str_list))


    let rec free_type_var (type_scheme:type_scheme) (env:Env.t) : string list = 
        let str_list = (free_type_var_of_type_scheme type_scheme) in
        (List.filter (fun str -> try let _ = (Env.lookup env str) in false with (RuntimeError "not bound") -> true) str_list)
  
    let remove_elt e l =
        let rec go l acc = match l with
            | [] -> List.rev acc
            | x::xs when e = x -> go xs acc
            | x::xs -> go xs (x::acc)
                in go l []
                  
   
    let remove_duplicates l =
        let rec go l acc = match l with
            | [] -> List.rev acc
            | x :: xs -> go (remove_elt x xs) (x::acc)
                in go l []
    
    let rec generalize (type_var:type_var) (env:Env.t) : type_scheme =
        let str_list = (free_type_var (Type type_var) env) in
        let remove_duplicate_str_list = remove_duplicates str_list in
        (TypeScheme (remove_duplicate_str_list, (Type type_var)))

    let rec generate_substitutions_for_type_scheme (str_list:string list) : substitutions =
        (List.map 
            (fun str -> 
                let name = counter() in
                let new_type_var1 = (TypeVar (name)) in
                Arrow ((TypeVar str), new_type_var1))
            str_list)

    let rec applySubToTypeVar (substitution:substitution) (var:type_var) : type_var =
        let Arrow (from_type_var, to_type_var) = substitution in
        match var with
        | TypeVar x ->
            (match from_type_var with
            | TypeVar y ->
                if x = y
                    then
                        to_type_var
                    else
                        var
            | _ -> var)
        | TypeInt -> TypeInt
        | TypeBool -> TypeBool
        | TypeString -> TypeString
        | TypePair (type_var1, type_var2) ->
            let new_type_var1 = (applySubToTypeVar substitution type_var1) in
            let new_type_var2 = (applySubToTypeVar substitution type_var2) in
            TypePair (new_type_var1, new_type_var2)
        | TypeLoc type_var ->
            let new_type_var = (applySubToTypeVar substitution type_var) in
            (TypeLoc new_type_var)
        | TypeArrow (type_var1, type_var2) ->
            let new_type_var1 = (applySubToTypeVar substitution type_var1) in
            let new_type_var2 = (applySubToTypeVar substitution type_var2) in
            TypeArrow (new_type_var1, new_type_var2)

    let rec applySubsToTypeVar (substitutions:substitutions) (var:type_var) : type_var =
        (List.fold_left (fun var substitution -> (applySubToTypeVar substitution var)) var substitutions)

    let rec applySubsToTypeVarList (substitutions:substitutions) (type_var_list:type_var list)  =
        List.map (fun type_var -> applySubsToTypeVar substitutions type_var) type_var_list

    let rec applySubsToTypeEquations (substitutions:substitutions) (equations:type_equations) : type_equations =
    match equations with
    | TypeEquation (type_var1, type_var2) ->
        let new_type_var1 = (applySubsToTypeVar substitutions type_var1) in
        let new_type_var2 = (applySubsToTypeVar substitutions type_var2) in
        TypeEquation (new_type_var1, new_type_var2)
    | TypeAnd (type_equations1, type_equations2) ->
        TypeAnd((applySubsToTypeEquations substitutions type_equations1), (applySubsToTypeEquations substitutions type_equations2))

    let rec updateEqList (old_type_var:type_var) (new_type_var:type_var) : unit =
        match (old_type_var, new_type_var) with
            | (TypeVar x), (TypeVar y) ->
                let isMember = (List.mem old_type_var !eqTypeVarList) in
                if isMember then (eqTypeVarList := (new_type_var::!eqTypeVarList)) else (())
            | TypeInt, TypeInt -> ()
            | TypeBool, TypeBool -> ()
            | TypeString, TypeString -> ()
            | (TypePair (old_type_var1, old_type_var2)), (TypePair (new_type_var1, new_type_var2)) -> 
                (updateEqList old_type_var1 new_type_var1);
                (updateEqList old_type_var2 new_type_var2);
            | (TypeLoc old_type_var), (TypeLoc new_type_var) ->
                (updateEqList old_type_var new_type_var)
            | (TypeArrow (old_type_var1, old_type_var2)), (TypeArrow (new_type_var1, new_type_var2)) -> 
                (updateEqList old_type_var1 new_type_var1);
                (updateEqList old_type_var2 new_type_var2);
            | _ -> ()

    let rec updateWriteList (old_type_var:type_var) (new_type_var:type_var) : unit =
        match (old_type_var, new_type_var) with
            | (TypeVar x), (TypeVar y) ->
                let isMember = (List.mem old_type_var !writeTypeVarList) in
                if isMember then writeTypeVarList := (new_type_var::!writeTypeVarList) else  ()
            | TypeInt, TypeInt -> ()
            | TypeBool, TypeBool -> ()
            | TypeString, TypeString -> ()
            | (TypePair (old_type_var1, old_type_var2)), (TypePair (new_type_var1, new_type_var2)) -> 
                (updateWriteList old_type_var1 new_type_var1);
                (updateWriteList old_type_var2 new_type_var2);
            | (TypeLoc old_type_var), (TypeLoc new_type_var) ->
                (updateWriteList old_type_var new_type_var)
            | (TypeArrow (old_type_var1, old_type_var2)), (TypeArrow (new_type_var1, new_type_var2)) -> 
                (updateWriteList old_type_var1 new_type_var1);
                (updateWriteList old_type_var2 new_type_var2);
            | _ -> ()

    let rec instantiate (type_scheme:type_scheme) : type_var =
        (match type_scheme with
        | Type type_var -> type_var
        | TypeScheme (str_list,  type_scheme) -> 
            let substitutions = (generate_substitutions_for_type_scheme str_list) in
            let type_var = (instantiate type_scheme) in
            let new_type_var = (applySubsToTypeVar substitutions type_var) in
            (updateEqList type_var new_type_var);
            (updateWriteList type_var new_type_var);
            new_type_var)


    let rec change_type_var_to_types (var:type_var) : types =
        (match var with
        | TypeInt -> TyInt
        | TypeBool -> TyBool
        | TypeString -> TyString
        | TypeArrow (type_var1, type_var2) -> TyArrow ((change_type_var_to_types type_var1), (change_type_var_to_types type_var2))
        | TypePair (type_var1, type_var2) -> TyPair((change_type_var_to_types type_var1), (change_type_var_to_types type_var2))
        | TypeLoc type_var -> TyLoc (change_type_var_to_types type_var)
        | TypeVar x -> raise (TypeError (String.concat " " ["change_type_var_to_types TypeVar"; x])))

    let rec unify (type_var1:type_var) (type_var2:type_var) : substitutions =
    (match (type_var1, type_var2) with
    | (TypeInt, TypeInt) -> []
    | (TypeBool, TypeBool) -> []
    | (TypeString, TypeString) -> []
    | ((TypeVar x), _) -> [Arrow (type_var1, type_var2)]
    | (_, (TypeVar x)) -> [Arrow (type_var2, type_var1)]
    | ((TypeLoc new_type_var1), (TypeLoc new_type_var2)) -> (unify new_type_var1 new_type_var2)
    | ((TypePair (type_var1, type_var2)), (TypePair (type_var3, type_var4))) ->
        let new_subs = (unify type_var1 type_var3) in
        let new_type_var2 = (applySubsToTypeVar new_subs type_var2) in
        let new_type_var4 = (applySubsToTypeVar new_subs type_var4) in
        let new_subs2 = (unify new_type_var2 new_type_var4) in
        (List.append new_subs new_subs2)
    | ((TypeArrow (type_var1, type_var2)), (TypeArrow (type_var3, type_var4))) ->
        let new_subs = (unify type_var1 type_var3) in
        let new_type_var2 = (applySubsToTypeVar new_subs type_var2) in
        let new_type_var4 = (applySubsToTypeVar new_subs type_var4) in
        let new_subs2 = (unify new_type_var2 new_type_var4) in
        (List.append new_subs new_subs2)
    | _ -> raise (TypeError "fail"))

    let rec unify_all (equations:type_equations) (substitutions:substitutions) =
        match equations with
        | TypeEquation (type_var1, type_var2) -> (List.append substitutions (unify type_var1 type_var2))
        | TypeAnd (type_equations1, type_equations2) ->
            let new_subs = (unify_all type_equations1 substitutions) in
            (unify_all (applySubsToTypeEquations new_subs type_equations2) new_subs)

    let global_subs = ref []

    let unification (equations:type_equations) : substitutions =
        (unify_all equations !global_subs)

    let applySubsToEnv (substitutions:substitutions) (env:Env.t) : Env.t =
    (Env.iter
        (fun id type_scheme ->
                match type_scheme with
                    | Type type_var -> 
                        let new_type_var = (applySubsToTypeVar substitutions type_var) in
                        (Hashtbl.replace env id (Type new_type_var))
                    | TypeScheme _ ->
                        (Hashtbl.replace env id type_scheme))
        env);
    env

    let rec generate_type_equations (env : Env.t) (exp : M.exp) (var : type_var) : type_equations =
        match exp with
        |CONST const ->
            (match const with
            | S str -> TypeEquation ((TypeString), var)
            | N num -> TypeEquation ((TypeInt), var)
            | B b -> TypeEquation ((TypeBool), var))
        | VAR id ->
            let new_type_scheme = (Env.lookup env id) in
            let new_type_var = (instantiate new_type_scheme) in
            (TypeEquation (new_type_var, var))
        | FN (id, exp) ->
            let new_type_var1 = (TypeVar (counter())) in
            let new_type_var2 = (TypeVar (counter())) in
            let new_type_equation = (TypeEquation (var, (TypeArrow (new_type_var1, new_type_var2)))) in
            let new_env = (Env.bind env id (Type new_type_var1)) in
            TypeAnd (new_type_equation, (generate_type_equations new_env exp new_type_var2))
        | APP (exp1, exp2) ->
            let new_type_var1 = (TypeVar (counter())) in
            let type_equations_1 = (generate_type_equations env exp1 (TypeArrow (new_type_var1, var))) in
            let type_equations_2 = (generate_type_equations env exp2 new_type_var1) in
            TypeAnd (type_equations_1, type_equations_2)
        | LET (decl, exp2) ->
            (match decl with
                | NREC (id, exp1) ->
                    let exp1_type_var = (TypeVar (counter())) in
                    let type_equations_1 = (generate_type_equations env exp1 exp1_type_var) in

                    (* generate type_var for let id = e1 in e2's e1 *)
                    let new_solution = (unification type_equations_1) in
                    global_subs := new_solution;
                    let result_exp1_type_var = (applySubsToTypeVar new_solution exp1_type_var) in
                    eqTypeVarList := (applySubsToTypeVarList new_solution !eqTypeVarList);
                    result_var := (applySubsToTypeVar new_solution !result_var);
                    let env = (applySubsToEnv new_solution env) in


                    (* generalize result_exp1_type_var *)
                    let generalized_exp1_type_scheme = (generalize result_exp1_type_var env) in

                    let new_env = (Env.bind env id generalized_exp1_type_scheme) in
                    let type_equations_2 = (generate_type_equations new_env exp2 (applySubsToTypeVar new_solution var)) in
                    type_equations_2
                | REC (id, exp1) ->
                    let exp1_type_var = (TypeVar (counter())) in
                    let new_env1 = (Env.bind env id (Type exp1_type_var)) in
                    let type_equations_1 = (generate_type_equations new_env1 exp1 exp1_type_var) in

                    (* generate type_var for let id = e1 in e2's e1 *)
                    let new_solution = (unification type_equations_1) in
                    global_subs := new_solution;
                    let result_exp1_type_var = (applySubsToTypeVar new_solution exp1_type_var) in
                    eqTypeVarList := (applySubsToTypeVarList new_solution !eqTypeVarList);
                    result_var := (applySubsToTypeVar new_solution !result_var);
                    let env = (applySubsToEnv new_solution env) in

                    (* generalize result_exp1_type_var *)
                    let generalized_exp1_type_scheme = (generalize result_exp1_type_var new_env1) in

                    let new_env2 = (Env.bind env id generalized_exp1_type_scheme) in
                    let type_equations_2 = (generate_type_equations new_env2 exp2 (applySubsToTypeVar new_solution var)) in
                    type_equations_2)
        | IF (exp1, exp2, exp3) ->
            let type_equations_1 = (generate_type_equations env exp1 TypeBool) in
            let type_equations_2 = (generate_type_equations env exp2 var) in
            let type_equations_3 = (generate_type_equations env exp3 var) in
            TypeAnd(type_equations_1, TypeAnd(type_equations_2, type_equations_3))
        | BOP (bop, exp1, exp2) ->
            (match bop with
            | ADD ->
                let type_equations_1 = TypeEquation (TypeInt, var) in
                let type_equations_2 = (generate_type_equations env exp1 TypeInt) in
                let type_equations_3 = (generate_type_equations env exp2 TypeInt) in
                TypeAnd (type_equations_1, TypeAnd(type_equations_2, type_equations_3))
            | SUB ->
                let type_equations_1 = TypeEquation (TypeInt, var) in
                let type_equations_2 = (generate_type_equations env exp1 TypeInt) in
                let type_equations_3 = (generate_type_equations env exp2 TypeInt) in
                TypeAnd (type_equations_1, TypeAnd(type_equations_2, type_equations_3))
            | AND ->
                let type_equations_1 = TypeEquation (TypeBool, var) in
                let type_equations_2 = (generate_type_equations env exp1 TypeBool) in
                let type_equations_3 = (generate_type_equations env exp2 TypeBool) in
                TypeAnd (type_equations_1, TypeAnd(type_equations_2, type_equations_3))
            | OR ->
                let type_equations_1 = TypeEquation (TypeBool, var) in
                let type_equations_2 = (generate_type_equations env exp1 TypeBool) in
                let type_equations_3 = (generate_type_equations env exp2 TypeBool) in
                TypeAnd (type_equations_1, TypeAnd(type_equations_2, type_equations_3))
            | EQ ->
                let type_equations_1 = TypeEquation (TypeBool, var) in
                let name = counter() in
                let new_type_var1 = (TypeVar (name)) in
                let type_equations_2 = (generate_type_equations env exp1 new_type_var1) in
                let type_equations_3 = (generate_type_equations env exp2 new_type_var1) in
                eqTypeVarList := (new_type_var1::(!eqTypeVarList));
                TypeAnd (type_equations_1, TypeAnd(type_equations_2, type_equations_3)))
        | READ ->
                TypeEquation (TypeInt, var)
        | WRITE exp ->
            writeTypeVarList := (var::(!writeTypeVarList));
            (generate_type_equations env exp var)
        | MALLOC exp ->
            let new_type_var1 = (TypeVar (counter())) in
            let type_equations_1 = (TypeEquation (var, (TypeLoc new_type_var1))) in
            let type_equations_2 = (generate_type_equations env exp new_type_var1) in
            TypeAnd (type_equations_1, type_equations_2)
        | ASSIGN (exp1, exp2) ->
            let type_equations_1 = (generate_type_equations env exp1 (TypeLoc var)) in
            let type_equations_2 = (generate_type_equations env exp2 var) in
            TypeAnd (type_equations_1, type_equations_2)
        | BANG exp ->
            (generate_type_equations env exp (TypeLoc var))
        | SEQ (exp1, exp2) ->       (*   e ; e    *)
            let new_type_var1 = (TypeVar (counter())) in
            let type_equations_1 = (generate_type_equations env exp1 new_type_var1) in
            let type_equations_2 = (generate_type_equations env exp2 var) in
            TypeAnd (type_equations_1, type_equations_2)
        | PAIR (exp1, exp2) ->      (*   (e, e)   *)
            let new_type_var1 = (TypeVar (counter())) in
            let new_type_var2 = (TypeVar (counter())) in
            let type_equations_1 = (TypeEquation (var, (TypePair (new_type_var1, new_type_var2)))) in
            let type_equations_2 = (generate_type_equations env exp1 new_type_var1) in
            let type_equations_3 = (generate_type_equations env exp2 new_type_var2) in
            TypeAnd (type_equations_1, TypeAnd(type_equations_2, type_equations_3))
        | SEL1 exp ->            (*   e.1      *)
            let new_type_var1 = (TypeVar (counter())) in
            let new_type_var2 = (TypeVar (counter())) in
            let type_equations_1 = (TypeEquation (var, new_type_var1)) in
            let type_equations_2 = (generate_type_equations env exp (TypePair (new_type_var1, new_type_var2))) in
            TypeAnd (type_equations_1, type_equations_2)
        | SEL2 exp ->           (*   e.2      *)
            let new_type_var1 = (TypeVar (counter())) in
            let new_type_var2 = (TypeVar (counter())) in
            let type_equations_1 = (TypeEquation (var, new_type_var2)) in
            let type_equations_2 = (generate_type_equations env exp (TypePair (new_type_var1, new_type_var2))) in
            TypeAnd (type_equations_1, type_equations_2)

    let checkEqList (solution:substitutions) =
        List.filter
            (fun eqTypeVar ->
                (match (applySubsToTypeVar solution eqTypeVar) with
                | TypePair _ -> raise (TypeError "EqList fail : Pair")
                | TypeArrow(_,_) -> raise (TypeError "EqList fail : Arrow")
                | _ -> false))
            (!eqTypeVarList)

    let checkWriteList (solution:substitutions) =
        List.filter
            (fun writeTypeVar ->
                (match (applySubsToTypeVar solution writeTypeVar) with
                | TypePair _ -> raise (TypeError "writeList fail : Pair")
                | TypeArrow _ -> raise (TypeError "writeList fail : Arrow")
                | TypeLoc _ -> raise (TypeError "writeList fail : Loc")
                | _ -> false))
            (!writeTypeVarList)

    let rec check exp =
        let equations = (generate_type_equations Env.empty exp !result_var) in
        let solution = (unification equations) in
        let result_type_var = (applySubsToTypeVar solution !result_var) in
        (checkEqList solution);
        (checkWriteList solution);
        (change_type_var_to_types result_type_var)
end
