(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * Lambda Calculus
 *)

module Evaluator =
  struct
	exception Error of string

    type subtstitution = Subs of string * Lambda.lexp

	let counter =
	  let count = ref (-1) in
	    fun () -> incr count; "#" ^ string_of_int(!count)

    let makeSubstitution formalParam exp1 = (Subs (formalParam, exp1))

    let rec applySubs subst lexp =
        let Subs (targetString, newExp) = subst in
        match lexp with
        | Lambda.Id id -> if (id = targetString) then newExp else (Lambda.Id id)
        | Lambda.Lam (paramName, newLexp) ->
            let newParamName = counter() in
            let newSubs = Subs (paramName, Lambda.Id newParamName) in
            let newLam = Lambda.Lam (newParamName, (applySubs subst (applySubs newSubs newLexp))) in
                newLam
        | Lambda.App (newLexp1, newLexp2) -> Lambda.App((applySubs subst newLexp1), (applySubs subst newLexp2))

  	let rec print_lexp exp =
		match exp with
		| Lambda.Id s -> print_string s
		| Lambda.Lam (s, e) -> print_string "\\"; print_string (s^"."); print_lexp e; print_string ""
		| Lambda.App (e1, e2) -> print_string "("; print_lexp e1; print_string ") ("; print_lexp e2; print_string ")"

    let print_subst subst =
        match subst with
        | Subs (str, lexp) -> print_string str; print_string " | "; print_lexp lexp

    let betaReduction formalParam body exp =
        let substitution = (makeSubstitution formalParam exp) in
        let newLexp = (applySubs substitution body) in
            newLexp

	let rec reduce : Lambda.lexp -> Lambda.lexp
	= fun exp ->
    match exp with
    | Lambda.App ((Lambda.Lam (formalParam, body)), exp1) ->
            (reduce (betaReduction formalParam body exp1))
    | Lambda.App (exp1, exp2) ->
        (match exp1 with
            | Lambda.App ((Lambda.Lam (formalParam, body)), exp3) ->
                let newLexp = Lambda.App( (betaReduction formalParam body exp3), exp2) in
                    (reduce newLexp)
            | _ -> (match exp2 with 
                | Lambda.App ((Lambda.Lam (formalParam, body)), exp3) ->
                    let newLexp = Lambda.App( exp1, (betaReduction formalParam body exp3)) in
                    (reduce newLexp)
                | _ -> Lambda.App((reduce exp1), (reduce exp2))))
            (*kA
        print_endline "APP";
        let r1 = (reduce exp1) in
            let result = (match r1 with
                | Lambda.Lam _ -> print_endline "APP1"; reduce (Lambda.App (r1, exp2))
                | _ -> print_endline "APP2"; Lambda.App(r1, (reduce exp2))) in
                print_endline "@@@@@@@@@@RESULT@@@@@@@@";
                print_lexp result;
                print_endline "";
                print_endline "@@@@@@@@@@@@@@@@@@@@@@@@";
                result
                *)


    | Lambda.Lam (id, exp1) -> 
        Lambda.Lam (id, (reduce exp1))
    | Lambda.Id id -> 
        Lambda.Id id
  end
