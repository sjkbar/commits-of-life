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

    let isBetaReducible lexp =
        match lexp with
        | Lambda.App ((Lambda.Lam _), _) -> true
        | _ -> false

    let betaReduction lexp =
        match lexp with
        | Lambda.App ((Lambda.Lam (formalParam, body)), exp) ->
            let substitution = (makeSubstitution formalParam exp) in
            let newLexp = (applySubs substitution body) in
                newLexp
        | _ -> raise (Error "betaReduction fail")

	let rec reduce : Lambda.lexp -> Lambda.lexp
	= fun exp ->
    if (isBetaReducible exp)
        then (
            let newTotalLexp = (betaReduction exp) in
            (reduce newTotalLexp)
        )
        else (
            match exp with
            | Lambda.App (exp1, exp2) ->
                if (isBetaReducible exp1)
                    then (
                        let newLexp1 = (betaReduction exp1) in
                        let newTotalLexp = Lambda.App (newLexp1, exp2) in
                        (reduce newTotalLexp)
                    )
                    else (
                        if (isBetaReducible exp2)
                            then (
                                let newLexp2 = (betaReduction exp2) in
                                let newTotalLexp = Lambda.App (exp1, newLexp2) in
                                newTotalLexp
                            )
                            else (
                                let newTotalLexp = (Lambda.App ((reduce exp1), (reduce exp2))) in
                                if( isBetaReducible newTotalLexp )
                                    then (
                                        (reduce newTotalLexp)
                                    )
                                    else (
                                        newTotalLexp
                                    )
                            )
                    )
            | Lambda.Lam (id, exp1) ->
                    Lambda.Lam (id, (reduce exp1))
            | Lambda.Id id ->
                Lambda.Id id
        )
(*
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
                        newLexp
                | _ ->
                    let newLexp = Lambda.App((reduce exp1), (reduce exp2)) in
                        (match newLexp with
                        | Lambda.App ((Lambda.Lam (formalParam, body)), exp3) -> (betaReduction formalParam body exp3)
                        | _ -> newLexp)))
    | Lambda.Lam (id, exp1) ->
            Lambda.Lam (id, (reduce exp1))
    | Lambda.Id id ->
        Lambda.Id id
        *)
  end
