(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * Lambda Calculus
 *)

module Evaluator =
  struct
	exception Error of string

  type substitution = (string * Lambda.lexp) list

  let rec generateString: string list -> string =
    fun sl ->
      let rec convert: int -> string list -> string =
        fun i csl ->
          let p = i / 26 in
          let q = i mod 26 in
          let s = String.make 1 (char_of_int (97+q)) in
          if p = 0
          then String.concat "" (csl @ [s])
          else convert p (csl @ [s]) in
      let rec generateString_aux: int -> string =
        fun i ->
          let newString = convert i [] in
          if List.exists (fun x ->
                           String.compare newString x = 0)
                         sl
          then generateString_aux (i + 1)
          else newString in
      generateString_aux 0

  let rec fv_lexp: Lambda.lexp -> string list =
    fun l ->
      match l with
      | Lambda.Id s ->
          [s]
      | Lambda.Lam (s, e) ->
          List.filter (fun x -> x <> s) (fv_lexp e)
      | Lambda.App (e1, e2) ->
          ((fv_lexp e1) @ (fv_lexp e2))

  let fv_sub: substitution -> string list =
    fun sub ->
      List.flatten (List.map fv_lexp
                             (List.map snd sub))

  let supp: substitution -> string list =
    fun sub ->
      List.map fst
      (
      List.filter (fun x ->
                    match x with
                    | (s1, Lambda.Id s2) when s1 = s2 -> false
                    | _ -> true)
                  sub)

  let rec substitute: substitution -> Lambda.lexp -> Lambda.lexp =
    fun sub ltarget ->
      match ltarget with
      | Lambda.Id x ->
          if (List.exists (fun s -> fst s = x) sub)
          then snd (List.find (fun s -> fst s = x) sub)
          else ltarget
      | Lambda.Lam (x, e) ->
          let appeared = [x] @
                         (fv_lexp ltarget) @
                         (fv_sub sub) in
          let y = generateString appeared in
          Lambda.Lam (y, substitute ((x, Lambda.Id y)::sub) e)
      | Lambda.App (e1, e2) ->
          Lambda.App (substitute sub e1, substitute sub e2)

  let rec checkRedex: Lambda.lexp -> bool =
    fun l ->
      match l with
      | Lambda.App (Lambda.Lam _, _) ->
          true
      | Lambda.App (e1, e2) ->
          (checkRedex e1) || (checkRedex e2)
      | Lambda.Lam (_, e) ->
          (checkRedex e)
      | _ -> false

  let rec betaReduction: Lambda.lexp -> Lambda.lexp =
    fun l ->
      match l with
      | Lambda.App (Lambda.Lam (x, e1), e2) ->
          substitute [(x, e2)] e1
      | Lambda.App (e1, e2) ->
          if (checkRedex e1)
          then Lambda.App (betaReduction e1, e2)
          else Lambda.App (e1, betaReduction e2)
      | Lambda.Lam (x, e) ->
          Lambda.Lam (x, betaReduction e)
      | _ -> l

	let rec reduce : Lambda.lexp -> Lambda.lexp =
    fun exp ->
      if checkRedex exp
      then reduce (betaReduction exp)
      else exp

  end
