(*
 * SNU 4190.310 Programming Languages 
 *
 * M0
 *)
open M
module Encoder = 
  struct
  	exception Error of string

  	let rec pp exp =
		match exp with
		| Lambda.Id s -> print_string s
		| Lambda.Lam (s, e) -> print_string "(\\"; print_string (s^".("); pp e; print_string "))"
		| Lambda.App (e1, e2) -> print_string "("; pp e1; print_string ") ("; pp e2; print_string ")"

	let rec encode : M.mexp -> Lambda.lexp
	= fun pgm -> 
    let rec encode_num_helper int1 =
        if int1 = 0
            then
                Lambda.Id("x")
            else
                Lambda.App(Lambda.Id("f"), encode_num_helper(int1 - 1)) in

    let rec encode_num int1 =
        let inner_lexp = (encode_num_helper int1) in
            Lambda.Lam("f", Lambda.Lam("x", inner_lexp)) in

    let rec encode_add_func =
        Lambda.Lam("n", Lambda.Lam("m", Lambda.Lam("f", Lambda.Lam ("x", 
            Lambda.App(
                Lambda.App(Lambda.Id "n", Lambda.Id "f"),
                Lambda.App(
                    (Lambda.App (Lambda.Id "m", Lambda.Id "f")),
                    (Lambda.Id "x"))
                )
            )
        ))) in

    let encode_predecessor_func = 
        Lambda.Lam ("n",
          Lambda.Lam ("f",
            Lambda.Lam ("x",
              Lambda.App (
                Lambda.App (
                  Lambda.App (
                    Lambda.Id "n", 
                    Lambda.Lam ("g",
                      Lambda.Lam ("h",
                        Lambda.App (
                          Lambda.Id "h", 
                          Lambda.App (
                            Lambda.Id "g", 
                            Lambda.Id "f"
                          )
                        )
                      )
                    )
                  ), 
                  Lambda.Lam ("u",
                    Lambda.Id "x"
                  )
                ), 
                Lambda.Lam ("u",
                  Lambda.Id "u"
                )
              )
            )
          )
        )
        in

    let encode_sub_func
        = Lambda.Lam ("m",
          Lambda.Lam ("n",
            Lambda.App (
              Lambda.App (
                Lambda.Id "n", 
                encode_predecessor_func
              ), 
              Lambda.Id "m"
            )
          )
        )
        in

    let rec encode_fn id lexp =
        Lambda.Lam(id, lexp) in

    let rec encode_app lexp1 lexp2 =
        Lambda.App(lexp1, lexp2) in

    let rec encode_var id =
        Lambda.Id id in 

    let rec encode_pair_func =
        Lambda.Lam ("x",
          Lambda.Lam ("y",
            Lambda.Lam ("z",
              Lambda.App (
                Lambda.App (
                  Lambda.Id "z", 
                  Lambda.Id "x"
                ), 
                Lambda.Id "y"
              )
            )
          )
        )
        in

    let encode_fst_func =
        Lambda.Lam ("p",
          Lambda.App (
            Lambda.Id "p", 
            Lambda.Lam ("x",
              Lambda.Lam ("y",
                Lambda.Id "x"
              )
            )
          )
        )
        in

    let encode_snd_func =
        Lambda.Lam ("p",
          Lambda.App (
            Lambda.Id "p", 
            Lambda.Lam ("x",
              Lambda.Lam ("y",
                Lambda.Id "y"
              )
            )
          )
        )
        in

    let encode_isz_func =
        Lambda.Lam ("n",
          Lambda.Lam ("x",
            Lambda.Lam ("y",
              Lambda.App (
                Lambda.App (
                  Lambda.Id "n", 
                  Lambda.Lam ("z",
                    Lambda.Id "y"
                  )
                ), 
                Lambda.Id "x"
              )
            )
          )
        ) in

    let y_combinator =
		Lambda.Lam ("f",
		  Lambda.App (
		    Lambda.Lam ("x",
		      Lambda.App (
		        Lambda.Id "f", 
		        Lambda.App (
		          Lambda.Id "x", 
		          Lambda.Id "x"
		        )
		      )
		    ), 
		    Lambda.Lam ("x",
		      Lambda.App (
		        Lambda.Id "f", 
		        Lambda.App (
		          Lambda.Id "x", 
		          Lambda.Id "x"
		        )
		      )
		    )
		  )
		) in

    let encode_and lexp1 lexp2 =
		Lambda.App (
		  Lambda.App (
		    Lambda.App (
		      Lambda.Lam ("n",
		        Lambda.Lam ("x",
		          Lambda.Lam ("y",
		            Lambda.App (
		              Lambda.App (
		                Lambda.Id "n", 
		                Lambda.Lam ("z",
		                  Lambda.Id "y"
		                )
		              ), 
		              Lambda.Id "x"
		            )
		          )
		        )
		      ), 
		      lexp1
		    ), 
		    Lambda.Lam ("f",
		      Lambda.Lam ("x",
		        Lambda.Id "x"
		      )
		    )
		  ), 
		  Lambda.App (
		    Lambda.App (
		      Lambda.App (
		        Lambda.Lam ("n",
		          Lambda.Lam ("x",
		            Lambda.Lam ("y",
		              Lambda.App (
		                Lambda.App (
		                  Lambda.Id "n", 
		                  Lambda.Lam ("z",
		                    Lambda.Id "y"
		                  )
		                ), 
		                Lambda.Id "x"
		              )
		            )
		          )
		        ), 
		        lexp2
		      ), 
		      Lambda.Lam ("f",
		        Lambda.Lam ("x",
		          Lambda.Id "x"
		        )
		      )
		    ), 
		    Lambda.Lam ("f",
		      Lambda.Lam ("x",
		        Lambda.App (
		          Lambda.Id "f", 
		          Lambda.Id "x"
		        )
		      )
		    )
		  )
		) in

    match pgm with
        | Num (int1) -> encode_num int1
        | Add (mexp1, mexp2) -> 
                Lambda.App (Lambda.App(encode_add_func, (encode mexp1)), (encode mexp2))
		| Sub (mexp1,  mexp2) ->
                Lambda.App (Lambda.App(encode_sub_func, (encode mexp1)), (encode mexp2))
        | Var id ->
            encode_var id
        | Fn (id,  mexp) ->
            let lexp1 = encode(mexp) in
                encode_fn id lexp1
        | App (mexp1, mexp2) ->
            let lexp1 = encode(mexp1) in
            let lexp2 = encode(mexp2) in
                encode_app lexp1 lexp2
        | Pair (mexp1, mexp2) ->
                Lambda.App (Lambda.App(encode_pair_func, (encode mexp1)), (encode mexp2))
        | Fst (mexp) ->
                Lambda.App(encode_fst_func, (encode mexp))
        | Snd (mexp) ->
                Lambda.App(encode_snd_func, (encode mexp))
        | Ifz (mexp1, mexp2, mexp3) ->
            let lexp1 = Lambda.App(encode_isz_func, encode(mexp1)) in
            let lexp2 = encode(mexp2) in
            let lexp3 = encode(mexp3) in
                Lambda.App(
                    Lambda.App(lexp1, lexp2),
                    lexp3)
        | And (mexp1, mexp2) ->
            encode_and (encode mexp1) (encode mexp2)
        | Rec (id1, id2, mexp) ->
            let f = Lambda.Lam(id1, Lambda.Lam (id2, encode(mexp))) in
                Lambda.App (y_combinator, f)

  end
