(*
 * SNU 4190.310 Programming Languages 
 *
 * Sonata
 *)
open Sm5
open Sonata
module Rozetta = struct
  exception UnSupportedOperation

  let rec trans : Sm5.command -> Sonata.command =
          let counter =
            let count = ref (-1) in
              fun () -> incr count; "@" ^ string_of_int(!count) in

		  let rec trans_value_obj value =
		    (match value with
		        | Sm5.Z int1 -> Sonata.Z int1
		        | Sm5.B bool1 -> Sonata.B bool1
		        | Sm5.Unit -> Sonata.Unit
		        | _ -> raise UnSupportedOperation) in

		  let rec trans_push_cmd sm5_obj =
		    let sonata_obj = (match sm5_obj with
		        | Sm5.Val value -> let sonata_value = (trans_value_obj value)
		            in Sonata.Val sonata_value
		        | Sm5.Id  str -> Sonata.Id str
		        | Sm5.Fn (str, command) ->
                    let randomName = counter() in
                    let result = String.concat "_" [randomName; "result"] in
                    let loc = String.concat "_" [randomName; "loc"] in
                    let value = String.concat "_" [randomName; "value"] in
                    let kontinum = String.concat "_" [randomName; "kontinum"] in

                    let change_result_function_arg_order =
                        [Sm5.BIND result; Sm5.BIND loc; Sm5.BIND value; Sm5.BIND kontinum]
                        @ [Sm5.PUSH (Sm5.Id result); Sm5.PUSH(Sm5.Id kontinum); Sm5.PUSH (Sm5.Id value); Sm5.PUSH (Sm5.Id loc)] in
                    let call = [Sm5.CALL] in
                    let new_command = (trans (command @ change_result_function_arg_order @ call)) in
                        Sonata.Fn(str, new_command)) in
		    Sonata.PUSH sonata_obj in

		  let rec trans_jtr_cmd command1 command2 =
		    [Sonata.JTR ((trans command1), (trans command2))] in

		  let rec trans_box_cmd int1 =
		    Sonata.BOX int1 in

		  let rec trans_unbox_cmd str =
		    Sonata.UNBOX str in

		  let rec trans_bind_cmd str =
		    Sonata.BIND str in

          let rec trans_call_cmd rest_sonata_command = 
                let randomName = counter() in
                let loc = String.concat "_" [randomName; "loc"] in
                let value = String.concat "_" [randomName; "value"] in
                let func1 = String.concat "_" [randomName; "function1"] in
                let func2 = String.concat "_" [randomName; "function2"] in
                let kontinum_formal_param = String.concat "_" [randomName; "kontinum_formal_param"] in

                let bind_functin_args = [Sonata.BIND loc; Sonata.BIND value; Sonata.BIND func1; Sonata.BIND func2] in
                let push_kontinum_function_args = [Sonata.PUSH(Sonata.Fn (kontinum_formal_param, rest_sonata_command)); Sonata.PUSH (Sonata.Val (Sonata.Z 1111)); Sonata.MALLOC] in
                let push_binded_function_args =  [Sonata.PUSH(Sonata.Id func2); Sonata.PUSH(Sonata.Id func1); Sonata.PUSH (Sonata.Id value); Sonata.PUSH (Sonata.Id loc)] in
                let call = [Sonata.CALL] in

                bind_functin_args @ push_kontinum_function_args @ push_binded_function_args @ call
          in

    fun command ->
        match command with
        | [] -> []
	    | (Sm5.PUSH obj)::tl -> trans_push_cmd obj::trans(tl)
	    | Sm5.POP::tl -> Sonata.POP::trans(tl);
	    | Sm5.STORE::tl -> Sonata.STORE::trans(tl);
	    | Sm5.LOAD::tl -> Sonata.LOAD::trans(tl);
	    | (Sm5.JTR (command1, command2))::tl ->  [Sonata.JTR ((trans (command1@tl)), (trans (command2@tl)))];
	    | Sm5.MALLOC::tl -> Sonata.MALLOC::trans(tl);
	    | (Sm5.BOX int1)::tl -> (trans_box_cmd int1)::trans(tl);
	    | (Sm5.UNBOX str)::tl -> (trans_unbox_cmd str)::trans(tl);
	    | (Sm5.BIND str)::tl -> (trans_bind_cmd str)::trans(tl);
	    | (Sm5.UNBIND)::tl -> (Sonata.UNBIND)::trans(tl);
	    | (Sm5.GET)::tl -> (Sonata.GET)::trans(tl);
	    | (Sm5.PUT)::tl -> (Sonata.PUT)::trans(tl);
	    | (Sm5.CALL)::[] -> [Sonata.CALL];
	    | (Sm5.CALL)::tl -> 
                    (trans_call_cmd (trans tl)) 
	    | (Sm5.ADD)::tl -> (Sonata.ADD)::trans(tl);
	    | (Sm5.SUB)::tl -> (Sonata.SUB)::trans(tl);
	    | (Sm5.MUL)::tl -> (Sonata.MUL)::trans(tl);
	    | (Sm5.DIV)::tl -> (Sonata.DIV)::trans(tl);
	    | (Sm5.EQ)::tl -> (Sonata.EQ)::trans(tl);
	    | (Sm5.LESS)::tl -> (Sonata.LESS)::trans(tl);
	    | (Sm5.NOT)::tl -> (Sonata.NOT)::trans(tl);
end
