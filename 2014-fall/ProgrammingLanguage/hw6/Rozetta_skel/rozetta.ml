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
              fun () -> incr count; "!@#^" ^ string_of_int(!count) in

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
                    let kontinum_name = "@#kontinum" in
                    let command1 = [Sonata.MALLOC; Sonata.BIND kontinum_name; Sonata.PUSH (Sonata.Id kontinum_name); Sonata.STORE] in
                    let command2 = (trans command) in
                    let command3 = [(Sonata.PUSH(Sonata.Id kontinum_name)); Sonata.LOAD; (Sonata.PUSH (Sonata.Val (Sonata.Z 999))); Sonata.MALLOC; Sonata.CALL] in
                    let commands = List.append command1 (List.append command2 command3) in
                        Sonata.Fn (str, commands)) in
		    Sonata.PUSH sonata_obj in

		  let rec trans_jtr_cmd command1 command2 tl =
		    [Sonata.JTR ((List.append (trans command1) (trans tl)), (List.append (trans command2) (trans tl)))] in

		  let rec trans_box_cmd int1 =
		    Sonata.BOX int1 in

		  let rec trans_unbox_cmd str =
		    Sonata.UNBOX str in

		  let rec trans_bind_cmd str =
		    Sonata.BIND str in

          let rec trans_call_cmd rest_sonata_command =
            let randomName=  counter() in
            let temp_loc_name = String.concat " " [randomName; "_loc_name"] in
            let temp_var_name = String.concat " " [randomName; "_var_name"] in
            let temp_proc_name1 = String.concat " " [randomName; "_proc_name1"] in
            let temp_return_name = String.concat " " [randomName; "rest_sonata_command"] in

            let command2_non_recursive_kontinum = [(Sonata.PUSH (Sonata.Fn (temp_return_name, rest_sonata_command)))] in (* if kontinum loc is -111, 0*)
            let command2_recursive_kontinum = [(Sonata.PUSH (Sonata.Fn (temp_return_name, (List.append rest_sonata_command [(Sonata.PUSH(Sonata.Id "@#kontinum")); Sonata.LOAD; (Sonata.PUSH (Sonata.Val (Sonata.Z 1111))); Sonata.MALLOC; Sonata.CALL]))))] in (* if kontinum loc is not -111, 0 *)

            let command1 = [Sonata.BIND temp_loc_name; Sonata.BIND temp_var_name; Sonata.BIND temp_proc_name1] in
            let command2 = [Sonata.PUSH (Sonata.Val (Sonata.L (-111, 0))); Sonata.PUSH (Sonata.Id "@#kontinum"); Sonata.EQ; Sonata.JTR (command2_non_recursive_kontinum, command2_recursive_kontinum)] in
            let command3 = [Sonata.PUSH(Sonata.Id temp_proc_name1); Sonata.PUSH (Sonata.Id temp_var_name); Sonata.PUSH (Sonata.Id temp_loc_name); Sonata.CALL] in
            let command4 = [] in
            List.append command1 (List.append command2 (List.append command3 command4)) in
(*
            let rest_command
            PUSH 나올 수 없는 임의의 바리어블ㅡ 앞으로 나올 trans(sm5커맨드)한 것을
            이놈을 메모리에 저장
            그 로케이션을 미리 정의한 메모리 영역에 기록.
            매모리 값 읽어서 환경에 컨티넘을 써줌.
*)

    let emptyContinum = ref(0) in
    fun command -> 
        match command with
        | [] -> []
	    | (Sm5.PUSH obj)::tl -> trans_push_cmd obj::trans(tl)
	    | Sm5.POP::tl -> Sonata.POP::trans(tl);
	    | Sm5.STORE::tl -> Sonata.STORE::trans(tl);
	    | Sm5.LOAD::tl -> Sonata.LOAD::trans(tl);
	    | (Sm5.JTR (command1, command2))::tl -> (trans_jtr_cmd command1 command2 tl);
	    | Sm5.MALLOC::tl -> Sonata.MALLOC::trans(tl);
	    | (Sm5.BOX int1)::tl -> (trans_box_cmd int1)::trans(tl);
	    | (Sm5.UNBOX str)::tl -> (trans_unbox_cmd str)::trans(tl);
	    | (Sm5.BIND str)::tl -> (trans_bind_cmd str)::trans(tl);
	    | (Sm5.UNBIND)::tl -> (Sonata.UNBIND)::trans(tl);
	    | (Sm5.GET)::tl -> (Sonata.GET)::trans(tl);
	    | (Sm5.PUT)::tl -> (Sonata.PUT)::trans(tl);
	    | (Sm5.CALL)::tl -> 
            if !emptyContinum = 0
                then
                    (print_endline "@@@@@@@@@@@@@@@@@@@@@@@@@@";
                    incr emptyContinum; List.append [Sonata.PUSH (Sonata.Val (Sonata.L (-111, 0))); Sonata.BIND "@#kontinum"] (trans_call_cmd (trans tl)))
                else
                    (trans_call_cmd (trans tl)) 
	    | (Sm5.ADD)::tl -> (Sonata.ADD)::trans(tl);
	    | (Sm5.SUB)::tl -> (Sonata.SUB)::trans(tl);
	    | (Sm5.MUL)::tl -> (Sonata.MUL)::trans(tl);
	    | (Sm5.DIV)::tl -> (Sonata.DIV)::trans(tl);
	    | (Sm5.EQ)::tl -> (Sonata.EQ)::trans(tl);
	    | (Sm5.LESS)::tl -> (Sonata.LESS)::trans(tl);
	    | (Sm5.NOT)::tl -> (Sonata.NOT)::trans(tl);
end
