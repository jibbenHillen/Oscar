module L = Llvm
module A = Ast
module S = Sast

open Conf
open Llvm_linker
open Llvm_bitreader

module StringMap = Map.Make(String)
open Hashtbl
(* module Hashtbl = Hashtbl.Make(String) *)

let local_vars = Hashtbl.create 50

let translate (messages, actors, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Oscar"
  and i64_t  = L.i64_type context
  and i32_t  = L.i32_type context
  and i8_t   = L.i8_type context
  and i1_t   = L.i1_type context
  and f_t    = L.double_type context
  and void_t = L.void_type context in

  let ltype_of_typ (typ : A.types) = match typ with
      A.Int_t     -> i32_t
    | A.Char_t    -> i8_t
    | A.Bool_t    -> i1_t
    | A.Unit_t    -> void_t
    | A.Double_t  -> f_t
    | A.String_t  -> L.pointer_type i8_t
    | _           -> raise (Failure ("TODO implement types") )
  in


  let func_lookup (fname) =
    match (L.lookup_function (match fname with
                                "Println" ->  "printf"
                              | _ -> fname ) the_module) with
        None -> raise (Failure ("Func " ^ fname ^ " not defined in codegen."))
      | Some(func) -> func
  in


  let create_imported_funcs () =
    (* Functions from C *) 
    let print_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let _ = L.declare_function "printf" print_t the_module in

    (* Functions from Native *)
    let makeIntList_t = L.function_type (L.pointer_type i64_t) [| |] in
    let _ = L.declare_function "MakeIntList" makeIntList_t the_module in
   (*  let foreach_t = L.function_type void_t [| (L.pointer_type i64_t); (L.pointer_type i64_t); |] in
    let _ = L.declare_function "ForEach" foreach_t the_module in
    (* second ForEach for Map *)
    let filter_t = L.function_type (L.pointer_type i64_t) [| (L.pointer_type i64_t); (L.pointer_type i64_t); |] in
    let _ = L.declare_function "Filter" filter_t the_module in
    (* second Filter for Map *)
    let reverse_t = L.function_type (L.pointer_type i64_t) [| (L.pointer_type i64_t) |] in
    let _ = L.declare_function "Reverse" reverse_t the_module in *)
    ();
  in

  let link_file file_name = 
    let llctx = Llvm.global_context () in
    let llmem = Llvm.MemoryBuffer.of_file "src/native/immut/immut.bc" in
    let llm = Llvm_bitreader.parse_bitcode llctx llmem in
    ignore(Llvm_linker.link_modules the_module llm);
  in


(*   let generate_map_of_type type llexpr =

    let sizeofexpr = ll.sizeof type in
    (* Build up expressions here *)
    

 *)

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m func =
      let name = func.S.sf_name
      and formal_types =
         Array.of_list (List.map (fun (_,t) -> ltype_of_typ t) func.S.sf_formals)
      in
      let ftype = L.function_type (ltype_of_typ func.S.sf_return_t) formal_types in
      StringMap.add name (L.define_function name ftype the_module, func) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body func =

    (* clear hashtable *)
    ignore(Hashtbl.clear local_vars);

    let (the_function, _) = StringMap.find func.S.sf_name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Add a local varible to the Hashtbl local_vars:
        Allocate on the stack, initialize its value,
        and add to Hashtbl local_vars *)
    let add_local (n, t) p =
      let local = L.build_alloca (ltype_of_typ t) n builder in
        let () =  ignore(L.build_store p local builder) in
        Hashtbl.add local_vars n local;
        L.set_value_name n p;
    in
    (* add all the formals for the function to the Hashtbl local_vars *)
    let () = ignore(List.map2 add_local func.S.sf_formals
                      (Array.to_list (L.params the_function))) in


    (* Return the value for a variable or formal argument *)
    let lookup n =
      try Hashtbl.find local_vars n with
          | Not_found -> raise (Failure ("undefined local variable: " ^ n))
    in

    let ll_binop op typ =
      match typ with
        A.Int_t | A.Char_t | A.Bool_t ->
          (match op with
              A.Add         -> L.build_add
            | A.Sub         -> L.build_sub
            | A.Mult        -> L.build_mul
            | A.Div         -> L.build_sdiv
            | A.Mod         -> L.build_srem
            | A.Equal       -> L.build_icmp L.Icmp.Eq
            | A.Neq         -> L.build_icmp L.Icmp.Ne
            | A.Less        -> L.build_icmp L.Icmp.Slt
            | A.Leq         -> L.build_icmp L.Icmp.Sle
            | A.Greater     -> L.build_icmp L.Icmp.Sgt
            | A.Geq         -> L.build_icmp L.Icmp.Sge
            | A.And         -> L.build_and
            | A.Or          -> L.build_or
            | A.Bit_And     -> L.build_and
            | A.Bit_Or      -> L.build_or
            | A.Bit_Xor     -> L.build_xor
            | A.Bit_RShift  -> L.build_lshr
            | A.Bit_LShift  -> L.build_shl
            | _             -> raise (Failure ("bad int/char/bool binop") )
          )
      (* TODO: make void ops work *)
      | A.Unit_t                      ->
          (match op with
            | A.Equal       -> L.build_icmp L.Icmp.Eq
            | A.Neq         -> L.build_icmp L.Icmp.Ne
            | _             -> raise (Failure ("bad unit binop") )
          )
      | A.Double_t                    ->
          (match op with
              A.Add         -> L.build_fadd
            | A.Sub         -> L.build_fsub
            | A.Mult        -> L.build_fmul
            | A.Div         -> L.build_fdiv
            | A.Equal       -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq         -> L.build_fcmp L.Fcmp.One
            | A.Less        -> L.build_fcmp L.Fcmp.Olt
            | A.Leq         -> L.build_fcmp L.Fcmp.Ole
            | A.Greater     -> L.build_fcmp L.Fcmp.Ogt
            | A.Geq         -> L.build_fcmp L.Fcmp.Oge
            | _             -> raise (Failure ("bad double binop") )
          )
      (* TODO: make string ops work *)
      | A.String_t                    ->
          (match op with
              A.Add         -> L.build_add
            | A.Equal       -> L.build_icmp L.Icmp.Eq
            | A.Neq         -> L.build_icmp L.Icmp.Ne
            | _             -> raise (Failure ("bad string binop") )
          )

      | A.List_t(t)         -> raise (Failure ("binop TODO") )
      | A.Set_t(s)          -> raise (Failure ("binop TODO") )
      | A.Map_t(kt, vt)     -> raise (Failure ("binop TODO") )
      | _                   -> raise (Failure ("bad binop" ) )

    in



    (* Invoke "f builder" if the current block doesn't already
    have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
          Some _ -> false
        | None -> (let () = ignore (f builder) in true) in

    (* Construct code for an expression; return its value *)
    (* takes a t_expr, which is (sexpr, types) *)
    let rec t_expr builder (te : S.t_expr) =
      match te with
        (S.SInt_Lit(i), typ)          -> L.const_int i32_t i
      | (S.SDouble_Lit(d), typ)       -> L.const_float f_t d
      | (S.SChar_Lit(c), typ)         -> L.const_int i8_t (Char.code c)
      | (S.SString_Lit(s), typ)       -> L.build_global_stringptr s "tmp" builder
      | (S.SBool_Lit(b), typ)         -> L.const_int i1_t (if b then 1 else 0)
      | (S.SUnit_Lit(u), typ)         -> raise (Failure ("no units") )
      | (S.SNoexpr, typ)              -> L.const_int i32_t 0
      | (S.SId(s), typ)               -> L.build_load (lookup s) s builder
      | (S.SAccess(e1, e2), typ)      -> raise (Failure ("TODO: access") )
      | (S.SLambda(func), typ)        -> raise (Failure ("TODO: lambdas") )
      | (S.SList_Lit(t, exprs), typ)  -> raise (Failure ("TODO: lists") )
      | (S.SSet_Lit(t, exprs), typ)  -> raise (Failure ("TODO: sets") )
      | (S.SMap_Lit(kt, vt, kvs), typ)-> raise (Failure ("TODO: maps") )
      | (S.SActor_Lit(n, exprs), typ) -> raise (Failure ("TODO: actors") )
      | (S.SPool_Lit(n, exprs, c), typ) -> raise (Failure ("TODO: pools") )
      | (S.SMessage_Lit(n, exprs), typ)-> raise (Failure ("TODO: messages") )
      | (S.SBinop(e1, op, e2), typ)   ->
        let e1' = t_expr builder e1
        and e2' = t_expr builder e2 in
          (ll_binop op (snd e1)) e1' e2' "tmp" builder

      | (S.SUop(op, e), typ) ->
        let e' = t_expr builder e in
          (match op with
              A.Neg -> (match typ with
                  A.Int_t -> L.build_neg
                | A.Double_t -> L.build_fneg
                | _ -> raise (Failure ("bad unop") ))
            | A.Not -> L.build_not
          ) e' "tmp" builder
      | (S.SFuncCall("Println", t_el), typ) -> build_print_call t_el builder
      | (S.SFuncCall(f, act), typ) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (t_expr builder) (List.rev act)) in
          let result = (
            match fdecl.S.sf_return_t with
                A.Unit_t -> ""
              | _ -> f ^ "_result"
            ) in
          L.build_call fdef (Array.of_list actuals) result builder

    (* Takes a list of expressions and builds the correct print call *)
    (* t_el is list of t_exprs *)
    and build_print_call t_el builder =

      let print_func = func_lookup "printf" in

      (* make a function that handles anything with bool_t as an if *)
      let to_print_texpr texpr = match (snd texpr) with
          A.Bool_t  ->
            (S.SFuncCall("AsString", [texpr]), A.String_t)
            (*let t = { S.sv_name = "_tmpBool";  S.sv_type = A.String_t;

              S.sv_init = (S.SString_Lit("true"), A.String_t) }
            and f = { S.sv_name = "_tmpBool";  S.sv_type = A.String_t;
              S.sv_init = (S.SString_Lit("false"), A.String_t) } in
            let () = ignore (stmt builder (S.SIf(texpr,
                S.SBlock([S.SVdecl(t)]), S.SBlock([S.SVdecl(f)])))) in
            (S.SId("_tmpBool"), A.String_t)*)

        | _     -> texpr
      in

      (* type to string used to print *)
      let map_type_to_string = function
          A.Int_t           -> "%d"
        | A.Bool_t          -> "%d" (* todo bools print as ints *)
        | A.Double_t        -> "%f"
        | A.Char_t          -> "%c"
        | A.String_t        -> "%s"
        | _                 -> raise ( Failure("TODO: printing types"))

      in

      (* get the things to print *)
      let pexprs_l = List.map to_print_texpr t_el in
      let params = List.map (t_expr builder) pexprs_l in
      (* get their types *)
      let param_types = List.map snd pexprs_l in

      let const_str = List.fold_left
                        (fun s t -> s ^ map_type_to_string t) "" param_types
      in
      (* default add newline *)
      let fmt_str = L.build_global_stringptr
                        (const_str ^ "\n") "tmp" builder in
      L.build_call print_func
                        (Array.of_list (fmt_str :: params)) "printf" builder

      and stmt_if builder predicate then_stmt else_stmt =
        let bool_val = t_expr builder predicate in

        let entry_bb = L.insertion_block builder in
        let merge_bb = L.append_block context "finishif" the_function in

        (* build then block *)
        let then_bb = L.append_block context "then" the_function in
        let () = L.position_at_end then_bb builder in
        let () = stmt builder then_stmt in
        let t_mrg = add_terminal builder (L.build_br merge_bb) in

        (* build else block *)
        let else_bb = L.append_block context "else" the_function in
        let () = L.position_at_end else_bb builder in
        let () = stmt builder else_stmt in
        let e_mrg = add_terminal builder (L.build_br merge_bb) in


        (* wherever our entry block was, go back there and cond branch *)
        let () = L.position_at_end entry_bb builder in
        let () = ignore(L.build_cond_br bool_val then_bb else_bb builder) in

        (* if we need to resolve to a merge, keep it and move builder,
         * otherwise, delete the merge block and go back to caller *)

        if (t_mrg || e_mrg) then
          L.position_at_end merge_bb builder
        else
          let () = (L.delete_block merge_bb) in
          L.position_at_end entry_bb builder

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
     and stmt builder = function
        S.SExpr(se, typ) -> ignore (t_expr builder (se, typ))
      | S.SBlock(sl) -> ignore(List.map (stmt builder) sl)
      | S.SReturn(se, typ) -> ignore(
          match func.S.sf_return_t with
              A.Unit_t -> L.build_ret_void builder
            | _ -> L.build_ret (t_expr builder (se, typ)) builder
          )
      | S.SVdecl(sval_decl) ->
          let init_val = t_expr builder sval_decl.sv_init in
            let tup = (sval_decl.sv_name, sval_decl.sv_type) in
              add_local tup init_val
      | S.SMutdecl(smvar_decl) -> raise ( Failure ("TODO: mutdecl") )
      | S.SFdecl(func) -> raise ( Failure ("TODO: funcs") )
      | S.SIf (predicate, then_stmt, else_stmt) ->
          stmt_if builder predicate then_stmt else_stmt
      | S.SActor_send(e1, e2) -> raise ( Failure ("TODO: act_send") )
      | S.SPool_send(e1, e2) -> raise ( Failure ("TODO: pool_broadcast") )
    in

    (* Build the code for each statement in the function *)
    let () = stmt builder func.S.sf_body in

    (* Add a return if the last block falls off the end *)
    match func.S.sf_return_t with
      A.Unit_t -> ignore(add_terminal builder L.build_ret_void)
      | _ -> ()
  in

  let _ = create_imported_funcs() in
  let _ = link_file Conf.bindings_path in
  let _ = List.iter build_function_body functions in
  the_module
