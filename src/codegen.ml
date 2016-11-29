module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (messages, actors, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Oscar"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let ltype_of_typ = function
    | A.Unit_t -> void_t
  in

  (* Declare print(), which the print built-in function will call *)
  let print_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let print_func = L.declare_function "printf" print_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m func =
      let name = func.A.f_name
      and formal_types =
         Array.of_list (List.map (fun (_,t) -> ltype_of_typ t) func.A.f_formals)
      in 
      let ftype = L.function_type (ltype_of_typ func.A.f_return_t) formal_types in
      StringMap.add name (L.define_function name ftype the_module, func) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body func =
    let (the_function, _) = StringMap.find func.A.f_name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in


    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.Int_Lit(i) -> L.const_int i32_t i
     (* | A.Bool_Lit b -> L.const_int i1_t (if b then 1 else 0) *)
      | A.Noexpr -> L.const_int i32_t 0
      (* | A.Id s -> L.build_load (lookup s) s builder *)
      | A.Binop(e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
          (match op with 
              A.Add     -> L.build_add
            | A.Sub     -> L.build_sub
            | A.Mult    -> L.build_mul
            | A.Div     -> L.build_sdiv
            | A.Mod     -> L.build_srem
            | A.Equal   -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.Less    -> L.build_icmp L.Icmp.Slt
            | A.Leq     -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.Geq     -> L.build_icmp L.Icmp.Sge
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            (* add bitwise ops *)
          ) e1' e2' "tmp" builder
      | A.String_Lit(s) -> L.build_global_stringptr s "tmp" builder
      | A.Call ("println", el) -> build_print_call el builder
    
    (* Takes a list of expressions and builds the correct print call *)
    and build_print_call el builder = 

      let rec map_param_to_type = function
          A.Int_Lit(_)      -> A.Int_t
        | A.Double_Lit(_)   -> A.Double_t
        | A.String_Lit(_)   -> A.String_t
        | A.Binop(e1, _, _)  -> map_param_to_type e1 
                                  (* temp fix, grabs type of left arg *)
      in 

      let map_type_to_string = function 
          A.Int_t           -> "%d"
       (*  | A.Bool_Lit          -> "%s" *)
        | A.Double_t        -> "%f"
       (* | A.Char_Lit          -> "%c" *)
        | A.String_t        -> "%s"

      in

      let params = List.map (expr builder) el in
      let param_types = List.map map_param_to_type el in

      
      let const_str = List.fold_left 
                        (fun s t -> s ^ map_type_to_string t) "" param_types
      in
      (* default add newline *)
      let fmt_str = L.build_global_stringptr 
                        (const_str ^ "\n") "tmp" builder in
      L.build_call print_func 
                        (Array.of_list (fmt_str :: params)) "printf" builder
      in

    


    (* Invoke "f builder" if the current block doesn't already
    have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (f builder) in
  
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
        A.Expr e -> ignore (expr builder e); builder
      | A.Block sl -> List.fold_left stmt builder sl
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block func.A.f_body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match func.A.f_return_t with
      | A.Unit_t -> L.build_ret_void)
  in

  List.iter build_function_body functions;
  the_module
