module A = Ast
module S = Sast
module L = Llvm
module C = Codegen

let as_int builder (te: S.t_expr) =
  match (snd te) with
      A.Double_t -> (let v = C.t_expr te builder in
                     L.build_fptosi v i32_t "tmp_i" builder)
    | _          ->  raise Failure ("can only cast float to int")

let as_double (te : S.t_expr) =
  match (snd te) with
      A.Int_t -> (let v = C.t_expr te builder in
                  L.build_sitofp v f_t "tmp_f" builder)
    | _       -> raise Failure ("can only cast int to float")


let as_string (te : S.t_expr) =
  match (snd te) with
      A.Bool_t  ->  let t = { S.sv_name = "_tmpBool";  S.sv_type = A.String_t;
                              S.sv_init = (S.SString_Lit("true"), A.String_t) }
                    and f = { S.sv_name = "_tmpBool";  S.sv_type = A.String_t;
                              S.sv_init = (S.SString_Lit("false"), A.String_t) }
                    in
                    let () = ignore (stmt builder (S.SIf(texpr,
                                      S.SBlock([S.SVdecl(t)]), S.SBlock([S.SVdecl(f)])))) 
                    in
                    (S.SId("_tmpBool"), A.String_t)
    | _         -> (let v = C.t_expr te builder in
                    let str = L.string_of_llvalue v in
                    L.build_global_stringptr str "tmp_s" builder)
