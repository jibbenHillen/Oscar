module A = Ast
module S = Sast
module L = Llvm
open Codegen

let as_int te builder =
  match (snd texpr) with
    Double_t -> (
            let v = t_expr te builder in
            L.build_fptosi v i32_t "tmp_i" builder
    _ -> raise Failure ("can only cast float to int")

let as_double te =
  match (snd texpr) with
    Int_t -> (
            let v = t_expr te builder in
            L.build_sitofp v f_t "tmp_f" builder
    _ -> raise Failure ("can only cast int to float")


let as_string te =
  match (snd texpr) with
    Bool_t  ->
    _       -> (let v = t_expr te builder in
                let str = L.string_of_llvalue v in
                L.build_global_stringptr str "tmp_s" builder)
