(* Top-level of the Oscar compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)
open Core.Std

open Sast
open Lexing
open Printf
open Transpile

type action = Compile | Ast | Sast

let make_lexbuf file =
  let lexbuf = Lexing.from_channel file in
  let curr_p = {lexbuf.lex_curr_p with pos_lnum=1} in
  {lexbuf with lex_curr_p = curr_p;}

let parse_error lexbuf =
  let s = lexeme_start_p lexbuf in
  let f = lexeme_end_p lexbuf in
    (fprintf stderr "Syntax error at line %d, char %d - %d: \"%s\" \n"
    s.pos_lnum (s.pos_cnum - s.pos_bol)
    (f.pos_cnum - f.pos_bol) (Lexing.lexeme lexbuf))

let _ =
  let arg_len = Array.length Sys.argv in
  let (action, oscar) =
    (if (arg_len = 1 || arg_len > 3) then
      let _ = print_endline("Usage: ./oscar [-l|-c|-s] *.oscar") in
      exit 1;
    else if arg_len > 1 then
      try
        (List.Assoc.find_exn [
                ("-c", Compile);  (* Generate, check LLVM IR *)
                ("-p", Ast);      (* Don't gen LLVM, just prettyprint ast *)
                ("-s", Sast);     (* Don't generate LLVM, just prettyprint *)
        ] Sys.argv.(1), open_in Sys.argv.(2))
      with Not_found ->
        raise (Failure ("Invalid flag " ^ Sys.argv.(1)))
    else
      (Compile, open_in Sys.argv.(1))
    ) in
  let lexbuf = make_lexbuf oscar
  and stdlex = make_lexbuf (open_in "include/stdlib.oscar") in
  let program =
    try
      Parser.program Scanner.token lexbuf
    with
      Parsing.Parse_error ->
        let () = ignore(parse_error lexbuf) in exit(1)
  and stdlib = Parser.program Scanner.token stdlex in
  match action with
      Ast  -> print_endline (Ast.str_program program)
    | _    ->
         let sprogram =
           try
             Analyzer.check_program program stdlib
           with
             Failure f ->
               Printf.eprintf "%s" ("Error: " ^ f);
               flush stderr;
               exit 1;
         in
         (* let ast = Parser.program Scanner.token lexbuf in
         Semant.check ast; *)
         match action with
            Ast   -> ()
          | Sast  -> print_endline (Sast.str_sprogram sprogram)
          | _ ->
              let program = Transpile.c_program sprogram in
              let c_op = "-Wall -pedantic -fsanitize=address -std=c++1y -O2" in
              let cxx_incls = "-I/usr/local/include/ -L/usr/local/lib/ " in
              let cxx = sprintf "clang++ %s %s " c_op cxx_incls in
              let ch = Unix.open_process_out cxx in
                Out_channel.output_string ch program;
