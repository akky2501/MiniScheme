open Syntax
open Env
open Primitive
open Evaluator



let init env =
  Env.add env "define" (Special do_define);
  Env.add env "define-macro" (Special do_define_macro);
  Env.add env "lambda" (Special do_lambda);
  Env.add env "quote"  (Special do_quote);
  Env.add env "set!"   (Special do_set);
  Env.add env "let"    (Special do_let);
  Env.add env "let*"   (Special do_letstar);
  Env.add env "letrec" (Special do_letrec);
  Env.add env "if"     (Special do_if);
  Env.add env "cond"   (Special do_cond);
  Env.add env "and"    (Special do_and);
  Env.add env "or"     (Special do_or);
  Env.add env "begin"  (Special do_begin);
  Env.add env "do"     (Special do_do);


  Env.add env "number?" (Primitive is_number);
  Env.add env "+"  (Primitive add);
  Env.add env "-"  (Primitive sub);
  Env.add env "*"  (Primitive mul);
  Env.add env "/"  (Primitive div);
  Env.add env "="  (Primitive num_eq);
  Env.add env "<"  (Primitive num_lt);
  Env.add env "<=" (Primitive num_le);
  Env.add env ">"  (Primitive num_gt);
  Env.add env ">=" (Primitive num_ge);

  Env.add env "null?"    (Primitive is_null);
  Env.add env "pair?"    (Primitive is_pair);
  Env.add env "list?"    (Primitive is_list);
  Env.add env "symbol?"  (Primitive is_symbol);
  Env.add env "car"      (Primitive car);
  Env.add env "cdr"      (Primitive cdr);
  Env.add env "cons"     (Primitive cons);
  Env.add env "list"     (Primitive list);
  Env.add env "length"   (Primitive length);
  Env.add env "memq"     (Primitive memq);
  Env.add env "member"     (Primitive member);
  Env.add env "last"     (Primitive last);
  Env.add env "append"   (Primitive append);
  Env.add env "set-car!" (Primitive set_car);
  Env.add env "set-cdr!" (Primitive set_cdr);

  Env.add env "boolean?" (Primitive is_boolean);
  Env.add env "not"      (Primitive bool_not);

  Env.add env "string?"        (Primitive is_string);
  Env.add env "string-append"  (Primitive string_append);
  Env.add env "symbol->string" (Primitive symbol_to_string);
  Env.add env "string->symbol" (Primitive string_to_symbol);
  Env.add env "string->number" (Primitive string_to_number);
  Env.add env "number->string" (Primitive number_to_string);

  Env.add env "procedure?" (Primitive is_procedure);
  Env.add env "call/cc"    (Primitive callcc);

  Env.add env "eq?"    (Primitive eq);
  Env.add env "neq?"   (Primitive neq);
  Env.add env "equal?" (Primitive equal);

  Env.add env "print"   (Primitive print);
  Env.add env "display" (Primitive display);
  ()



exception StopEval
exception QuitREPL

let stop signal =
  print_newline ();
  print_endline "catch signal_int.";
  raise StopEval

let setting () =
  let open Batteries.Sys in
  set_signal sigint (Signal_handle stop)

let gen_stream () =
  (* let sbuf = ref "" in 要求文字数がラインバッファサイズを超えていたとき用の一時変数 *)
  fun buf n ->
    let rec input_loop () =
      Printf.printf "> ";
      read_line () ^ "\n"
    in
    try
      let s = input_loop () in
      let m = min (String.length s) n in
      let () = Bytes.blit_string s 0 buf 0 m in
      m
    with End_of_file -> 0


let repl () =
  let env = { up = None; tbl = Hashtbl.create 10 } in
  let success v = print_endline (show_sexpr v); Some v in
  let error msg = print_endline msg; None in
  let lexbuf = gen_stream () |> Lexing.from_function in
  let rec rep () =
    match Parser.main Lexer.token lexbuf with
    | Some (Cons {car = Sym "load"; cdr = Cons {car = Str file; cdr = Nil}}) ->
      let channel = open_in file in 
      let lexbuf = Lexing.from_channel channel in
      let fin = ref false in
      while not !fin do
        (match Parser.main Lexer.token lexbuf with
         | Some exp -> let _ = Evaluator.eval exp env error success in ()
         | None -> fin := true)
      done;
      close_in channel
    | Some exp -> let _ = Evaluator.eval exp env error success in ()
    | None -> raise QuitREPL
  in
  setting ();
  init env;
  try
    while true do
      try rep () with
      | StopEval ->
        Lexing.flush_input lexbuf;
        print_endline "stop evaluate.";
        flush stdout;
      | Parser.Error ->
        Lexing.flush_input lexbuf;
        print_endline "parse error.";
        print_endline "input buffer flushed."
    done
  with QuitREPL -> print_endline "quit repl."