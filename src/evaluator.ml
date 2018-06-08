open Syntax
open Env


let is_true = function
  | Bool(false) -> false
  | _ -> true

let rec list_of_cons = function
  | Cons {car = h; cdr = t} ->
    let (l, v) = list_of_cons t in
    (h::l, v)
  | Nil -> ([], None)
  | x -> ([], Some x)

let rec cons_of_list = function
  | [] -> Nil
  | h :: t -> Cons {car = h; cdr = cons_of_list t}


exception Escape
let list_of_bindngs bind =
  try
    let ret = match list_of_cons bind with
      | (assoc, None) -> List.map (function
          | Cons {car = Sym var; cdr = Cons {car = v; cdr = Nil}} -> (var, v)
          | _ -> raise Escape) assoc
      | _ -> raise Escape
    in
    Some ret
  with Escape -> None


let rec list_of_clause lst =
  let rec traverse = function
    | Cons {car = Cons{car = Sym "else"; cdr = body}; cdr = Nil} -> ([], Some body)
    | Cons {car = Cons{car = Sym "else"; cdr = body}; cdr = _} -> raise Escape (* else clause must be last term.*)
    | Cons {car = Cons{car = cond; cdr = body}; cdr = Nil} -> ([(cond, body)], None)
    | Cons {car = Cons{car = cond; cdr = body}; cdr = tail} ->
      let (pairs, eclause) = traverse tail in
      ((cond, body) :: pairs, eclause)
    | _ -> raise Escape
  in
  try Some (traverse lst) with Escape -> None


let list_of_do_bindngs bind =
  try
    let ret = match list_of_cons bind with
      | (assoc, None) -> List.map (function
          | Cons {car = Sym var; cdr = Cons {car = ini; cdr = Cons {car = update;cdr = Nil}}} -> (var, ini, update)
          | _ -> raise Escape) assoc
      | _ -> raise Escape
    in
    Some ret
  with Escape -> None



let rec eval e env err cont =
  (*print_endline (show_sexpr e); print_env env 0;*)
  match e with
  | Undefined    as v -> cont v
  | Nil          as v -> cont v
  | Num        _ as v -> cont v
  | Bool       _ as v -> cont v
  | Str        _ as v -> cont v
  | Special    _ as v -> cont v
  | Primitive  _ as v -> cont v
  | Cont       _ as v -> cont v
  | Procedure  _ as v -> cont v
  | Macro      _ as v -> cont v
  | Sym s -> (match find env s with
      | Some t -> cont t
      | None   -> err ("cannot find symbol: " ^ s))
  | Cons {car = h; cdr = t} -> eval h env err (fun h ->
      match h with
      | Special form -> form t env err cont
      | Primitive f -> eval_seq t env err (fun args -> f args err cont)
      | Procedure p -> eval_seq t env err (fun args -> apply p args err cont)
      | Macro m -> (match list_of_cons t with
          | (args, None) -> apply m args err (fun v -> eval v env err cont)
          | _ -> err "macro: illformed.")
      | Cont k -> eval_seq t env err (function
          | a :: [] -> k a
          | _ -> err "continuation required 1 argument.")
      | x -> err ("head is not a function. :" ^ show_sexpr x))

and eval_seq seq env err cont = match seq with
  | Cons {car = e; cdr = t} -> eval e env err (fun v ->
      eval_seq t env err (fun s -> cont (v :: s)))
  | Nil -> cont []
  | _ -> err "eval_seq: sequence must be list."

and apply {formals = f; va = v; body = b; env = e} args err cont =
  let newenv = { up = Some e; tbl = Hashtbl.create 0 } in
  let rec register formals args = match (formals, args) with
    | ([], []) -> Ok ()
    | ([], args) -> (match v with
        | Some va -> add newenv va (cons_of_list args); Ok ()
        | None -> Error "this proceure has no variadic arguments.")
    | (formals, []) -> Error "fewer arguments."
    | (f::ftail, a::atail) -> add newenv f a; register ftail atail
  in
  match register f args with
  | Ok () -> eval b newenv err cont
  | Error s -> err ("apply: " ^ s)


and do_define args env err cont = match args with
  | Cons{car = Sym s; cdr = Cons{car = e; cdr = Nil}} -> eval e env err (fun v -> add env s v; cont Undefined)
  | Cons{car = Cons{car = Sym s; cdr = args}; cdr = body} ->
    let t = Cons{car = Sym "lambda"; cdr = Cons{car = args; cdr = body}} in
    eval t env err (fun v -> add env s v; cont Undefined)
  | _ -> err "define: illformed."

and do_define_macro args env err cont = match args with
  | Cons{car = Cons{car = Sym s; cdr = args}; cdr = body} ->
    let t = Cons{car = Sym "lambda"; cdr = Cons{car = args; cdr = body}} in
    eval t env err (function 
        | Procedure p -> add env s (Macro p); cont Undefined
        | _ -> assert false)
  | _ -> err "define-macro: illformed."

and do_lambda args env err cont = match args with
  | Cons{car = args; cdr = body} ->
    let (args, va) = list_of_cons args in
    if List.for_all (function Sym _ -> true | _ -> false) args
    && (match va with Some (Sym _) | None -> true | _ -> false)
    then cont (Procedure {
        formals = List.map (function Sym s -> s | _ -> assert false) args; 
        va = BatOption.map (function Sym s -> s | _ -> assert false) va;
        body = Cons{car = Sym "begin"; cdr = body};
        env = env})
    else err "lambda: parameters must be symbol."
  | _ -> err "lambda: illformed."

and do_quote args env err cont = match args with
  | Cons {car = sym; cdr = Nil} -> cont sym
  | _ -> err "quote: illformed."

and do_set args env err cont = match args with
  | Cons{car = Sym s; cdr = Cons{car = t; cdr = Nil}} -> 
    eval t env err (fun v -> match overwrite env s v with
        | Ok () -> cont Undefined
        | Error () -> err ("set!: unbounded variable["^s^"]."))
  | _ -> err "set!: illformed."

and do_let args env err cont = match args with
  | Cons {car = Sym label; cdr = Cons {car = bindings; cdr = body}} ->
    (match list_of_bindngs bindings with
     | Some pairs ->
       let formals = List.map (fun (x, _) -> Sym x) pairs in
       let args = List.map (fun (_, x) -> x) pairs in
       let parameters = Cons {car = Sym label; cdr = cons_of_list formals} in
       let def = Cons {car = Sym "define"; cdr = Cons {car = parameters; cdr = body}} in
       let call = Cons {car = Sym label; cdr = cons_of_list args} in
       let lam = Cons {car = Sym "lambda"; 
                       cdr = Cons {car = Nil; 
                                   cdr = Cons {car = def;
                                               cdr = Cons {car = call;
                                                           cdr = Nil}}}} in
       let e = Cons {car = lam; cdr = Nil} in
       eval e env err cont
     | None -> err "labeled let [bindings]: illformed.")
  | Cons {car = bindings; cdr = body} ->
    (match list_of_bindngs bindings with
     | Some pairs ->
       let formals = List.map (fun (x, _) -> Sym x) pairs in
       let args = List.map (fun (_, x) -> x) pairs in
       let lam = Cons {car = Sym "lambda"; 
                       cdr = Cons {car = cons_of_list formals; 
                                   cdr = body}} in
       let e = Cons {car = lam; cdr = cons_of_list args} in
       eval e env err cont
     | None -> err "let [bindings]: illformed.")
  | _ -> err "let: illformed."


and do_letstar args env err cont = match args with
  | Cons {car = bindings; cdr = body} ->
    (match list_of_bindngs bindings with
     | Some pairs ->
       let pairs = List.map (fun (n, v) -> Cons {car = Sym n; cdr = Cons {car = v; cdr = Nil}}) pairs in
       let e = List.fold_right (fun bind e -> 
           Cons {car = Sym "let"; cdr = Cons {car = Cons {car = bind; cdr = Nil}; cdr = Cons { car = e; cdr = Nil}}}
         ) pairs (Cons {car = Sym "begin"; cdr = body})
       in
       eval e env err cont
     | None -> err "let* [bindings]: illformed.")
  | _ -> err "let*: illformed."

and do_letrec args env err cont = match args with
  | Cons {car = bindings; cdr = body} ->
    (match list_of_bindngs bindings with
     | Some pairs ->
       let defs = List.map (fun (n, _) -> Cons {car = Sym "define"; cdr = Cons {car = Sym n; cdr = Cons {car = Undefined; cdr = Nil}}}) pairs in
       let sets = List.map (fun (n, v) -> Cons {car = Sym "set!";   cdr = Cons {car = Sym n; cdr = Cons {car = v        ; cdr = Nil}}}) pairs in
       let e = List.fold_right (fun c acc -> Cons {car = c; cdr = acc}) sets body in
       let e = List.fold_right (fun c acc -> Cons {car = c; cdr = acc}) defs e in
       let e = Cons {car = Sym "begin"; cdr = e} in
       eval e env err cont
     | None -> err "letrec [bindings]: illformed.")
  | _ -> err "letrec: illformed."

and do_if args env err cont = match args with
  | Cons {car = cond; cdr = Cons{car = t; cdr = e}} ->
    eval cond env err (fun v ->
        if is_true v then eval t env err cont
        else match e with
          | Cons {car = e; cdr = Nil} -> eval e env err cont
          | Nil -> cont Undefined
          | _ -> err "if [else]: illformed.")
  | _ -> err "if: illformed."

and do_cond args env err cont = match list_of_clause args with
  | Some (cls, ecls) ->
    let ini = Cons {car = Sym "begin"; cdr = BatOption.default (Cons {car = Undefined; cdr = Nil}) ecls} in
    let e = List.fold_right (fun (cond, body)  acc -> 
        let body = Cons {car = Sym "begin"; cdr = body} in
        Cons {car = Sym "if"; cdr = Cons {car = cond; cdr = Cons {car = body; cdr = Cons {car = acc; cdr = Nil}}}}) cls ini in
    eval e env err cont
  | None -> err "cond: illformed."

and do_and args env err cont = match args with
  | Nil -> cont (Bool true)
  | Cons{car = h; cdr = Nil} -> eval h env err cont
  | Cons{car = h; cdr = t}   -> eval h env err (fun x ->
      if is_true x then do_and t env err cont
      else cont x)
  | _ -> err "and: illformed."

and do_or args env err cont = match args with
  | Nil -> cont (Bool false)
  | Cons{car = h; cdr = Nil} -> eval h env err cont
  | Cons{car = h; cdr = t}   -> eval h env err (fun x ->
      if is_true x then cont x
      else do_or t env err cont)
  | _ -> err "or: illformed."

(* // 注意深く書かないとthunk(?)が残ってしまう
   and do_begin args env err cont = match args with
   | Nil -> cont Undefined
   | Cons{car = l; cdr = Nil} -> eval l env err (fun v -> cont v)
   | Cons{car = h; cdr = t} -> eval h env err (fun _ -> do_begin t env err (fun v -> cont v))
   | _ -> err "begin: illformed."
*)

and do_begin args env err cont = match args with
  | Nil -> cont Undefined
  | Cons{car = l; cdr = Nil} -> eval l env err cont
  | Cons{car = h; cdr = t} -> eval h env err (fun _ -> do_begin t env err cont)
  | _ -> err "begin: illformed."

and do_do args env err cont = match args with
  | Cons {car = bindings; cdr = Cons {car = Cons {car = pred; cdr = Cons {car = value; cdr = Nil}}; cdr = body}} ->
    (match list_of_do_bindngs bindings with
     | Some triples -> 
       let label = "-*-*-*-do-loop-label-*-*-*-" in
       let formals = List.map (fun (x, _, _) -> Sym x) triples in
       let inits   = List.map (fun (_, x, _) -> x) triples in
       let updates = List.map (fun (_, _, x) -> x) triples in
       let body = Cons {car = Sym "begin"; cdr = body} in
       let call = Cons {car = Sym label; cdr = cons_of_list updates} in
       let iter = Cons {car = Sym "begin"; cdr = Cons {car = body; cdr = Cons {car = call; cdr = Nil}}} in
       let cexp = Cons {car = Sym "if"; cdr = Cons {car = pred; cdr = Cons {car = value; cdr = Cons {car = iter; cdr = Nil}}}} in
       let func = Cons {car = Sym "lambda"; cdr = Cons {car = cons_of_list formals; cdr = Cons {car = cexp; cdr = Nil}}} in
       let bind = Cons {car = Sym label; cdr = Cons {car = func; cdr = Nil}} in
       let start = Cons {car = Sym label; cdr = cons_of_list inits} in
       let e = Cons {car = Sym "letrec"; cdr = Cons {car = Cons {car = bind; cdr = Nil}; cdr = Cons{car = start; cdr = Nil}}} in
       eval e env err cont
     | None -> err "do [bingings]: illformed."
    )
  | _ -> err "do: illformed."


