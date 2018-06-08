open Syntax
open Env
open Evaluator


(* number *)
(* number?, +, -, *, /, =, <, <=, >, >= *)

let is_number args err cont = match args with
  | Num _ :: [] -> cont (Bool true)
  | _ :: [] -> cont (Bool false)
  | _ -> err "number? required 1 argument."

let add args err cont =
  let r = List.fold_left (fun acc x -> match x with
      | Num v -> BatOption.map (fun a -> a + v) acc
      | _ -> None) (Some 0) args
  in
  match r with
  | Some r -> cont (Num r)
  | None -> err "+: error"

let sub args err cont = match args with
  | Num init :: tail ->
    let r = List.fold_left (fun acc x -> match x with
        | Num v -> BatOption.map (fun a -> a - v) acc
        | _ -> None) (Some init) tail
    in
    (match r with
     | Some r -> cont (Num r)
     | None -> err "-: error.")
  | _ -> err "-: error."

let mul args err cont =
  let r = List.fold_left (fun acc x -> match x with
      | Num v -> BatOption.map (fun a -> a * v) acc
      | _ -> None) (Some 1) args
  in
  match r with
  | Some r -> cont (Num r)
  | None -> err "*: error"

let div args err cont = match args with
  | Num init :: tail ->
    let r = List.fold_left (fun acc x -> match x with
        | Num v -> BatOption.map (fun a -> a / v) acc
        | _ -> None) (Some init) tail
    in
    (match r with
     | Some r -> cont (Num r)
     | None -> err "/: error.")
  | _ -> err "/: error."

let num_eq args err cont = match args with
  | Num lhs :: Num rhs :: tail ->
    let r = List.fold_left (fun acc x -> match x with
        | Num v -> BatOption.map (fun (value, sign) -> (value, sign && (value = v))) acc
        | _ -> None) (Some (lhs, lhs = rhs)) tail
    in
    (match r with
     | Some (_, r) -> cont (Bool r)
     | None -> err "=: error.")
  | _ -> err "=: error."

let num_lt args err cont = match args with
  | Num lhs :: Num rhs :: tail ->
    let r = List.fold_left (fun acc x -> match x with
        | Num v -> BatOption.map (fun (value, sign) -> (v, sign && (value < v))) acc
        | _ -> None) (Some (rhs, lhs < rhs)) tail
    in
    (match r with
     | Some (_, r) -> cont (Bool r)
     | None -> err "<: error.")
  | _ -> err "<: error."

let num_le args err cont = match args with
  | Num lhs :: Num rhs :: tail ->
    let r = List.fold_left (fun acc x -> match x with
        | Num v -> BatOption.map (fun (value, sign) -> (v, sign && (value <= v))) acc
        | _ -> None) (Some (rhs, lhs <= rhs)) tail
    in
    (match r with
     | Some (_, r) -> cont (Bool r)
     | None -> err "<=: error.")
  | _ -> err "<=: error."

let num_gt args err cont = match args with
  | Num lhs :: Num rhs :: tail ->
    let r = List.fold_left (fun acc x -> match x with
        | Num v -> BatOption.map (fun (value, sign) -> (v, sign && (value > v))) acc
        | _ -> None) (Some (rhs, lhs > rhs)) tail
    in
    (match r with
     | Some (_, r) -> cont (Bool r)
     | None -> err ">: error.")
  | _ -> err ">: error."

let num_ge args err cont = match args with
  | Num lhs :: Num rhs :: tail ->
    let r = List.fold_left (fun acc x -> match x with
        | Num v -> BatOption.map (fun (value, sign) -> (v, sign && (value >= v))) acc
        | _ -> None) (Some (rhs, lhs >= rhs)) tail
    in
    (match r with
     | Some (_, r) -> cont (Bool r)
     | None -> err ">=: error.")
  | _ -> err ">=: error."


(* list *)
(* 
null?, pair?, list?, symbol?,
car, cdr, cons, list, length, memq, last, append,
set-car!, set-cdr!
*)

let is_null args err cont = match args with
  | Nil :: [] -> cont (Bool true)
  | _ :: [] -> cont (Bool false)
  | _ -> err "null? required 1 argument."

let is_pair args err cont = match args with
  | Cons _ :: [] -> cont (Bool true)
  | _ :: [] -> cont (Bool false)
  | _ -> err "pair? required 1 argument."

let rec is_list_impl = function
  | Cons {car = _; cdr = cdr} -> is_list_impl cdr
  | Nil -> true
  | _ -> false

let is_list args err cont = match args with
  | a :: [] -> cont (Bool (is_list_impl a))
  | _ -> err "list? required 1 argument."

let is_symbol args err cont = match args with
  | Sym _ :: [] -> cont (Bool true)
  | _ :: [] -> cont (Bool false)
  | _ -> err "symbol? required 1 argument."

let car args err cont = match args with
  | Cons {car = car; cdr = _} :: [] -> cont car
  | _ :: [] -> err "car: argument is not pair."
  | _ -> err "car required 1 argument."

let cdr args err cont = match args with
  | Cons {car = _; cdr = cdr} :: [] -> cont cdr
  | _ :: [] -> err "cdr: argument is not pair."
  | _ -> err "cdr required 1 argument."

let cons args err cont = match args with
  | x :: y :: [] -> cont (Cons {car = x; cdr = y})
  | _ -> err "cons required 2 argument."

let list args err cont = cont (cons_of_list args)

let length args err cont = match args with
  | l :: [] ->
    let rec f acc = function
      | Cons {car = _; cdr = cdr} -> f (1+acc) cdr
      | Nil -> acc
      | _ -> -1
    in
    let n = f 0 l in
    if n >= 0 then cont (Num n) else err "length: not list."
  | _ -> err "length: required 2 argument."

let last args err cont = match args with
  | l :: [] ->
    let rec f = function
      | Cons {car = v; cdr = Nil} -> Some v
      | Cons {car = _; cdr = cdr} -> f cdr
      | _ -> None
    in
    (match f l with
     | Some v -> cont v
     | None -> err "last: not list.")
  | _ -> err "last: required 2 argument."

let append args err cont = assert false

let set_car args err cont = match args with
  | Cons (_ as c) :: value :: [] -> c.car <- value; cont Undefined
  | _ -> err "set-car! required 2 argument."

let set_cdr args err cont = match args with
  | Cons (_ as c) :: value :: [] -> c.cdr <- value; cont Undefined
  | _ -> err "set-cdr! required 2 argument."


(* boolean *)
(* boolean?, not *)

let is_boolean args err cont = match args with
  | Bool _ :: [] -> cont (Bool true)
  | _ :: [] -> cont (Bool false)
  | _ -> err "boolean? required 1 argument."

let bool_not args err cont = match args with
  | Bool b :: [] -> cont (Bool (not b))
  | _ -> err "not: required 1 argument."


(* string *)
(*
string?, string-append,
symbol->string, string->symbol, string->number, number->string
*)

let is_string args err cont = match args with
  | Str _ :: [] -> cont (Bool true)
  | _ :: [] -> cont (Bool false)
  | _ -> err "string? required 1 argument."

let string_append args err cont =
  let r = List.fold_left (fun acc x -> match x with
      | Str v -> BatOption.map (fun a -> a ^ v) acc
      | _ -> None) (Some "") args
  in
  match r with
  | Some r -> cont (Str r)
  | None -> err "string-append: error"

let symbol_to_string args err cont = match args with
  | Str s :: [] -> cont (Sym s)
  | _ -> err "symbol->string required 1 argument."

let string_to_symbol args err cont = match args with
  | Sym s :: [] -> cont (Str s)
  | _ -> err "symbol->string required 1 argument."

let string_to_number args err cont = match args with
  | Str s :: [] -> (match int_of_string_opt s with
      | Some v -> cont (Num v)
      | None -> err "string->number: failed.")
  | _ -> err "string->numer required 1 argument."

let number_to_string args err cont = match args with
  | Num v :: [] -> cont (Str (string_of_int v))
  | _ -> err "number->string required 1 argument."

(* procedure *)
(* procedure? call/cc *)

let is_procedure args err cont = match args with
  | Procedure _ :: [] -> cont (Bool true)
  | _ :: [] -> cont (Bool false)
  | _ -> err "procedure? required 1 argument."

let callcc args err cont = match args with
  | Procedure p :: [] ->
    apply p (Cont cont :: []) err cont
  | _ -> err "callcc: illformed argument."


(* equivalent *)
(* eq?, neq?, equal? *)

let eq_impl lhs rhs = match (lhs, rhs) with
  | (Undefined, Undefined) -> true
  | (Nil, Nil) -> true
  | (Num u, Num v) when u = v -> true
  | (Bool a, Bool b) when a = b -> true
  | (Sym s, Sym t) when s = t -> true
  | (a, b) when a == b -> true
  | _ -> false

let eq args err cont = match args with
  | l :: r :: [] -> cont (Bool (eq_impl l r))
  | _ -> err "eq? required 2 argument."

let neq args err cont = match args with
  | l :: r :: [] -> cont (Bool (not (eq_impl l r)))
  | _ -> err "neq? required 2 argument."

let rec equal_impl lhs rhs = match (lhs, rhs) with
  | (Undefined, Undefined) -> true
  | (Nil, Nil) -> true
  | (Num u, Num v) when u = v -> true
  | (Bool a, Bool b) when a = b -> true
  | (Str s, Str t) when s = t -> true
  | (Sym s, Sym t) when s = t -> true
  | (Cons {car = la; cdr = ld}, Cons {car = ra; cdr = rd}) ->
    equal_impl la ra && equal_impl ld rd
  | (a, b) when a == b -> true
  | _ -> false

let equal args err cont = match args with
  | l :: r :: [] -> cont (Bool (equal_impl l r))
  | _ -> err "equal? required 2 argument."


(* memq use eq? *)
let rec memq_impl item = function
  | Cons {car = car; cdr = cdr } as l ->
    if eq_impl item car then Some l else memq_impl item cdr
  | Nil -> Some(Bool false)
  | _ -> None

let memq args err cont = match args with
  | item :: list :: [] -> (match memq_impl item list with
      | Some l -> cont l
      | None -> err "memq: argument must be list.")
  | _ -> err "memq required 2 arguments."

let rec member_impl item = function
  | Cons {car = car; cdr = cdr } as l ->
    if equal_impl item car then Some l else member_impl item cdr
  | Nil -> Some(Bool false)
  | _ -> None

let member args err cont = match args with
  | item :: list :: [] -> (match member_impl item list with
      | Some l -> cont l
      | None -> err "member: argument must be list.")
  | _ -> err "member required 2 arguments."

(* output *)
(* print *)

let print args err cont = match args with
  | item :: [] -> print_endline (show_sexpr item); cont Undefined
  | _ -> err "print required 1 arguments."

let display args err cont = match args with
  | item :: [] -> print_string (show_sexpr item); cont Undefined
  | _ -> err "display required 1 arguments."

