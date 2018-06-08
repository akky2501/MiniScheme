
type sexpr =
  | Cons of cell
  | Undefined
  | Nil
  | Num of int
  | Bool of bool
  | Str of string
  | Sym of string
  | Special of (sexpr -> env -> err_cont_t -> eval_cont_t -> sexpr option)
  | Primitive of (sexpr list -> err_cont_t -> eval_cont_t -> sexpr option)
  | Cont of eval_cont_t
  | Procedure of procedure
  | Macro of procedure
and cell = { mutable car: sexpr; mutable cdr: sexpr }
and procedure = { formals: string list; va: string option; body: sexpr; env: env }
and env = { up: env option; tbl: (string, sexpr) Hashtbl.t }
and eval_cont_t = sexpr -> sexpr option
and err_cont_t = string -> sexpr option


let rec show_sexpr = function
  | Cons {car = h; cdr = t} -> "Cons(" ^ (show_sexpr h) ^ ", " ^ (show_sexpr t) ^ ")"
  | Undefined -> "#undef"
  | Nil -> "Nil"
  | Num v -> string_of_int v
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Str s -> "\"" ^ s ^ "\""
  | Sym s -> "Sym(" ^ s ^ ")"
  | Special s -> "Special"
  | Primitive _ -> "PrimitiveFunc"
  | Cont _ -> "Cont"
  | Procedure _ -> "Procedure"
  | Macro _ -> "Macro"


let rec print_env {up = up; tbl = tbl;} n =
  Hashtbl.iter (fun k v -> Printf.printf "%d -> %s: %s\n" n k (show_sexpr v)) tbl;
  match up with Some up -> print_env up (n+1) | None -> ()
