%{
  open Syntax
%}

%token LP
%token RP
%token DOT
%token QUOTE
%token TRUE
%token FALSE
%token <int> INT
%token <string> STRLIT
%token <string> SYM
%token EOF

%start <Syntax.sexpr option> main

%%

main:
  | expr { Some $1 }
  | EOF  { None }

expr:
  | LP nonempty_list(expr) option(dot_pair) RP
    {
      let ini = match $3 with
                  | Some e -> e
                  | None -> Nil
      in
      List.fold_right (fun x acc -> Cons {car = x; cdr = acc}) $2 ini
    }
  | QUOTE expr { Cons { car = Sym "quote"; cdr = Cons {car = $2; cdr = Nil}} }
  | atom { $1 }

dot_pair:
  | DOT expr { $2 }

atom:
  | LP RP  { Nil }
  | TRUE   { Bool true }
  | FALSE  { Bool false }
  | INT    { Num $1 }
  | STRLIT { Str $1 }
  | SYM    { Sym $1 }


%%