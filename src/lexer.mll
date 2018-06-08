{
  open Parser

  type strlit_state =
    | Tok of string
    | Fin
}

let space = ['\t' '\n' '\r' ' ']

let identifier = 
  ['0'-'9' 'A'-'Z' 'a'-'z'
   '!' '$' '%' '&' '*' '+'
   '-' '.' '/' '<' '=' '>'
   '?' '@' '^' '_' ]+

let integer = '0' | ['1'-'9']['0'-'9']*

rule token = parse
  | space+ { token lexbuf }
  | '(' { LP }
  | ')' { RP }
  | '.' { DOT }
  | '\'' { QUOTE }
  | ';' { comment lexbuf; token lexbuf }
  | '"'
    {
      let rec collect x =
        match (string_literal lexbuf) with
        | Tok s -> collect (x ^ s)
        | Fin -> x
      in
      STRLIT (collect "")
    }
  | "#t" { TRUE }
  | "#f" { FALSE }
  | integer as i { INT (int_of_string i) }
  | identifier as id { SYM id }
  | eof { EOF }

and comment = parse
  | ('\n' | eof) { () }
  | _ { comment lexbuf }

and string_literal = parse
  | '"'      { Fin }
  | '\\' 'n' { Tok "\n" }
  | '\\' 't' { Tok "\t" }
  | '\\' 'r' { Tok "\r" }
  | '\\' '\\' { Tok "\\" }
  | [^'"' '\\']* as s  { Tok s }
  