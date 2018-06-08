open Printf
open Repl

let ( ** ) g f = fun x -> g (f x)


let () =
  repl()