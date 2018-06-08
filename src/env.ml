open Syntax


let rec find env s =
  match Hashtbl.find_opt env.tbl s with
  | Some v -> Some v
  | None -> (match env.up with 
      | Some up -> find up s
      | None -> None)

let add env s v =
  Hashtbl.add env.tbl s v

let rec overwrite env s v =
  match Hashtbl.find_opt env.tbl s with
  | Some _ -> Hashtbl.replace env.tbl s v; Ok ()
  | None -> (match env.up with 
      | Some up -> overwrite up s v
      | None -> Error() )