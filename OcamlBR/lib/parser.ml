(** Copyright 2021-2024, mshipilov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)


open Angstrom
open Base
open Ast



let is_keyword = function
  | "let" | "in" | "fun" | "rec" | "if" | "then" | "else" |"true" | "false" -> true
  | _ -> false
;;


let pws = take_while Char.is_whitespace
let pstoken s = pws *> string s
let ptoken s = pws *> s
let pparens p = pstoken "(" *> p <* pstoken ")"


let chain e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;


let pint =
  let sign = choice [ pstoken "-"; pstoken "+"; pstoken "" ] in
  let rest = take_while1 Char.is_digit in
  lift2 (fun sign rest -> Int.of_string (sign ^ rest)) sign rest >>| fun x -> Int x
;;

let pbool =
  choice [ pstoken "true" *> return true; pstoken "false" *> return false ]
  >>| fun x -> Bool x
;;

let pstr = char '"' *> take_till (Char.equal '"') <* char '"' >>| fun x -> String x
let punit = pstoken "()" >>| fun _ -> Unit
let const = choice [ pint; pbool; pstr; punit ]


let varname =
  let pfirst =
    satisfy (fun ch -> Char.is_alphanum ch || Char.equal ch '_') >>| Char.escaped
  in
  let prest =
    take_while (fun ch -> Char.is_alphanum ch || Char.is_digit ch || Char.equal ch '_')
  in
  let varname = lift2 (fun x y -> x ^ y) pfirst prest in
  ptoken varname
  >>= fun s ->
  if is_keyword s then fail "Unsupported keyword has been introduced" else return s
;;


let pvar = varname >>| fun x -> PVar x
let pconst = const >>| fun x -> PConst x

let pEconst = const >>| fun x -> Econst x
let pEvar = varname >>| fun x -> Evar x

let peapp e =
  let func_call =
    lift2 (fun fn arg -> Efun_application (fn, arg)) e e
  in
  chain e (return (fun e1 e2 -> Efun_application (e1, e2))) <|> func_call
;;


let ptint = pstoken "int" *> return (Int 0)
let ptstring = pstoken "string" *> return (String "")
let ptbool = pstoken "bool" *> return (Bool false)
let pty = choice [ ptint; ptstring; ptbool ]


let parsebinop op token =
  pws *> pstoken token *> return (fun e1 e2 -> Ebin_op (op, e1, e2))
;;


let add = parsebinop Add "+"
let sub = parsebinop Sub "-"
let mult = parsebinop Mult "*"
let div = parsebinop Div "/"


let pbranch pexpr =
  lift3
    (fun e1 e2 e3 -> Eif_then_else (e1, e2, Some e3))
    (pstoken "if" *> pexpr)
    (pstoken "then" *> pexpr)
    (pstoken "else" *> pexpr)
;;


let rel =
  choice
    [ parsebinop Eq "="
    ; parsebinop Neq "<>"
    ; parsebinop Lt "<"
    ; parsebinop Gt ">"
    ; parsebinop Lte "<="
    ; parsebinop Gte ">="
    ]
;;


let ppattern =
  choice
    [pconst; pvar]
;;


let plet pexpr =
  let rec pbody pexpr =
    ppattern >>= function
    | PVar id -> pbody pexpr <|> (pstoken "=" *> pexpr >>| fun e -> Efun ([id], e))
    | _ -> failwith "Only variable patterns are supported"
  in
  pstoken "let"
  *> lift4
       (fun r id e1 e2 -> Elet (r, id, e1, e2))
       (pstoken "rec" *> return Recursive <|> return Non_recursive)
       (pparens varname <|> varname)
       (pstoken "=" *> pexpr <|> pbody pexpr)
       (pstoken "in" *> pexpr <|> return (Econst Unit))


let pexpr =
  fix (fun expr ->
    let expr = choice [ pEconst; pEvar; pparens expr] in
    let expr = peapp expr <|> expr in
    let expr = chain expr (mult <|> div) in
    let expr = chain expr (add <|> sub) in
    let expr = chain expr rel in
    let expr = pbranch expr <|> expr in
    let expr = plet expr <|> expr in
    expr)
  ;;


let parse str =
  match parse_string ~consume:All pexpr str with
  | Ok ast -> print_endline (show_expr ast)
  | Error msg -> Printf.printf "Parsing error: %s\n" msg
;;