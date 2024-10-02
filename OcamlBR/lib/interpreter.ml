[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Base

type value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VUnit
  | VTuple of value list
  | VList of value list
  | VFun of pattern * expr * binding

and binding = (id, value, Base.String.comparator_witness) Base.Map.t

type error =
  | UnknownVariable of string
  | DivisionByZero
  | TypeError
  | UnboundValue of string
  | PatternMatchingFailed

module type Monad = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module Env (M : Monad) = struct
  open M

  let empty = Base.Map.empty (module Base.String)
  let extend key value env = Base.Map.update env key ~f:(fun _ -> value)

  let find map key =
    match Base.Map.find map key with
    | Some value -> return value
    | None -> fail (UnboundValue key)
end

module Interpret (M : Monad) = struct
  open M
  open Env (M)

  type environment = (id * value) list


  let eval_bin_op op v1 v2 =
    match op, v1, v2 with
    | Add, VInt x, VInt y -> return (VInt (x + y))
    | Sub, VInt x, VInt y -> return (VInt (x - y))
    | Mult, VInt x, VInt y -> return (VInt (x * y))
    | Div, VInt _, VInt 0 -> fail DivisionByZero
    | Div, VInt x, VInt y -> return (VInt (x / y))
    | Gt, VInt x, VInt y -> return (VBool (x > y))
    | Lt, VInt x, VInt y -> return (VBool (x < y))
    | Eq, VInt x, VInt y -> return (VBool (x = y))
    | Neq, VInt x, VInt y -> return (VBool (x <> y))
    | Gte, VInt x, VInt y -> return (VBool (x >= y))
    | Lte, VInt x, VInt y -> return (VBool (x <= y))
    | And, VBool x, VBool y -> return (VBool (x && y))
    | Or, VBool x, VBool y -> return (VBool (x || y))
    | _, _, _ -> fail TypeError 
  ;;

  let eval_pattern env = function
    | PAny, _ -> Some env
    | PConst (Int x), VInt v when x = v -> Some env
    | PConst (Bool x), VBool v when Bool.equal x v -> Some env
    | PConst Unit, VUnit -> Some env
    | PVar id, v -> Some (extend id v env)
    | _ -> None
  ;;
  

  let eval_un_op op v =
    match op, v with
    | Negative, VInt x -> return (VInt (-x))
    | Not, VBool b -> return (VBool (not b))
    | _ -> fail TypeError

  let lookup env x =
    let* v = find env x in
    match v with
    | VFun (p, e, env) -> return (VFun (p, e, extend x v env))
    | v -> return v

    let eval_const = function
    | Int i -> return (VInt i)
    | Bool b -> return (VBool b)
    | Char c -> return (VChar c)
    | String s -> return (VString s)
    | Unit -> return VUnit
  

  let eval_expr =
    let rec eval env = function
      | Econst e -> eval_const e

      | Evar v -> lookup env v

      | Ebin_op (op, e1, e2) ->
        let* v1 = eval env e1 in
        let* v2 = eval env e2 in
        eval_bin_op op v1 v2

      | Eif_then_else (e1, e2, e3_opt) ->
      let* v1 = eval env e1 in
      (match v1 with
        | VBool true -> eval env e2
        | VBool false -> (
            match e3_opt with
            | Some e3 -> eval env e3
            | None -> return VUnit
          )
        | _ -> fail TypeError)

      | Elet ((Non_recursive, _, e1), EUnit) -> eval env e1
      | Elet ((Recursive, _, e1), EUnit) -> eval env e1
      | Elet ((Non_recursive, x, e1), e2) ->
        let* v1 = eval env e1 in
        let env' = extend x v1 env in
        let* v2 = eval env' e2 in
        return v2
      | Elet ((Recursive, x, e1), e2) ->
        let* v1 = eval env e1 in
        let v2 =
          match v1 with
          | VFun (x, e, _) -> VFun (x, e, env)
          | _ -> v1
        in
        let env' = extend x v2 env in
        eval env' e2

      | Efun (x, e) -> return (VFun (x, e, env))

      | Efun_application (e1, e2) ->
        let* v1 = eval env e1 in
        let* v2 = eval env e2 in
        (match v1 with
          | VFun (x, e, env) ->
            (match eval_pattern env (x, v2) with
            | Some env -> eval env e
            | None -> fail PatternMatchingFailed)
          | _ -> fail TypeError )
        
      | _ -> fail TypeError
    in
    eval
  ;;

  let interpret_expr expr = eval_expr empty expr

end


module Run (M : Monad) = struct
  let run expr =
    let module Interpreter = Interpret(M) in
    Interpreter.interpret_expr expr
end


module R = struct
  include Base.Result

  let ( let* ) m f = m >>= fun x -> f x
end


module InterpretResult = Interpret (R)

let run_expr_interpreter = InterpretResult.interpret_expr


module PP = struct
  open Format

  let pp_value ppf = function
    | VInt x -> fprintf ppf "%d" x
    | VBool x -> fprintf ppf "%b" x
    | VUnit -> fprintf ppf "()"
    | VFun _ -> fprintf ppf "<fun>"
  ;;

  let pp_error ppf : error -> unit = function
    | DivisionByZero -> fprintf ppf "Division by zero"
    | UnboundValue s -> fprintf ppf "Unbound value %s" s
    | TypeError -> fprintf ppf "Operator and operand type mismatch"
    | PatternMatchingFailed -> fprintf ppf "Mismatch between function and arguments"
    | UnknownVariable var -> fprintf ppf "Unknown variable: %s" var
  ;;

  let print_value = printf "%a" pp_value
  let print_error = printf "%a" pp_error
end
