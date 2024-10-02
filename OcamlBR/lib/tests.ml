(** Copyright 2021-2024, mshipilov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Parser
open Interpreter
open Interpreter.PP


let pp_interpret_expr ast =
  (match run_expr_interpreter ast with
    | Ok value -> print_value value
    | Error e -> print_error e)
;;

let pp_run code =
  match Parser.parse_expr code with
  | Ok e -> pp_interpret_expr e
  | _ -> Stdlib.print_endline "Parsing failed"
;;

let%expect_test _ =
  pp_run "let rec fact n = if n < 2 then 1 else n * fact (n - 1) in fact 5";
  [%expect {| 120 |}]
;;