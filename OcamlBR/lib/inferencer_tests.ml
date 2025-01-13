(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Inferencer.Infer


let infer_from_file file_name =
  let file_path = "../tests/inferencer_tests/" ^ file_name in
  let input = 
    let ic = open_in file_path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    content
  in
  let result = infer_program_test input in
  result

let%expect_test "do_not_type_001" =
  let _ = infer_from_file "do_not_type/001.ml" in
  [%expect {| val ..3232. |}]
(*
let%expect_test "do_not_type_002if" =
  let _ = infer_from_file "do_not_type/002if.ml" in
  [%expect {| val ... |}]
*)






let%expect_test _ =
  let _ = infer_program_test {|let n x = x in let f g = g 3 in f n  |} in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let f x = x |} in
  [%expect {| val f : '0 -> '0 |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let f x = x + 2 |} in
  [%expect {| val f : int -> int|}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let x = 2 in x = 1 |} in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let rec fac n = if n < 1 then 1 else n * fac (n - 1) |} in
  [%expect {| val fac : int -> int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let rec x = 1+ x|} in
  [%expect {| val x : int |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test
      {|let rec is_even n = if n = 0 then true else is_odd (n - 1) and is_odd n = if n = 0 then false else is_even (n - 1)|}
  in
  [%expect {|
    val is_even : int -> bool
    val is_odd : int -> bool |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test {|let square x = x*x in let id x = x in (id square) (id  2) |}
  in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let x = 2 in let a = true in not a && x |} in
  [%expect {| Infer error: Unification failed on int and bool |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let rec f x = f |} in
  [%expect {| Infer error: Occurs check failed |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let a = if true then 2 + 9 else 1 |} in
  [%expect {| val a : int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| if a then 2 else 1 |} in
  [%expect {| Infer error: Undefined variable "a" |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| fun x y -> x + y |} in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let f x y z w = if y&&z then x else  w + 1 |} in
  [%expect {| val f : int -> (bool -> (bool -> (int -> int))) |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let a = fun x::y::z::w -> if z > 0 then y else x |} in
  [%expect {| val a : int list -> int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let b = fun (a,b,(2::t), d) -> a + d  |} in
  [%expect {| val b : (int * '1 * int list * int) -> int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let (<|>) a b = a/b + b*a |} in
  [%expect {| val <|> : int -> (int -> int) |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let w [2; v] (y, dx, d) = (-4, 5+v, true&&d) |} in
  [%expect {| val w : int list -> (('2 * '3 * bool) -> (int * int * bool)) |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test {|let f = fun ((3, true): int*bool) x -> if x then 4 else 0  |}
  in
  [%expect {| val f : (int * bool) -> (bool -> int) |}]
;;






(*delete after*)

let%expect_test _ =
  let _ =
    infer_program_test {| 
    let rec fac n = if n<=1 then 1 else n * fac (n-1)

let main =
  let () = print_int (fac 4) in
  0
  |}
  in
  [%expect {||}]
;;


let%expect_test _ =
  let _ =
    infer_program_test {|
    let rec fac_cps n k =
  if n=1 then k 1 else
  fac_cps (n-1) (fun p -> k (p*n))

let main =
  let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
  0

  |}
  in
  [%expect {|  |}]
;;


let%expect_test _ =
  let _ =
    infer_program_test {|
    let rec fib_acc a b n =
  if n=1 then b
  else
    let n1 = n-1 in
    let ab = a+b in
    fib_acc b ab n1

let rec fib n =
  if n<2
  then n
  else fib (n - 1) + fib (n - 2) 

let main =
  let () = print_int (fib_acc 0 1 4) in
  let () = print_int (fib 4) in
  0

 |}
  in
  [%expect {| |}]
;;


let%expect_test _ =
  let _ =
    infer_program_test {|
    let wrap f = if 1 = 1 then f else f

let test3 a b c =
  let a = print_int a in
  let b = print_int b in
  let c = print_int c in
  0

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

let main =
  let rez =
      ((wrap test10) 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)
  in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0

  |}
  in
  [%expect {|  |}]
;;


let%expect_test _ =
  let _ =
    infer_program_test {|

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

  let rez =
      (test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)

  |}
  in
  [%expect {|  |}]
;;


let%expect_test _ =
  let _ =
    infer_program_test {|
    let rec fix f x = f (fix f) x

let fac self n = if n<=1 then 1 else n * self (n-1)

let main =
  let () = print_int (fix fac 6) in
  0

 |}
  in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test {|
let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

let foo x = foo true (foo false (foo true (foo false x)))
let main =
  let () = print_int (foo 11) in
  0
 |}
  in
  [%expect {|  |}]
;;


let%expect_test _ =
  let _ =
    infer_program_test {|
let _start () () a () b _c () d __ =
  let () = print_int (a+b) in
  let () = print_int __ in
  a*b / _c + d

let main =
  print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555))
 |}
  in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test {|

    let addi = fun f g x -> (f x (g x: bool) : int)

    let main =
      let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
      0
 |}
  in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test {|
let temp =
  let f = fun x -> x in
  (f 1, f true)
 |}
  in
  [%expect {|  |}]
;;


let%expect_test _ =
  let _ =
    infer_program_test {|
let _1 = fun x y (a, _) -> (x + y - a) = 1

let _2 =
    let x, Some f = 1, Some ( ( + ) 4 )
    in f x

let _3 =  Some (1, "hi")

let _4 = let rec f x = f 5 in f

let _5 =
    let id x = x in
    match Some id with
      | Some f -> let _ = f "42" in f 42
      | None -> 0

let _6 = fun arg -> match arg with Some x -> let y = x in y

let int_of_option = function Some x -> x | None -> 0

let _42 = function 42 -> true | _ -> false

let id1, id2 = let id x = x in (id, id)
 |}
  in
  [%expect {|  |}]
;;