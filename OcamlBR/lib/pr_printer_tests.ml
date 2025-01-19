(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Parser
open Pr_printer

let parse str =
  match parse_expr str with
  | Ok structure ->
    Stdlib.print_endline (Stdlib.Format.asprintf "%a" prpr_structure structure);
    Stdlib.print_endline (show_structure structure)
  | Error _ -> Stdlib.print_endline "Parsing failed"
;;

let%expect_test _ =
  parse "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 5";
  [%expect
    {|
    let rec factorial = (fun n -> if n = 0 then 1 else n * factorial n - 1) in factorial 5 ;;
    [(SEval
        (Elet (Recursive,
           (Evalue_binding (((PVar (Id "factorial")), None),
              (Efun (((PVar (Id "n")), None), [],
                 (Eif_then_else (
                    (Ebin_op (Eq, (Evar (Id "n")), (Econst (Int 0)))),
                    (Econst (Int 1)),
                    (Some (Ebin_op (Mult, (Evar (Id "n")),
                             (Efun_application ((Evar (Id "factorial")),
                                (Ebin_op (Sub, (Evar (Id "n")), (Econst (Int 1))
                                   ))
                                ))
                             )))
                    ))
                 ))
              )),
           [], (Efun_application ((Evar (Id "factorial")), (Econst (Int 5)))))))
      ]
    |}]
;;

let%expect_test _ =
  parse "if 5 > 6 then let x = 5 in x + 5";
  [%expect
    {|
    if 5 > 6 then let  x = 5 in x + 5 ;;
    [(SEval
        (Eif_then_else ((Ebin_op (Gt, (Econst (Int 5)), (Econst (Int 6)))),
           (Elet (Non_recursive,
              (Evalue_binding (((PVar (Id "x")), None), (Econst (Int 5)))),
              [], (Ebin_op (Add, (Evar (Id "x")), (Econst (Int 5)))))),
           None)))
      ]
    |}]
;;

let%expect_test _ =
  parse "let x = 5 in let y = 6 in (x < y) and (x == y)";
  [%expect {|
    Parsing failed
    |}]
;;

let%expect_test _ =
  parse "[1; [2; 3]; 4]";
  [%expect
    {|
  [1; [2; 3]; 4] ;;
  [(SEval
      (Elist
         [(Econst (Int 1)); (Elist [(Econst (Int 2)); (Econst (Int 3))]);
           (Econst (Int 4))]))
    ]
  |}]
;;

let%expect_test _ =
  parse "[1 + 2; 3 * 4; 5 - 6]";
  [%expect
    {|
  [1 + 2; 3 * 4; 5 - 6] ;;
  [(SEval
      (Elist
         [(Ebin_op (Add, (Econst (Int 1)), (Econst (Int 2))));
           (Ebin_op (Mult, (Econst (Int 3)), (Econst (Int 4))));
           (Ebin_op (Sub, (Econst (Int 5)), (Econst (Int 6))))]))
    ]
  |}]
;;

let%expect_test _ =
  parse "let a = (1, 2, 3)";
  [%expect
    {|
    let  a = (1, 2, 3) ;;
    [(SValue (Non_recursive,
        (Evalue_binding (((PVar (Id "a")), None),
           (Etuple ((Econst (Int 1)), (Econst (Int 2)), [(Econst (Int 3))])))),
        []))
      ]
  |}]
;;

let%expect_test _ =
  parse "1234 + 676 - 9002 * (52 / 2)";
  [%expect
    {|
    1234 + 676 - 9002 * 52 / 2 ;;
    [(SEval
        (Ebin_op (Sub, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 676)))),
           (Ebin_op (Mult, (Econst (Int 9002)),
              (Ebin_op (Div, (Econst (Int 52)), (Econst (Int 2))))))
           )))
      ]
    |}]
;;

let%expect_test _ =
  parse "if 1234 + 1 = 1235 then let x = 4 in (x, 2)";
  [%expect
    {|
    if 1234 + 1 = 1235 then let  x = 4 in (x, 2) ;;
    [(SEval
        (Eif_then_else (
           (Ebin_op (Eq, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 1)))),
              (Econst (Int 1235)))),
           (Elet (Non_recursive,
              (Evalue_binding (((PVar (Id "x")), None), (Econst (Int 4)))),
              [], (Etuple ((Evar (Id "x")), (Econst (Int 2)), [])))),
           None)))
      ]
    |}]
;;

let%expect_test _ =
  parse "39482309482390842309482438208 + 2";
  [%expect {| 
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let x = 5 in let y = 3 in x + y;; if 13 > 12 then let a = 2 in a - 4";
  [%expect
    {|
    let  x = 5 in let  y = 3 in x + y ;;
    if 13 > 12 then let  a = 2 in a - 4 ;;
    [(SEval
        (Elet (Non_recursive,
           (Evalue_binding (((PVar (Id "x")), None), (Econst (Int 5)))),
           [],
           (Elet (Non_recursive,
              (Evalue_binding (((PVar (Id "y")), None), (Econst (Int 3)))),
              [], (Ebin_op (Add, (Evar (Id "x")), (Evar (Id "y"))))))
           )));
      (SEval
         (Eif_then_else ((Ebin_op (Gt, (Econst (Int 13)), (Econst (Int 12)))),
            (Elet (Non_recursive,
               (Evalue_binding (((PVar (Id "a")), None), (Econst (Int 2)))),
               [], (Ebin_op (Sub, (Evar (Id "a")), (Econst (Int 4)))))),
            None)))
      ]
  |}]
;;

let%expect_test _ =
  parse "let x = 5 ;; if 13 > 12 then let a = 2 in a + x";
  [%expect
    {|
    let  x = 5 ;;
    if 13 > 12 then let  a = 2 in a + x ;;
    [(SValue (Non_recursive,
        (Evalue_binding (((PVar (Id "x")), None), (Econst (Int 5)))), []));
      (SEval
         (Eif_then_else ((Ebin_op (Gt, (Econst (Int 13)), (Econst (Int 12)))),
            (Elet (Non_recursive,
               (Evalue_binding (((PVar (Id "a")), None), (Econst (Int 2)))),
               [], (Ebin_op (Add, (Evar (Id "a")), (Evar (Id "x")))))),
            None)))
      ]
  |}]
;;

let%expect_test _ =
  parse "let x = match 3 with | 1 -> 10 | 2 -> 20 | _ -> 30 ;;";
  [%expect
    {|
  let  x = match 3 with | 1 -> 10 | 2 -> 20 | _ -> 30 ;;
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "x")), None),
         (Ematch ((Econst (Int 3)),
            (Ecase ((PConst (Int 1)), (Econst (Int 10)))),
            [(Ecase ((PConst (Int 2)), (Econst (Int 20))));
              (Ecase (PAny, (Econst (Int 30))))]
            ))
         )),
      []))
    ]
  |}]
;;

let%expect_test _ =
  parse "((5 + 6) * (4 - 7)) - 1232";
  [%expect
    {|
  (5 + 6) * (4 - 7) - 1232 ;;
  [(SEval
      (Ebin_op (Sub,
         (Ebin_op (Mult, (Ebin_op (Add, (Econst (Int 5)), (Econst (Int 6)))),
            (Ebin_op (Sub, (Econst (Int 4)), (Econst (Int 7)))))),
         (Econst (Int 1232)))))
    ]
  |}]
;;

let%expect_test _ =
  parse "let x = 5";
  [%expect
    {|
  let  x = 5 ;;
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "x")), None), (Econst (Int 5)))), []))
    ]
  |}]
;;

let%expect_test _ =
  parse "[1; 2; 3] = 1";
  [%expect
    {|
  [1; 2; 3] = 1 ;;
  [(SEval
      (Ebin_op (Eq,
         (Elist [(Econst (Int 1)); (Econst (Int 2)); (Econst (Int 3))]),
         (Econst (Int 1)))))
    ]
  |}]
;;

let%expect_test _ =
  parse "let x : int = 42;;";
  [%expect
    {|
  let  x : int = 42 ;;
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "x")), (Some int)), (Econst (Int 42)))),
      []))
    ]
  |}]
;;

let%expect_test _ =
  parse "let f : int -> string = fun x -> string_of_int x;;";
  [%expect
    {|
  let  f : int -> string = (fun x -> string_of_int x) ;;
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "f")), (Some int -> string)),
         (Efun (((PVar (Id "x")), None), [],
            (Efun_application ((Evar (Id "string_of_int")), (Evar (Id "x"))))))
         )),
      []))
    ]
  |}]
;;

let%expect_test _ =
  parse "let y : (int * string * bool) = (1, \"hello\", true);;";
  [%expect
    {|
  let  y : (int * string * bool) = (1, "hello", true) ;;
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "y")), (Some (int * string * bool))),
         (Etuple ((Econst (Int 1)), (Econst (String "hello")),
            [(Econst (Bool true))]))
         )),
      []))
    ]
  |}]
;;

let%expect_test _ =
  parse "let l : int list = [1; 2; 3];;";
  [%expect
    {|
  let  l : int list = [1; 2; 3] ;;
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "l")), (Some int list)),
         (Elist [(Econst (Int 1)); (Econst (Int 2)); (Econst (Int 3))]))),
      []))
    ]
  |}]
;;

let%expect_test _ =
  parse "let g : (int -> bool) list = [(fun x -> x > 0); (fun x -> x < 0)];;";
  [%expect
    {|
  let  g : (int -> bool) list = [(fun x -> x > 0); (fun x -> x < 0)] ;;
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "g")), (Some (int -> bool) list)),
         (Elist
            [(Efun (((PVar (Id "x")), None), [],
                (Ebin_op (Gt, (Evar (Id "x")), (Econst (Int 0))))));
              (Efun (((PVar (Id "x")), None), [],
                 (Ebin_op (Lt, (Evar (Id "x")), (Econst (Int 0))))))
              ])
         )),
      []))
    ]
  |}]
;;

let%expect_test _ =
  parse "let f : string -> (int -> bool) = fun x -> fun y -> x + y";
  [%expect
    {|
   let  f : string -> (int -> bool) = (fun x -> (fun y -> x + y)) ;;
   [(SValue (Non_recursive,
       (Evalue_binding (((PVar (Id "f")), (Some string -> (int -> bool))),
          (Efun (((PVar (Id "x")), None), [],
             (Efun (((PVar (Id "y")), None), [],
                (Ebin_op (Add, (Evar (Id "x")), (Evar (Id "y"))))))
             ))
          )),
       []))
     ]
  |}]
;;

let%expect_test _ =
  parse "let f = fun (3, true): int*bool -> 4";
  [%expect
    {|
   let  f = (fun ((3, true) : (int * bool)) -> 4) ;;
   [(SValue (Non_recursive,
       (Evalue_binding (((PVar (Id "f")), None),
          (Efun (
             ((PTuple ((PConst (Int 3)), (PConst (Bool true)), [])),
              (Some (int * bool))),
             [], (Econst (Int 4))))
          )),
       []))
     ]
  |}]
;;

let%expect_test _ =
  parse "let f ((3, true) : int * bool) x ([ 7; 5 ] : int list) = 4";
  [%expect
    {|
   let  f = (fun ((3, true) : (int * bool)) x ([7; 5] : int list) -> 4) ;;
   [(SValue (Non_recursive,
       (Evalue_binding (((PVar (Id "f")), None),
          (Efun (
             ((PTuple ((PConst (Int 3)), (PConst (Bool true)), [])),
              (Some (int * bool))),
             [((PVar (Id "x")), None);
               ((PList [(PConst (Int 7)); (PConst (Int 5))]), (Some int list))],
             (Econst (Int 4))))
          )),
       []))
     ]
  |}]
;;

let%expect_test _ =
  parse
    "type d = { aa: int }\n\
    \      type old = { aa: int list -> int ; bb: string option }\n\
    \      type new = { aa: int; bb: bool }\n\
    \      let f = fun x -> x\n\
    \      let g x = \n\
    \        (match x with\n\
    \        | [] -> 0\n\
    \        | hd :: snd :: tl -> hd\n\
    \        | h::tl -> 1)\n\
    \      let m = { aa = g [5; 13; 92] ; bb = f (not true) } \n\
    \      let p : old = { aa = g ; bb = Some \"hello\" }\n\
    \      type k = { s : old; z : new }\n\
    \      let i = { s = p; z = m }\n\
    \      let f x = x.aa\n\
    \      let h (x : old) = x.aa\n\
    \      let r = f m + p.aa [1; 3; 5; 7]";
  [%expect
    {|
      type d = { aa : int } ;;
      type old = { aa : int list -> int ; bb : (string) option } ;;
      type new = { aa : int ; bb : bool } ;;
      let  f = (fun x -> x) ;;
      let  g = (fun x -> match x with | [] -> 0 | hd :: snd :: tl -> hd | h :: tl -> 1) ;;
      let  m = { aa = g ([5; 13; 92]) ; bb = f (not (true)) } ;;
      let  p : old = { aa = g ; bb = (Some "hello") } ;;
      type k = { s : old ; z : new } ;;
      let  i = { s = p ; z = m } ;;
      let  f = (fun x -> x.aa) ;;
      let  h = (fun (x : old) -> x.aa) ;;
      let  r = f m + (p.aa) ([1; 3; 5; 7]) ;;
      [(SType ("d", (Sfield_decl ((Label "aa"), int)), []));
        (SType ("old", (Sfield_decl ((Label "aa"), int list -> int)),
           [(Sfield_decl ((Label "bb"), (string) option))]));
        (SType ("new", (Sfield_decl ((Label "aa"), int)),
           [(Sfield_decl ((Label "bb"), bool))]));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "f")), None),
              (Efun (((PVar (Id "x")), None), [], (Evar (Id "x")))))),
           []));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "g")), None),
              (Efun (((PVar (Id "x")), None), [],
                 (Ematch ((Evar (Id "x")), (Ecase ((PList []), (Econst (Int 0)))),
                    [(Ecase (
                        (PCons ((PVar (Id "hd")),
                           (PCons ((PVar (Id "snd")), (PVar (Id "tl")))))),
                        (Evar (Id "hd"))));
                      (Ecase ((PCons ((PVar (Id "h")), (PVar (Id "tl")))),
                         (Econst (Int 1))))
                      ]
                    ))
                 ))
              )),
           []));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "m")), None),
              (Erecord (
                 (Erecord_field ((Label "aa"),
                    (Efun_application ((Evar (Id "g")),
                       (Elist
                          [(Econst (Int 5)); (Econst (Int 13)); (Econst (Int 92))])
                       ))
                    )),
                 [(Erecord_field ((Label "bb"),
                     (Efun_application ((Evar (Id "f")),
                        (Eun_op (Not, (Econst (Bool true))))))
                     ))
                   ]
                 ))
              )),
           []));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "p")), (Some old)),
              (Erecord ((Erecord_field ((Label "aa"), (Evar (Id "g")))),
                 [(Erecord_field ((Label "bb"),
                     (Eoption (Some (Econst (String "hello"))))))
                   ]
                 ))
              )),
           []));
        (SType ("k", (Sfield_decl ((Label "s"), old)),
           [(Sfield_decl ((Label "z"), new))]));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "i")), None),
              (Erecord ((Erecord_field ((Label "s"), (Evar (Id "p")))),
                 [(Erecord_field ((Label "z"), (Evar (Id "m"))))]))
              )),
           []));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "f")), None),
              (Efun (((PVar (Id "x")), None), [],
                 (Efield_access ((Evar (Id "x")), (Label "aa")))))
              )),
           []));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "h")), None),
              (Efun (((PVar (Id "x")), (Some old)), [],
                 (Efield_access ((Evar (Id "x")), (Label "aa")))))
              )),
           []));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "r")), None),
              (Ebin_op (Add, (Efun_application ((Evar (Id "f")), (Evar (Id "m")))),
                 (Efun_application (
                    (Efield_access ((Evar (Id "p")), (Label "aa"))),
                    (Elist
                       [(Econst (Int 1)); (Econst (Int 3)); (Econst (Int 5));
                         (Econst (Int 7))])
                    ))
                 ))
              )),
           []))
        ]
   
  |}]
;;

let%expect_test _ =
  parse
    "\n\
    \      type inner = { x : int }\n\
    \      type outer = { x : string; inner : inner }\n\
    \      let x = { x = \"hello\" ; inner = { x = 34 } }\n\
    \      let y = if x.inner.x > 100 then print_int x.inner.x else print_endline x.x";
  [%expect
    {|
      type inner = { x : int } ;;
      type outer = { x : string ; inner : inner } ;;
      let  x = { x = "hello" ; inner = { x = 34 } } ;;
      let  y = if x.inner.x > 100 then print_int (x.inner.x) else print_endline (x.x) ;;
      [(SType ("inner", (Sfield_decl ((Label "x"), int)), []));
        (SType ("outer", (Sfield_decl ((Label "x"), string)),
           [(Sfield_decl ((Label "inner"), inner))]));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "x")), None),
              (Erecord ((Erecord_field ((Label "x"), (Econst (String "hello")))),
                 [(Erecord_field ((Label "inner"),
                     (Erecord ((Erecord_field ((Label "x"), (Econst (Int 34)))),
                        []))
                     ))
                   ]
                 ))
              )),
           []));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "y")), None),
              (Eif_then_else (
                 (Ebin_op (Gt,
                    (Efield_access (
                       (Efield_access ((Evar (Id "x")), (Label "inner"))),
                       (Label "x"))),
                    (Econst (Int 100)))),
                 (Efun_application ((Evar (Id "print_int")),
                    (Efield_access (
                       (Efield_access ((Evar (Id "x")), (Label "inner"))),
                       (Label "x")))
                    )),
                 (Some (Efun_application ((Evar (Id "print_endline")),
                          (Efield_access ((Evar (Id "x")), (Label "x"))))))
                 ))
              )),
           []))
        ]
   
  |}]
;;

let%expect_test _ =
  parse
    "\n\
    \      type d = { aa: int}\n\
    \      type t = { aa: int; bb: bool }\n\
    \      type nested = {raz: t; dva: d}\n\
    \      let a = { raz = { aa = 4; bb = true}; dva = {aa = 10} }\n\
    \      let b = { aa = 100; bb = not false }\n\
    \      let c = (b.aa + a.raz.aa) / a.dva.aa";
  [%expect
    {|
      type d = { aa : int } ;;
      type t = { aa : int ; bb : bool } ;;
      type nested = { raz : t ; dva : d } ;;
      let  a = { raz = { aa = 4 ; bb = true } ; dva = { aa = 10 } } ;;
      let  b = { aa = 100 ; bb = not (false) } ;;
      let  c = (b.aa + a.raz.aa) / a.dva.aa ;;
      [(SType ("d", (Sfield_decl ((Label "aa"), int)), []));
        (SType ("t", (Sfield_decl ((Label "aa"), int)),
           [(Sfield_decl ((Label "bb"), bool))]));
        (SType ("nested", (Sfield_decl ((Label "raz"), t)),
           [(Sfield_decl ((Label "dva"), d))]));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "a")), None),
              (Erecord (
                 (Erecord_field ((Label "raz"),
                    (Erecord ((Erecord_field ((Label "aa"), (Econst (Int 4)))),
                       [(Erecord_field ((Label "bb"), (Econst (Bool true))))]))
                    )),
                 [(Erecord_field ((Label "dva"),
                     (Erecord ((Erecord_field ((Label "aa"), (Econst (Int 10)))),
                        []))
                     ))
                   ]
                 ))
              )),
           []));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "b")), None),
              (Erecord ((Erecord_field ((Label "aa"), (Econst (Int 100)))),
                 [(Erecord_field ((Label "bb"),
                     (Eun_op (Not, (Econst (Bool false))))))
                   ]
                 ))
              )),
           []));
        (SValue (Non_recursive,
           (Evalue_binding (((PVar (Id "c")), None),
              (Ebin_op (Div,
                 (Ebin_op (Add, (Efield_access ((Evar (Id "b")), (Label "aa"))),
                    (Efield_access (
                       (Efield_access ((Evar (Id "a")), (Label "raz"))),
                       (Label "aa")))
                    )),
                 (Efield_access ((Efield_access ((Evar (Id "a")), (Label "dva"))),
                    (Label "aa")))
                 ))
              )),
           []))
        ]
   
  |}]
;;
