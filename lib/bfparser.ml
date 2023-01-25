[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-39"] 
[@@@warning "-33"] 

open Parserutil

(*

grammar
--------------------------------
oper := + | - | > | < | . | , 
stmt := loop stmt | oper stmt | oper | loop
loop := [ stmt ]

*)

type bfoper = 
  | Inc
  | Dec
  | Lft
  | Rgt
  | Put
  | Get

type bfast = 
  | Oper of bfoper
  | Stmt of bfast list
  | Loop of bfast

let incP = satP (fun (ch) -> ch = '+') >> returnP Inc
let decP = satP (fun (ch) -> ch = '-') >> returnP Dec
let leftP = satP (fun (ch) -> ch = '<') >> returnP Lft
let rightP = satP (fun (ch) -> ch = '>') >> returnP Rgt
let inputP = satP (fun (ch) -> ch = ',') >> returnP Get
let outputP = satP (fun (ch) -> ch = '.') >> returnP Put

let lpP = satP (fun (ch) -> ch = '[')
let rpP = satP (fun (ch) -> ch = ']')

let operP = 
  (incP <|> decP <|> leftP <|> rightP <|> inputP <|> outputP) 
  >>= fun (r) -> returnP (Oper r)

let rec stmtP () = 
  (
    operP >>= fun (x) -> 
    (stmtP ()) >>= fun (y) -> 
    match y with
    | Stmt zs -> returnP (Stmt (x::zs))
    | z -> returnP (Stmt [x; z])
  ) 
  <|>
  (
    (loopP ()) >>= fun (x) -> 
    (stmtP ()) >>= fun (y) -> 
    match y with
    | Stmt zs -> returnP (Stmt (x::zs))
    | z -> returnP (Stmt [x; z])
  ) 
  <|> loopP () <|> operP
and loopP () =
  lpP >>= fun _ ->
  (stmtP ()) >>= fun (x) -> 
  rpP >>= fun _ -> returnP (Loop x)

let parsebf s = stmtP () s