[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-39"] 
[@@@warning "-33"] 

open Parserutil

(*

    match ch with
    | '+' -> auxparse s (n+1) st (Inc :: acc)
    | '-' -> auxparse s (n+1) st (Dec :: acc)
    | '>' -> auxparse s (n+1) st (Rgt :: acc)
    | '<' -> auxparse s (n+1) st (Lft :: acc)
    | '.' -> auxparse s (n+1) st (Opt :: acc)
    | ',' -> auxparse s (n+1) st (Ipt :: acc)
    | '[' -> auxparse s (n+1) (n::st) (Jez n :: acc)
    | ']' -> ...

grammar
--------------------------------
oper := + | - | > | < | . | , 
stmt := loop stmt | oper stmt | oper | loop

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