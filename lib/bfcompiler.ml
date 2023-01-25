[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-39"] 
[@@@warning "-33"] 

exception Todo

open Bfparser
open Vmcore

let rec auxcomp (ast : bfast) (n : int) : bfcode = 
  match ast with
  | Oper oper -> 
    (
      match oper with
      | Inc -> [Inc]
      | Dec -> [Dec]
      | Lft -> [Lft]
      | Rgt -> [Rgt]
      | Put -> [Opt]
      | Get -> [Ipt]
    )
  | Stmt stmt -> 
    (
      match stmt with
      | [] -> []
      | x::xs -> 
          let lstmt = Stmt xs in 
          let inst = (auxcomp x n) in 
          let pos = n + List.length inst in
            inst @ (auxcomp lstmt pos)
    )
  | Loop astc -> 
    (
      let r = auxcomp astc (n+1) in 
        [Jez (n + 1 + List.length r)] @ r @ [Jmp (n-1)]
    )

exception ParseFailed
exception ParseFailedRemains of string

let compilebf (s : string) = 
  match parsebf s with
  | None -> raise ParseFailed
  | Some (ast, left) -> 
      if left = "" 
      then auxcomp ast 0 
      else raise (ParseFailedRemains left)