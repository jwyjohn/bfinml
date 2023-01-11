[@@@warning "-33"] 

open Bfinml.Bfcore

let makeinsts il initv = 
  match il with
  | [] -> { ptr = 0; now = initv; left = []; right = [] }
  | x::xs ->  { ptr = 0; now = x; left = []; right = xs }

let hello = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

let mymachine = 
  { halt = false; 
    input = []; 
    inst = makeinsts (parse hello) ERR;
    data = makeinsts [] (Char.chr 0) 
  }

(* let () = print_endline "Hello, World!" *)

let _ = runtillhalt mymachine
