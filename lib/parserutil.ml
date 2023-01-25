[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-39"] 
[@@@warning "-33"] 

exception Todo

(* string and char util functions  *)
let explode s = List.of_seq (String.to_seq s)
let implode ls = String.of_seq (List.to_seq ls)
let isSpace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'
let isDigit c = '0' <= c && c <= '9'
let isLower c = 'a' <= c && c <= 'z'
let isUpper c = 'A' <= c && c <= 'Z'
let isLetter c = isLower c || isUpper c

(* parser type definition *)
type 'a parser = string -> ('a * string) option

(* `returnP v` always success *)
let returnP : 'a -> 'a parser = fun (v) -> fun (inp) -> Some (v, inp)

(* `failP` always fail *)
let failP : 'a parser = fun (inp) -> None

(* `getchP` eats a char *)
let getchP : char parser = 
  fun (inp) -> 
    match explode inp with
    | [] -> None
    | x::xs -> Some (x, implode xs)

(* Monad bind *)
let bindP (p : 'a parser) (f : 'a -> 'b parser) : 'b parser = 
  fun (inp) -> 
    match p inp with
    | None -> None
    | Some (v, inp') -> f v inp'
let (>>=) p f = bindP p f
let (>>) p f = p >>= fun _ -> f
let (<|>) (f : 'a parser) (g : 'a parser) : 'a parser = 
  fun (inp) -> 
    match f inp with
    | None -> g inp
    | res -> res

let rec manyP p  = many1P p <|> returnP []
and many1P p = 
  p >>= fun (x) -> 
  manyP p >>= fun (xs) ->
  returnP (x::xs)

(* if a char satisfies a char -> bool *)
let satP (f : char -> bool) : char parser = 
  getchP >>= fun (ch) -> if f ch then returnP ch else failP

let letterP = satP isLetter
let upperP = satP isUpper
let lowerP = satP isLower
let spaceP = satP isSpace
let digitP = satP isDigit