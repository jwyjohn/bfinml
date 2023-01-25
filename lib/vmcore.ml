[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-39"] 
[@@@warning "-33"] 

exception Todo

type 'a memory = 
  {ptr : int; now : 'a; left : 'a list; right : 'a list}

let initdatamem : char memory = 
  {ptr = 0; now = Char.chr 0; left = []; right = []}

let isrightend (mem : 'a memory) : bool = 
  match mem.right with
  | [] -> true
  | _ -> false

let goleft (mem : 'a memory) (initv : 'a) : 'a memory = 
  match mem.left with
  | x::xs -> {ptr = mem.ptr - 1; now = x; left = xs; right = mem.now :: mem.right}
  | [] -> {mem with ptr = mem.ptr - 1; now = initv; right = mem.now :: mem.right}

let goright (mem : 'a memory) (initv : 'a): 'a memory = 
  match mem.right with
  | x::xs -> {ptr = mem.ptr + 1; now = x; left = mem.now :: mem.left; right = xs}
  | [] -> {mem with ptr = mem.ptr + 1; now = initv; left = mem.now :: mem.left}

let rec gotopos (mem : 'a memory) (pos : int) (initv : 'a) : 'a memory  = 
  if pos = mem.ptr then mem 
  else 
    if pos < mem.ptr 
    then gotopos (goleft mem initv) pos initv
    else gotopos (goright mem initv) pos initv

let rec rewind (mem : 'a memory) (initv : 'a) = 
  match mem.left with
  | _x::_xs -> rewind (goright mem initv) initv
  | [] -> mem

let rec printdatamem (mem : char memory) : string = 
  match mem.right with
  | _x::_xs -> 
    (Printf.sprintf "[%6d] %2x \n" mem.ptr (Char.code mem.now)) 
    ^ printdatamem (goright mem (Char.chr 0))
  | [] -> ""


type instruction = 
  | Inc
  | Dec
  | Lft
  | Rgt
  | Opt
  | Ipt
  | Jez of int
  | Jmp of int
  | ERR

type bfcode = instruction list

type machine = {halt : bool; input : char list; inst : instruction memory; data : char memory}

let incchar (ch : char) = Char.chr (((Char.code ch) + 1) mod 256)
let decchar (ch : char) = Char.chr (((Char.code ch) + 255) mod 256)

exception ErrorInstruction of machine
exception ErrorNoInputBuf

let getchar (il : char list) : char * char list = 
  match il with
  | c::xs -> (c, xs)
  | [] -> raise ErrorNoInputBuf

let runstep (m : machine) : (char option) * machine = 
  if m.halt then (None, m) else
    let oc, newm = 
      match m.inst.now with
      | Inc -> (None, {m with data = {m.data with now = incchar m.data.now}})
      | Dec -> (None, {m with data = {m.data with now = decchar m.data.now}})
      | Lft -> (None, {m with data = goleft m.data (Char.chr 0)})
      | Rgt -> (None, {m with data = goright m.data (Char.chr 0)})
      | Opt -> (Some m.data.now, m)
      | Ipt -> 
        let c, cs = getchar m.input in 
          (None, {m with data = {m.data with now = c}; input = cs})
      | Jez i -> 
        if (Char.code m.data.now) = 0 
        then (None, {m with inst = gotopos m.inst i ERR})
        else (None, m)
      | Jmp i -> (None, {m with inst = gotopos m.inst i ERR})
      | _ -> raise (ErrorInstruction m)
    in 
    match newm.inst.right with
    | [] -> (oc, {newm with halt = true})
    | _ -> (oc, {newm with inst = goright newm.inst ERR})

let rec auxruntill maxc (m : machine) acc cnt = 
  (* print_int m.inst.ptr ; print_newline () ; *)
  if m.halt || cnt > maxc then (cnt, acc) else 
    let oc, nm = runstep m in
    match oc with
    | None -> auxruntill maxc nm acc (cnt + 1)
    | Some ch -> auxruntill maxc nm (acc ^ Char.escaped ch) (cnt + 1)

let runtill (m : machine) cycles = 
  let cnt, output = auxruntill cycles m "" 0 in
  print_int cnt ; print_newline () ; print_endline output

let runtillhalt (m : machine) = 
  let cnt, output = auxruntill 2147483647 m "" 0 in
  print_int cnt ; print_newline () ; print_endline output



