open Utils

type name = string

type instruction =
  | Unwind
  | Pushglobal of name
  | Pushint of int
  | Push of int
  | Mkap
  | Slide of int

type gmCode = instruction list

type node =
  | NNum of int
  | NAp of (addr * addr)
  | NGlobal of (int * gmCode)
			  
type gmStack = addr list

type gmHeap = node heap

type gmGlobals = (name * addr) list

type gmStats = int
			  
type gmState = gmCode * gmStack * gmHeap * gmGlobals * gmStats

let getCode (code, stack, heap, globals, stats) = code
let putCode code' (code, stack, heap, globals, stats) = (code', stack, heap, globals, stats)
let getStack (code, stack, heap, globals, stats) = stack
let putStack stack' (code, stack, heap, globals, stats) = (code, stack', heap, globals, stats)
let getHeap (code, stack, heap, globals, stats) = heap
let putHeap heap' (code, stack, heap, globals, stats) = (code, stack, heap', globals, stats)
let getGlobals (code, stack, heap, globals, stats) = globals
let statInitial = 0
let statIncSteps n = n + 1
let statGetSteps s = s
let getStats (code, stack, heap, globals, stats) = stats
let putStats stats' (code, stack, heap, globals, stats) = (code, stack, heap, globals, stats')

let doAdmin s = putStats (statIncSteps (getStats s)) s

let gmFinal s =
  match getCode s with
  | [] -> true
  | _ -> false

let pushglobal f state =
  let a = List.assoc f (getGlobals state) in
  putStack (a::getStack state) state

let pushint n state =
  let (heap', a) = hAlloc (getHeap state) (NNum n) in
  putHeap heap' (putStack (a::getStack state) state)

let mkap state =
  let a1::a2::as' = getStack state in
  let (heap', a) = hAlloc (getHeap state) (NAp (a1, a2)) in
  putHeap heap' (putStack (a::as') state)

let getArg = function
  | NAp (a1, a2) -> a2
  | _ -> failwith "getArg"
	  
let push n state =
  let as' = getStack state in
  let a = getArg (hLookup (getHeap state) (List.nth as' (n + 1))) in
  putStack (a::as') state

let slide n state =
  let rec drop n = function
    | [] -> []
    | x::xs -> if n = 0 then x::xs else drop (n - 1) xs in
  let a::a' = getStack state in
  putStack (a::drop n a') state

let unwind state =
  let a::a' = getStack state in
  let heap = getHeap state in
  let newState = function
    | NNum n -> state
    | NAp (a1, a2) -> putCode [Unwind] (putStack (a1::a::a') state)
    | NGlobal (n, c) -> if List.length a' < n then failwith "Unwinding with too few arguments" else putCode c state in
  newState (hLookup heap a)
	   
let dispatch = function
  | Pushglobal f -> pushglobal f
  | Pushint n -> pushint n
  | Mkap -> mkap
  | Push n -> push n
  | Slide n -> slide n
  | Unwind -> unwind

let step s =
  match getCode s with
  | [] -> failwith "empty code impossible"
  | i::is -> dispatch i (putCode is s)

let rec eval s = 
  let restStates = if gmFinal s then [] else eval (doAdmin (step s)) in
  s::restStates
