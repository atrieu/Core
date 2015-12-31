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
  | Nap of (addr * addr)
  | NGlobal of (int * gmCode)
			  
type gmStack = addr list

type gmHeap = heap node

type gmGlobals = Assoc of (name * addr)

type gmStats = int
			  
type gmState = gmCode * gmStack * gmHeap * gmGlobals * gmStats

							 
