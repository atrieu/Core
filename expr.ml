type name = string

type isRec =
  | Recursive
  | NonRecursive

type 'a expr =
  | Evar of name (** Variables *)
  | Enum of int (** Numbers *)
  | Econstr of (int * int) (** Constructor tag arity *)
  | Eap of ('a expr * 'a expr) (** Applications *)
  | Elet of (isRec * ('a * 'a expr) list * 'a expr) (** Let(rec) expressions *)
  | Ecase of ('a expr * ('a alter) list) (** Case expressions *)
  | Elam of ('a list * 'a expr) (** Lambda abstractions *)
and 'a alter = int * 'a list * 'a expr

type coreExpr = name expr
type coreAlter = name alter

let bindersOf = fst |> List.map
let rhsOf = snd |> List.map

let isAtomicExpr = function
  | Evar _ | Enum _ -> true
  | _ -> false

type 'a scDefn = name * 'a list * 'a expr
type 'a program = ('a scDefn) list

type coreScDefn = name scDefn
type coreProgram = name program

let preludeDefs : coreProgram =
  [
    ("I", ["x"], Evar "x");
    ("K", ["x"; "y"], Evar "x");
    ("K1", ["x"; "y"], Evar "y");
    ("S", ["f"; "g"; "x"], Eap (Eap (Evar "f", Evar "x"), Eap (Evar "g", Evar "x")));
    ("compose", ["f"; "g"; "x"], Eap (Evar "f", Eap (Evar "g", Evar "x")));
    ("twice", ["f"], Eap (Eap (Evar "compose", Evar "f"), Evar "f")) 
  ]
