type 'a heap = int * int * (int * 'a) list
type addr = int

let hInitial = (0, 1, [])
let hAlloc (size, next, cts) n = ((size + 1, next + 1, (next, n)::cts), next)
let hUpdate (size, next, cts) a n = (size, next, (a, n)::List.remove_assoc a cts)
let hFree (size, next, cts) a = (size - 1, next, List.remove_assoc a cts)
let hLookup (size, next, cts) a = List.assoc a cts
let hAddresses (size, next, cts) = List.map fst cts
let hSize (size, next, cts) = size
let hNull = 0
let hIsNull a = a = 0
