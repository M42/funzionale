module TypeCheck

// Type System
// There are two types, integers and lists of integers
type tp = INT | LSINT

// A expression has the form:
//  K - integer constant
//  Plus - sum of integers
//  Cons - list, given head and tail
//  Head - head of a list
//  Tail - tail of a list
//  Nil  - empty list
type exp =
    | K    of int
    | Plus of exp*exp
    | Cons of exp*exp
    | Head of exp
    | Tail of exp
    | Nil

// Typechecks recursively
let rec tpck (expr:exp) =
    match expr with
        | K ->
            Some INT
        | Plus(a,b) ->
            if tpck(a) = Some INT and tpck(b) = Some INT
            then Some INT else None
        | Cons(h,t) ->
            if tpck(h) = Some INT and tpck(t) = Some LSINT
            then Some LSINT else None
        | Head(l) ->
            if tpck(l) = Some LSINT
            then Some INT else None
        | Tail(l) ->
            if tpck(l) = Some LSINT
            then Some LSINT else None
        | Nil ->
            Some LSINT
