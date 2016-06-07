module TypeCheck

// Type System
// There are two types, integers and lists of integers
type tp = INT | LSINT

// An expression has the form:
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
        | K _ ->
            Some INT
        | Plus(a,b) ->
            if tpck(a) = Some INT && tpck(b) = Some INT
            then Some INT else None
        | Cons(h,t) ->
            if tpck(h) = Some INT && tpck(t) = Some LSINT
            then Some LSINT else None
        | Head(l) ->
            if tpck(l) = Some LSINT
            then Some INT else None
        | Tail(l) ->
            if tpck(l) = Some LSINT
            then Some LSINT else None
        | Nil ->
            Some LSINT

// Showing the type of an expression
let showtype (expr:exp) =
    match tpck expr with
        | Some t -> printf "%A has type %A\n" exp t
        | None -> printf "%A is not typable" exp
        

// Handling exceptions
// First draft: in order to create a good typing system, we should
// write more detailed exceptions
exception TypeError of (exp * tp)

let rec tpckf (expr:exp) =
    match expr with
        | K _ ->
            INT
        | Plus(a,b) ->
            if tpckf(a) = INT && tpckf(b) = INT
            then INT else raise(TypeError(expr, INT))
        | Cons(h,t) ->
            if tpckf(h) = INT && tpckf(t) = LSINT
            then LSINT else raise(TypeError(expr, INT))
        | Head(l) ->
            if tpckf(l) = LSINT
            then INT else raise(TypeError(expr, LSINT))
        | Tail(l) ->
            if tpckf(l) = LSINT
            then LSINT else raise(TypeError(expr, LSINT))
        | Nil ->
            LSINT    

let main (expr:exp) =
    try
        let t = tpckf(expr)
        printf "%A has type %A\n" expr t
    with
    | TypeError(expe,tpe) ->
        printf "Error typing %A with %A\n" expe tpe
;;
