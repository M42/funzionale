#r "FsCheck.dll"
open FsCheck

type algebra<'a> =
    | Const of 'a
    | Var of string
    | Sum of 'a algebra * 'a algebra
    | Prod of 'a algebra * 'a algebra

type algebraInt = int algebra
type algebraBool = bool algebra

let rec eval(expr : int algebra) =
    match expr with
        | Const n -> n
        | Sum(a,b) -> eval(a)+eval(b)
        | Prod(a,b) -> eval(a)*eval(b)
        | Var(s) -> 0
    ;;



/// OPTIMIZATION
let propagate(func : 'a algebra -> 'a algebra, expr : 'a algebra) =
    match expr with
        | Sum(a,b) -> Sum(func a,func b)
        | Prod(a,b) -> Prod(func a,func b)
        | other -> other


let rec optimize(expr : int algebra) =
    propagate(optimize,expr)
    |> zeroneutral
    |> oneneutral

and zeroneutral(expr : int algebra) =
    match expr with
        | Sum(a,Const 0) -> a
        | Sum(Const 0,a) -> a
        | other -> other

and oneneutral(expr : int algebra) =
    match expr with
        | Prod(a,Const 1) -> a
        | Prod(Const 1,a) -> a
        | other -> other

and zeronull(expr : int algebra) =
    match expr with
        | Prod(a,Const 0) -> a
        | Prod(Const 0,a) -> a
        | other -> other
;;

// CHECKING
let ``optimization works`` expr =
    eval(expr) = eval(optimize expr)

Check.Quick(``optimization works``)
