#r "liststack.dll"
open Stack
open System

type operator = Add | Prod | Minus
type token = Op of operator | Const of int

let operation (stack:Stack<int>, op: (int -> int ->int)) =
    let factor1 = top(stack)
    let stack2  = pop(stack)
    let factor2 = top(stack2)
    let stack3  = pop(stack2)

    match factor1 with
        | None -> None
        | Some x ->
        match factor2 with
            | None -> None
            | Some y -> Some (push (op x y) stack3)
    ;;
    
let evalRpn (tokens : token list) =
    let evalToken (stack:Stack<int>) (tkn:token) =
        let stackop =
            match tkn with
                | Op Add -> operation(stack,(fun x y -> x + y))
                | Op Prod -> operation(stack,(fun x y -> x * y))
                | Op Minus -> operation(stack,(fun x y -> x - y))
                | Const n -> Some (push n stack)
        match stackop with
            | None -> (exit 1)
            | Some st -> st
    in List.fold evalToken empty tokens
    ;;


let parse (line : String) =
    let elements = line.Split(' ') |> List.ofArray
    let parseelement = function
        | "+" -> Op Add
        | "-" -> Op Minus
        | "*" -> Op Prod
        | n   -> Const (System.Int32.Parse(n))
        
    List.map parseelement elements
    ;;


[<EntryPoint>]
let main (argv : String []) =
    let input = Console.ReadLine()
    let parsed = parse input
    let stack = evalRpn parsed
    
    match top stack with
        | None -> printfn "Error!"
        | Some d -> printfn "Output %d" d
    
    0
