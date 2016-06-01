module Sett

type 'a Sett =
    | Node of 'a * 'a Sett * 'a Sett
    | Leaf
;;

// Comparison of hashes
let (.<) x y = (hash x) < (hash y);;


// Simple tree functions
let empty = Leaf;;

let isEmpty = function
    | Leaf -> true
    | _    -> false
;;

let rec contains x = function
    | Node(y,lt,rt) when x.<y -> contains x lt
    | Node(y,lt,rt) when y.<x -> contains x rt
    | Node(y,lt,rt) when x=y  -> true
    | Node(y,lt,rt)           -> (contains x lt) || (contains x rt)
    | Leaf                    -> false
;;

let rec toList s =
    match s with
        | Node(y,lt,rt) -> (toList lt) @ [y] @ (toList rt)
        | Leaf          -> []
;;

let rec add x s =
    match s with
        | Node(y,lt,rt) when x.<y -> Node(y,add x lt,rt)
        | Node(y,lt,rt) when y.<x -> Node(y,lt,add x rt)
        | Node(x,lt,rt)           -> Node(x,lt,rt)
        | Leaf                    -> Node(x,Leaf,Leaf)
;;

let union a b = List.fold (fun s x -> add x s) a (toList b);;

let ofList l = List.fold (fun s x -> add x s) Leaf l;;


