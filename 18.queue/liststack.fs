module Stack

type Stack<'a> = Stack of 'a list

let empty = Stack []

let push x (Stack l) = Stack (x::l)

let pop = function
    | Stack (x::xs) -> Stack xs
    | Stack []      -> Stack []
    ;;

let top = function
    | Stack (x::xs) -> Some x
    | Stack []      -> None
    ;;

let size (Stack l) = List.length l
