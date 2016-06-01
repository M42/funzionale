module BinTree

type 'a binTree =
    | Null
    | Node of 'a * 'a binTree * 'a binTree
    ;;


let rec insert(x:'a, tree:'a binTree) =
    match tree with
    | Null                     -> Node(x,Null,Null)
    | Node(y,lt,rt) when x < y -> Node(y,insert(x,lt),rt)
    | Node(y,lt,rt) when x > y -> Node(y,lt,insert(x,rt))
    | Node(y,lt,rt)            -> Node(y,insert(x,lt),rt)
    ;;
    
