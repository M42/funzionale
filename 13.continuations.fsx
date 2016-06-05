module Continuation
#r "FsCheck.dll"
open FsCheck;;

type 'a tree =
    | Node of 'a * 'a tree * 'a tree
    | Leaf
;;


// Naive version
let rec count(tree : 'a tree) =
    match tree with
        | Leaf -> 0
        | Node(_,lt,rt) -> (1 + count lt + count rt)
;;

// Accumulator version
let rec count2(tree : 'a tree, acc : int) =
    match tree with
        | Leaf -> acc
        | Node(_,lt,rt) -> count2(lt, count2(rt, acc + 1))
;;

// Continuation version
let rec count3(tree : 'a tree, k : int -> int) =
    match tree with
        | Leaf -> k 0
        | Node(_,lt,rt) -> count3(lt, fun p -> count3(rt,fun q -> k(1+p+q)))
;;



// CHECKING
let count1a2(tree : 'a tree) =
    count(tree) = count2(tree,0)
;;
Check.Quick(count1a2);;

let count1a3(tree : 'a tree) =
    count(tree) = count3(tree,id)
;;
Check.Quick(count1a3);;
