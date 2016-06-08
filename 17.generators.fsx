module Generators

#r "FsCheck"
open FsCheck


// Definition of a tree
type Tree = Leaf | Branch of int * Tree * Tree;;

// Unsafe tree generator. Size is not bounded!
let rec unsafeTree() =
    Gen.oneof [ Gen.constant Leaf
                Gen.map3 (fun a x y -> Branch(a,x,y)) (Gen.constant 3) (unsafeTree()) (unsafeTree())
              ]
    ;;


// Safe tree generator
// It can be called as:
//    trees |> Gen.sample 0 9
let rec trees s =
    match s with
        | 0 ->
            Gen.constant Leaf
        | n when n>0 ->
            let subtree = trees (n/2)
            Gen.oneof [ Gen.constant Leaf
                        Gen.map3 (fun a x y -> Branch(a,x,y)) (Gen.choose(0,n)) subtree subtree
                        ]
        | _ -> invalidArg "s" "Only positive arguments"

let tree =
    Gen.sized trees


type MyGenerators= 
    static member Tree() = Arb.fromGen tree

Arb.registerByType(typeof<MyGenerators>)






let rec reverse(t:Tree) =
    match t with
        | Leaf -> Leaf
        | Branch(a,x,y) -> Branch(a,reverse(y),reverse(x))
;;

let revrevtree(t:Tree) = 
    reverse(reverse(t)) = t
Check.Quick(revrevtree);;
