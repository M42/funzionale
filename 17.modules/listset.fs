module Sett

type Sett<'a> = S of 'a list
let empty = S []
let isEmpty (S s) = List.isEmpty s
let contains x (S s) = List.exists (fun y -> x=y) s
let add x (S s) = if contains x (S s) then (S s) else (S (x::s))
let union (S s) (S p) = List.fold (fun xs x -> add x xs) (S s) p
let ofList l = S l
let toList (S l) = l
