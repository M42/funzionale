module Superiore

#r "FsCheck"
open FsCheck;;

// 2. Definire map
let rec map f ls =
    match ls with
        | [] -> []
        | (x::xs) -> (f x)::(map f xs)
;;


let ``map preserves identity`` (xs : 'a list) =
    map id xs = xs
Check.Quick ``map preserves identity``

let ``map preserves composition`` (xs : 'a list, f : 'a -> 'b, g : 'b -> 'c) =
    map (f >> g) xs = ((map f) >> (map g)) xs
Check.Quick ``map preserves composition``


// 3. Definire filter
let rec filter p ls =
    match ls with
        | [] -> []
        | (x::xs) -> if p(x) then x::(filter p xs) else (filter p xs)
;;

let ``filter bounds length`` p ls =
    List.length(filter p ls) <= List.length(ls)
Check.Quick ``filter bounds length``


// 5. Divisori
let divisori n =
    filter (fun x -> n%x = 0) [1..n]
;;
