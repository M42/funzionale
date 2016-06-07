module Higher

let concat xs ys =
    List.foldBack (fun x xs -> x::xs) xs ys
;;

let filter p xs =
    List.foldBack (fun x xs -> if p(x) then x::xs else xs) xs []
;;

