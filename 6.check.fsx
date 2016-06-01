module Checking

let rec rev ls =
    match ls with
    | [] -> []
    | x::xs -> (rev xs) @ [x]

let prop_revrev xs =
    rev(rev(xs)) = xs
    
