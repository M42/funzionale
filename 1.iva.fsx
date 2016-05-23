module Iva

let iva (c:char) =
    match c with
    | 'a' | 'A' -> 4
    | 'b' | 'B' -> 10
    | _ -> 22
    ;;

let piuiva (categoria:char, costobase:float) =
    costobase * float (100 + iva(categoria)) / 100.0
    ;;

let menoiva (categoria:char, costobase:float) =
    costobase * 100.0 / float (100 + iva(categoria))
    ;;
