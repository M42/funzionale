module Figura


type figura =
    | Rettangolo of float*float
    | Quadrato   of float
    | Triangolo  of float*float
    ;;

let area(fig:figura) =
    match fig with
    | Rettangolo(a,b) when a>0. && b>0. -> Some (a*b)
    | Quadrato(l)     when l>0.         -> Some (l*l)
    | Triangolo(b,h)  when b>0. && h>0. -> Some ((b*h)/2.)
    | _                                 -> None 
    ;;
    

