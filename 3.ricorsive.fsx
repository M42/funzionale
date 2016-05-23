module Ricorsive

let rec mcd(n:int, m:int) =
    match (n,m) with
    | (n,0) -> n
    | (n,m) -> mcd(m,n%m)
    ;;

let simplify((a,b):int*int) =
    let mcda = mcd(a,b)
    (a/mcda, b/mcda)
    ;;
