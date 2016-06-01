#r "listset.dll"
//#r "hashlist.dll"
open Sett


let emptydefinition =
    isEmpty empty

let containsdefinition s x =
    contains x (add x s)

let fromtolist l =
    toList (ofList l) = l
