module Relations

type Rel<'a,'b when 'a : comparison and 'b : comparison> = Set<'a * 'b>;;

let domain rel = Set.map (fun (a,b) -> a) rel;;
let range rel = Set.map (fun (a,b) -> b) rel;;

let idRel set = Set.map (fun x -> (x,x));;
let simmRel rel = Set.union rel (Set.map (fun (a,b) -> (b,a)) rel)
let apply rel a = Set.filter (fun (x,y) -> x=a) rel |> range

let cartesian p q =
    Set.map (fun x -> Set.map (fun y -> (x,y)) q) p
    |> Set.toSeq
    |> Set.unionMany
;;

let composition rel1 rel2 =
    cartesian rel1 rel2
    |> Set.filter (fun ((x,y),(w,z)) -> y=w)
    |> Set.map (fun ((x,y),(w,z)) -> (x,z))
;;
