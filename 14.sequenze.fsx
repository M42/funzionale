module Sequence

let naturals = Seq.initInfinite (fun x -> x);;

let rec map f sq = seq{
    let e   = Seq.item 0 sq
    let sqe = Seq.skip 1 sq
    yield  (f e)
    yield! (map f sqe)
    };;

let rec naturals1 = seq{
    yield 0
    yield! map (fun x -> x+1) naturals1
    }
