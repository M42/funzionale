module Somme

let rec sumSeq sq = seq{
    let e0 = Seq.item 0 sq
    let e1 = Seq.item 1 sq
    let sq2 = seq{
      yield e1+e0
      yield! Seq.skip 2 sq
    }
    
    yield e0
    yield! sumSeq sq2
    }
