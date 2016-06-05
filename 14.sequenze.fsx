module Sequence

// ESERCIZI DI SEQUENZE 1
// 1. Definire map
let rec map f sq = seq{
    let e   = Seq.item 0 sq
    let sqe = Seq.skip 1 sq
    yield  (f e)
    yield! (map f sqe)
    };;

// 2. Aplicare alla sequenza dei naturalli per generare "squares"
let naturals = Seq.initInfinite (fun x -> x);;
let squares = map (fun x -> x*x) naturals;;

// 3. Considerare il caso di sequenza vuota
let rec map2 f sq = seq{
    let empty = Seq.isEmpty sq 
    
    if (not empty) then
      let e = Seq.item 0 sq
      let sqe = Seq.skip 1 sq
      yield (f e)
      yield! (map2 f sqe)
    }
;;

// 4. Definire filter
let rec filter pred sq = seq{
    let e = Seq.item 0 sq
    let sq1 = Seq.skip 1 sq
    if pred e then
      yield e
    yield! (filter pred sq1)
    }
;;


// 5. Multipli di 3
let multipli = filter (fun x -> x%3 = 0) naturals;;

// 6. Fibonacci
let rec fibfrom a b = seq {
    yield a
    yield! (fibfrom b (a+b))
    }

let fib n = Seq.item n (fibfrom 0UL 1UL)

