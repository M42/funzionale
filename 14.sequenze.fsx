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


// ESERCIZI DI SEQUENZE 2
// 1. Funzione sift
let rec sift a sq = seq{
    let e = Seq.item 0 sq
    let sq1 = Seq.skip 1 sq
    if not (e%a = 0) then
      yield e
    yield! (sift a sq1)
    }
;;

let nat = Seq.initInfinite (fun x -> x);;
let sq1 = sift 2 nat
let sq2 = sift 3 nat

// 2. Sieve
let rec sieve sq = seq{
    let x0 = Seq.item 0 sq
    yield x0
    yield! (sieve (sift x0 sq))
    }
    
let primes = sieve (Seq.skip 2 nat);;

// 3. Versione con meccanismo di caching
let siftC a sq = Seq.cache (sift a sq)
let sieveC sq = seq{
    let x0 = Seq.item 0 sq
    yield x0
    yield! (sieve (siftC x0 sq))
    }
let primesC = Seq.cache(sieveC (Seq.skip 2 nat));;
