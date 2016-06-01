module Testing
#r "FsCheck"
open FsCheck

// Creates the function
let fizzbuzz = function
    | d when d%15=0 -> "fizzbuzz"
    | d when d%3 =0 -> "fizz"
    | d when d%5 =0 -> "buzz"
    | d             -> ""
    ;;





// Checking simple tests
// It passes all tests, but how many tests are have been passed only by the
// use of the condition.
let ``fizz if 3-multiple`` d =
    (d>0 && d%3=0 && d%15<>0) ==> (fizzbuzz d = "fizz")
;;
    
// A specific generator will do a more precise job
type FizzBuzzTest =
    //static member positiveIntegers =
    //    Arb.Default.Int32()
    //    |> Arb.mapFilter abs (fun n -> n>0)

    static member fizzIntegers =
        Arb.Default.Int32()
        |> Arb.filter (fun n -> n>0 && n%3=0 && n%15<>0)

Arb.registerByType(typeof<FizzBuzzTest>)
