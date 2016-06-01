module Exceptions

// Using system exceptions
// You only have to use :? with .NET exceptions
let safehead list =
    try
        Some (List.head list)
    with
        | :? System.ArgumentException -> None
;;


// Creating new exceptions
exception NegativeFactorial of int;;

let rec factorial(n:int) =
    match n with
        | 0 -> 1
        | n when n>0 -> factorial(n-1) * n
        | _ -> raise (NegativeFactorial n)
;;

let handlingfactorial(n:int) =
    try
        string(factorial n)
    with
        | NegativeFactorial m -> sprintf "Negative factorial! (%d)" m
;;
