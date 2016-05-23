module Liste

let rec split = function
    | [] -> ([],[])
    | [a] -> ([a],[])
    | a::b::xs ->
        let (fsx,ssx) = split xs
        (a :: fsx, b :: ssx)
        ;;
    ;;
