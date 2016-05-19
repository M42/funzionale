// Match syntax
let not (x:bool) =
    match x with
        | true -> false;
        | false -> true
    ;;


// Function syntax
let not = function
    | true -> false
    | false -> true
    ;;
