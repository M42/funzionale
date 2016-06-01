module Sett

(**
 An interface for a set
 *)
type Sett<'a when 'a:equality>

val empty : Sett<'a>
val isEmpty : Sett<'a> -> bool
val contains : 'a -> Sett<'a> -> bool
val add : 'a -> Sett<'a> -> Sett<'a>
val union : Sett<'a> -> Sett<'a> -> Sett<'a>
val ofList : 'a list -> Sett<'a>
val toList : Sett<'a> -> 'a list
