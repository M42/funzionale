module Stack

type Stack<'a>
val empty : 'a Stack
val push : 'a -> 'a Stack -> 'a Stack
val pop : 'a Stack -> 'a Stack
val top : 'a Stack -> 'a option
val size : 'a Stack -> int
