module Files

open System.IO;;

let rec allFiles(dir:string) = seq{
    let files = Directory.GetFiles(dir)
    let dirs  = Directory.GetDirectories(dir)
    yield! files
    for d in dirs do
      yield! allFiles(d)
    }
