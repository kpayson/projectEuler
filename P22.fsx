open System.IO

let file= @"C:\Users\Ken\Documents\Visual Studio 10\Projects\ProjectEuler\ProjectEuler\P22.txt"
let testfile = @"C:\Users\Ken\Documents\Visual Studio 10\Projects\ProjectEuler\ProjectEuler\test.txt"
let names = File.ReadAllText(file).Replace("\"","").Split(',') |> Array.sort 

let wordVal (word:string) =
    let A = System.Convert.ToInt32('A')
    let c = word.ToCharArray() |> Array.fold (fun acc x -> System.Convert.ToInt32(x) - A + 1 + acc) 0
    System.Convert.ToInt64 c

let dotProd (x:int64 []) (y:int64 []) =
    let mutable tot = 0L
    for i=0 to (x.Length-1) do
        tot <- tot + (x.[i] * y.[i])
    tot
    
let rec totNameScore (names:string []) = 
    let nameVals = names |> Array.map (fun x -> wordVal x)
    let posArray = Array.init (names.Length) (fun n -> System.Convert.ToInt64(n + 1))
    dotProd nameVals posArray
               
totNameScore names