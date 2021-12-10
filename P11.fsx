
open System.IO

let file = @"C:\Users\Ken\Documents\Visual Studio 10\Projects\ProjectEuler\ProjectEuler\p11.txt";

let readMatrix file =
    let lines = File.ReadAllLines(file)
    let matrix: int[,] = Array2D.zeroCreate 20 20
    for i=0 to lines.Length-1 do
        let nums = lines.[i].Split(' ')
        for j=0 to nums.Length-1 do
            matrix.[i,j] <- System.Int32.Parse(nums.[j])
    matrix
         
let localMaxProd (matrix:int[,]) (n:int) (m:int) =
    let prods = 
        seq {
            yield matrix.[0+n,0+m] * matrix.[1+n,1+m] * matrix.[2+n,2+m] * matrix.[3+n,3+m]
            yield matrix.[0+n,3+m] * matrix.[1+n,2+m] * matrix.[2+n,1+m] * matrix.[3+n,0+m]
            for i=0 to 3 do
                yield matrix.[i+n,0+m] * matrix.[i+n,1+m] * matrix.[i+n,2+m] * matrix.[i+n,3+m]
                yield matrix.[0+n,i+m] * matrix.[1+n,i+m] * matrix.[2+n,i+m] * matrix.[3+n,i+m]   
        }
    prods |> Seq.max
    

let globalMaxProd (matrix:int[,]) = 
    let prods =
        seq {
            for i=0 to 15 do
                for j=0 to 15 do
                    yield localMaxProd matrix i j }  
    prods |> Seq.max                      