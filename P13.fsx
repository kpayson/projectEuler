open System.IO

let numFile = @"C:\Users\Ken\Documents\Visual Studio 10\Projects\ProjectEuler\ProjectEuler\p13Nums.txt"

let sumNums numFile =
    let numStrings = File.ReadAllLines(numFile)
    numStrings |> Seq.map (fun x-> Math.BigInt.Parse(x)) |> Seq.sum

let firstNDigits num n =
    num.ToString().Substring(0,n)
    

firstNDigits (sumNums numFile) 10