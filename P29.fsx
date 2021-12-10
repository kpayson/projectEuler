let powerGrid: Math.BigInt [,]  = Array2D.zeroCreate 101 101
powerGrid.[2,2] <- 4I

let uniqueNums = new System.Collections.Generic.Dictionary<string,bool>()

for i= 1 to 100 do
    powerGrid.[i,1] <- Math.BigInt.Parse(i.ToString())
    powerGrid.[1,i] <- Math.BigInt.One
    
for i = 2 to 100 do
    for j = 2 to 100 do
        let t = Math.BigInt.Parse(i.ToString())
        powerGrid.[i,j] <- t * powerGrid.[i,(j-1)]
        let n = powerGrid.[i,j].ToString()
        if (uniqueNums.ContainsKey(n) = false) then
            uniqueNums.Add(n,true)

uniqueNums.Keys.Count