
let dict = new System.Collections.Generic.Dictionary<int64,int>()
dict.Add(1L,1)

let lenArray : int [] = Array.zeroCreate(1000000)
lenArray.[1] <- 1;

let collatz1 n =
    if n=1L then 1L
    elif n%2L=0L then n/2L
    else 3L*n+1L
    
let addIfNew k tot =    
    if (dict.ContainsKey(k) = false) then
        dict.Add(k,tot)
        
let collatzLen (n:int64) =
    let stack = new System.Collections.Generic.Stack<int64>()  
    let mutable total:int = 0
    let mutable k:int64 = n
    let mutable v:int64 = 0L
    stack.Push(n)

    while (stack.Count > 0)  do
        if k > 1L then
            if dict.ContainsKey(k) then
                total <- (stack.Count + (dict.Item k) - 1)
                addIfNew n total
                stack.Clear()
            else
                k <- (collatz1 k)
                stack.Push(k)
                
        else
            v <- (stack.Pop())
            total <- total + 1
            addIfNew v total
    done
    
    total
      

//3 10 5 16 8 4 2 1
                  
let solveAll (n:int) (m:int)=
    for i in [n..m] do
        let l = System.Convert.ToInt64 (i)
        (collatzLen l) |> ignore
        
let maxLen  = dict.Values |> Seq.max

dict.Keys |> Seq.maxBy (fun x -> dict.Item x)
