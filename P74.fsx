let rec fact n =
    if n = 0 then 1
    else n * fact (n - 1)
    
let factArray = [|0..9|] |> Array.map(fun n -> fact n)

let seqLen: int [] = Array.zeroCreate(1000000)

let rec sumFactDigits n = 
    let d,r = n/10, n%10
    let f = factArray.[r]
    if (d = 0) then f
    else f + sumFactDigits d
    
let findSeqLen n =
    let s = new System.Collections.Generic.Dictionary<int,bool>()
    let rec aux n = 
        if (s.ContainsKey(n)) then 0
        else
            s.Add(n,true)
            1 + (aux (sumFactDigits n))
    aux n

let seqLenCount k n =
    let mutable cnt = 0
    for i=1 to n do
        if (findSeqLen i) = k then
            cnt <- cnt + 1
    cnt;;
    
        
    
        
