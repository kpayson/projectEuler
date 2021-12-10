
let reverseNum (n:Math.BigInt) =
    let rec reverseNumAux (a:Math.BigInt) (b:Math.BigInt) =
        if a = 0I then b
        else 
            let d,r = Math.BigInt.DivRem(a, 10I)
            reverseNumAux d (10I*b + r)
    reverseNumAux n 0I

let isPalendromic (n:Math.BigInt)  =
    n = (reverseNum n)
    
let revAndAdd n =
    n + (reverseNum n)
    

let isLychrel (maxIter:int) (n:int) =
    let rec isLychrelAux n i =
        if (i = maxIter) then
            true
        else
            let m = reverseNum n
            if (n = m)
                then false
            else
               isLychrelAux (n+m) (i+1)
    isLychrelAux (revAndAdd (Math.bigint n)) 1
  
    
let numSat (s:seq<'a>) (p:'a->bool) =
    s |> Seq.fold (fun acc x -> if p x then acc + 1 else acc) 0
    
let numLychrelLtN n = 
    numSat (seq{1..n}) (isLychrel 50);;
                
        
            