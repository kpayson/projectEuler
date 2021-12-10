#light

type math = System.Math

let squares = [| for i=0 to 100 do yield i * i|]

let isPrime p =
    let maxK = p |> float |> math.Sqrt |> math.Floor |> int
    let rec aux n =
        let k1 = 6 * n - 1
        let k2 = 6 * n + 1
        if (k2 < maxK) then
            if ( p % k1 = 0 || p % k2 = 0) then false
            else aux (n + 1)
        else
            true 
            
    if ( p < 2 ) then false
    elif ( p = 2 || p = 3) then true
    elif ( p % 2 = 0 || p % 3 = 0) then false
    else aux 1;
    
let allPrimes n = 
    let siv: int [] = Array.zeroCreate(n)
    
    let maxK = n |> float |> math.Sqrt |> math.Floor |> int
    
    let rec nextP k = 
        if siv.[k] = 0 then 
            k
        else
            nextP (k+1)
            
    let rec filter k x =
        if (x < n) then
            siv.[x] <- 1
            filter k (x + k)
    
    let rec aux k =
        filter k (2*k)
        let p = nextP (k+1)
        if (p < maxK) then
            aux p
  
    aux 2
    siv
 
let primes n =
    let siv = allPrimes (n+1)
    [2..n] |> List.filter (fun x -> siv.[x] = 0)   
    

let quad a b n =
    n*n + a*n + b
    

let rec seqLen a b k=
    let x = quad a b k
    if (isPrime x) then
        seqLen a b (k + 1)
    else
        k

let third x =
    let a, b, c = x
    c
    
let seqLengths (pList: int list) =
    [
        for p in pList do
            for q in pList do
                yield (p, q, (seqLen p q 0))
                yield (-p, q, (seqLen (-p) q 0))
                yield (p, -q, (seqLen p (-q) 0))
                done
            done
    ] 
    //|> List.sortBy (fun x -> -(third x))
    |> List.maxBy (fun x -> (third x))

    
   