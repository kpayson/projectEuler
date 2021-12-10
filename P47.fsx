type math = System.Math
let N = 1000000
let siv: int [] = Array.zeroCreate(N + 1)
let maxK n = 
    n |> float |> math.Sqrt |> math.Floor |> int


let buildSiv n =
    let K = maxK n
    
    let rec nextPrime i =
        if i > N then -1
        elif (siv.[i] = 0) then i
        else nextPrime (i + 1)
    
    let rec filter i p =
        if (i < N) then
            siv.[i] <- siv.[i] + 1
            filter (i + p) p
    
    let rec filterAll n =
        if (n < K) then
            filter n n
            let p = nextPrime n
            filterAll p
    
    filterAll 2
        

let rec findFourInARow i  =
    let rec aux n =
        if (n > 3) then
            n
        elif ( siv.[i + n] > 3) then
            aux (n + 1)
        else 
            n
    let n = aux 0
    if n > 3 then i
    else findFourInARow (i + n + 1)
        

buildSiv N
let answer = findFourInARow 2   


