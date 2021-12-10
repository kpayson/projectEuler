//37 36 35 34 33 32 31
//38 17 16 15 14 13 30
//39 18  5  4  3 12 29
//40 19  6  1  2 11 28
//41 20  7  8  9 10 27
//42 21 22 23 24 25 26
//43 44 45 46 47 48 49

let cornerNumbers n = 
    let a = n*n;
    let b = a - n + 1;
    let c = b - n + 1;
    let d = c - n + 1;
    [b;c;d]


let maxK n =
    n |> float |> System.Math.Sqrt |> System.Math.Floor |> int 

let isPrime n = 
    let k = maxK n
    let rec aux x d =
        if (d > k) then true
        elif (n % d) = 0 then false
        else aux x (d+2)
    if (n % 2) = 0 then false
    else aux n 3
    
let rec primePercent n numPrimesFound numCorners =
    let corners = cornerNumbers n
    let newPrimesCount = corners |> List.fold (fun acc x -> if isPrime(x) then acc + 1 else acc) 0
    let x = numPrimesFound + newPrimesCount
    let y = numCorners + 4
    if ( float(x) / float(y) ) < 0.1 then
        n
    else
        primePercent (n+2) x y
    