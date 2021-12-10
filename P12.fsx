#light

type math = System.Math

let floorRoot (n:int) =
    n |> float |> math.Sqrt |> math.Floor |> int
    
let divisorCount n =
    let maxK = floorRoot n
    let t = [2..(maxK)] |> List.fold (fun acc k -> if n % k = 0 then acc + 2 else acc) 2
    t
    
let rec firstTriangleNumWithKDivisors k n t =
    if divisorCount n >= k then
        n
    else firstTriangleNumWithKDivisors k (n + (t+1)) (t+1) 
        
let firstTriangleNumWith500Divisors = firstTriangleNumWithKDivisors 500 1 1