#light

type math = System.Math

let N = 28123
let floorRoot (n:int) =
    n |> float |> math.Sqrt |> math.Floor |> int
  


let sumDivisors (n:int) =
    let maxK = floorRoot n
    let t = [2..(maxK)] |> List.fold (fun acc k -> if (n % k = 0) then (acc + k + (n/k) ) else acc) 1   
    if (maxK * maxK = n) then 
        (t - maxK)
    else t



let isAbundant n =
    (sumDivisors n) > n 
    
let getAllAbundant n =
    set [
        for i=1 to n do
            if isAbundant i then
                yield i     
    ]

let expressableAsAbundantSum (n:int) (s:Set<int>) = 
    s |> Set.exists (fun x -> s.Contains (n-x))

let getAllExpressableAsSum n =
    let s = getAllAbundant n
    set [
        for i=1 to n do
            if (expressableAsAbundantSum i s) then
                yield i
    ]

let nonExpressable n =
    let nums = set [1..n]
    let expressableNums = getAllExpressableAsSum n
    let nonExpressable = Set.Subtract(nums, expressableNums)
    nonExpressable
    
let nonExpressableSum (s:Set<int>) =
    let sum = s |> Set.fold  (fun acc x -> acc + x) 0  
    sum
    
let allAbundant = getAllAbundant N
let allExpressableAsSum = getAllExpressableAsSum N
