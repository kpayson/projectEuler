type math = System.Math

let rec fact n = 
    if n = 1 then n
    else n * (fact (n - 1))

let divRem n m =
    (n/m,n % m)
    
let digits = [|0;1;2;3;4;5;6;7;8;9|]
let mil = 1000000

let d1, r1 = divRem mil (fact 9)
let d2, r2 = divRem r1 (fact 8)
let d3, r3 = divRem r2 (fact 7)
let d4, r4 = divRem r3 (fact 6)
let d5, r5 = divRem r4 (fact 5)
let d6, r6 = divRem r5 (fact 4)
let d7, r7 = divRem r6 (fact 3)
let d8, r8 = divRem r7 (fact 2)

//26625122
// 2    0 1 _ 3 4 5 6 7 8 9
// 7    0 1 _ 3 4 5 6 _ 8 9
// 8    0 1 _ 3 4 5 6 _ _ 9
// 3    0 1 _ _ 4 5 6 _ _ 9
// 9    0 1 _ _ 4 5 6 _ _ _
// 1    0 _ _ _ 4 5 6 _ _ _
// 5    0 _ _ _ 4 _ 6 _ _ _
// 6
// 0
// 4
//2783915604  //not right answer but why?
