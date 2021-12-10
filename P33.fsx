
type math = System.Math

let rec gcd a b =
    if b = 0 then a
    else gcd b (a % b)

let lowestTerms a b =
    let k = gcd a b
    (a/k,b/k)
    
let fracVal a b c d =
    let m = (10 * a + b)
    let n = (10 * c + d)
    lowestTerms m n
    
let isCurious a b c d = 
    let fv = fracVal a b c d
    (b <> 0 || d <> 0) && ((fv = (lowestTerms a c)) || (fv = (lowestTerms b d)))
            
//let curiousFractions =
//    [for a = 1 to 9 do
//        for b = 0 to 9 do
//            for c = (a+1) to 9 do
//                for d = 0 to 9 do
//                    let fv = fracVal a b c d
//                    if isCurious a b c d then
//                        yield (a,b,c,d)]

let curiousFractions =
    [for x = 10 to 98 do
        for y = (x+1) to 99 do
            let a = x / 10
            let b = x % 10
            let c = y / 10
            let d = y % 10
            if ((b = c) && ( d * x) = (a * y)) || ((a = d) && (c * x) = (b * y)) then
                yield (x,y)
    
    ]

let curiousFractionsProd (fracts:(int * int) list) =
    fracts |> List.fold (fun acc x ->
        let x_a,x_b = x
        let acc_a,acc_b = acc
        let num = x_a * acc_a
        let den = x_b * acc_b
        (num,den)) (1,1)

let cfp = curiousFractions |> curiousFractionsProd 

let a,b = cfp

let _,d = lowestTerms a b


    
            
    
