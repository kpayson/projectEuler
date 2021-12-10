#light



// Learn more about F# at http://fsharp.net

//problem #2
//	Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million.
//1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987; 1597; 2584;
//   4181; 6765; 10946; 17711; 28657; 46368; 75025; 121393; 196418; 317811;
//   514229; 832040; 1346269; 2178309; 3524578; 5702887

let rec fib n = 
    if (n=1) then 1
    elif (n=2) then 1
    else fib (n-1) + fib (n-2)
    
let nums = [1..33]
let fib_nums = nums |> List.map fib 
let tot = fib_nums|> List.filter (fun x->x % 2=0) |> List.sum


//problem #3
// prime factors of 13195 are 5, 7, 13 and 29.
//What is the largest prime factor of the number 600851475143 ?

let N = 600851475143.0

let sqrt n =
    System.Math.Sqrt n;
    
let init_guess n =
    let g = n |> System.Math.Sqrt |> System.Math.Floor
    if ((g % 2.0)=0.0) then g+1.0
    else g

let max (x:float) (y:float) = 
    System.Math.Max( x, y)    

let rec maxDivisor n d =
    if (d < 3.0) then n
    elif (n % d) = 0.0 then
        d
    else
        maxDivisor n (d-2.0)
        
let rec findMaxFactor (n:float)  = 
    let guess = init_guess n
    let d = maxDivisor n guess
    if (n = d) then n
    else
        max (findMaxFactor d) (findMaxFactor (n/d))

        

let rec isPalendrome (s:string) i j =
    if (j<i) then true
    elif (s.Chars(i) = s.Chars(j)) then 
        isPalendrome s (i+1) (j-1)     
    else
        false
 
let isPalendromeNum (n:int) =
    let s = n.ToString()
    isPalendrome s 0 (s.Length-1)
    
let palendromeNums =
    let nums = [100..999]
    seq { for x in nums do
            for y in nums do
                if (isPalendromeNum (x*y)) then
                    yield x*y }
                    
let maxNum = palendromeNums |> Seq.max




//problem #6
// What is the difference between the sum of the squares and the square of the sums?

//sum(n) = n(n+1)/2
//sum(n)^2 = n*n*(n+1)*(n+1)/4
//sum(n^2) = n(n+1)(2n+1)/6 
//3*n*n*(n+1)*(n+1) / 12 - 2*n*(n+1)*(2n+1) /12 = n*(n+1)*[3*n*(n+1) - 2*(2n+1)] /12
//=n*(n+1)[3*n^2+3*n-4*n-2] /12= n*(n+1)(3n^2-n-2)/12

let squareSumDiff n =
    n*(n+1)*(3*n*n-n-2)/12
    
let squareSumDiff2 nums =
    let sumOfSquare numList = numList |> List.map (fun x-> x*x) |> List.sum    
    let squareOfSum numList = numList |> List.sum |> (fun x->x*x)     
    let diff = (squareOfSum nums) - (sumOfSquare nums)
    diff
 
 //problem #7 	
//Find the 10001st prime.  
let findPrime (n:int) =
      
    let calcSivSize (m:int) =
        let nf = System.Convert.ToDouble(m)
        System.Convert.ToInt32(2.0 * nf * ceil(log(nf)))
    
    let getNewSiv size =
        let (siv : int []) = Array.zeroCreate(size)
        siv
    
    let sivSize = n  |> calcSivSize 
    let siv = sivSize |> getNewSiv
    
    let maxFactor (m:int) = 
        let nf = System.Convert.ToDouble(m)
        System.Convert.ToInt32(floor(sqrt(nf)))
   
    let rec sift (arr: int []) (start:int) (k:int) =
        //printfn "k=%d start=%d sivSize=%d\n" k start sivSize 
        if (start < sivSize) then
            arr.[start] <- 1
            sift arr (start+k) k
        else
            arr
             
    let rec getNextP (arr: int []) (k:int) =
        if (k<sivSize) then
            if (arr.[k] = 0) then k
            else getNextP arr (k+1)
        else
            sivSize
            
    let K = maxFactor sivSize
    
    let rec doSift (arr: int []) (k:int) =      
        if (k > K) then
            arr
        else           
            let a = sift arr (2*k) k
            doSift a (getNextP a (k+1))
    
    let rec primes (arr: int []) (i:int) =
        seq { if (i<arr.Length) then
                if(arr.[i] =0) then
                    yield i
                    yield! primes arr (i+1)
                else 
                    yield! primes arr (i+1)}

    let sifted = doSift siv 2
    let primeN  = (primes sifted 2) |> Seq.nth(n-1)

    
    primeN
    //Seq.to_list    