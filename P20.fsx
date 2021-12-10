let rec factorial (n:Math.BigInt) = 
    if (n.IsOne) then Math.BigInt.One
    else  (n * factorial (n-1I))
    

let rec sumDigits (n:Math.BigInt) =
    let k,r = Math.BigInt.DivRem(n,10I)
    if ( k = 0I ) then r
    else r + sumDigits k



