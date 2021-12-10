let rec gcd a b =
    if b = 0 then
        a
    else
        gcd b (a % b)


let fractionCount dmax = 
    let mutable cnt = 0;
    for n = 1 to (dmax) do
        for d = (n+1) to dmax do
            if ( (gcd n d) = 1) then
                cnt <- cnt + 1
    cnt;;
