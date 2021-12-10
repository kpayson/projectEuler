type math = System.Math

let rec fact n = 
    if n = 0 then 1
    else n * (fact (n - 1))
    
let factVals = [|0..9|] |> Array.map (fun x -> fact x)

let sumfactdigits n =
    let rec aux n acc =
        if n = 0 then acc
        else
            let d = n/10
            let r = n % 10
            aux d (acc + factVals.[r])
    aux n 0

let a = [ for i = 3 to 10000000 do
            if (i = sumfactdigits i) then
                yield i]