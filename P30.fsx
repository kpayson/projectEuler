let rec digits n = 
    let d = n / 10
    let r = n % 10
    [
        yield r;
        if d > 0 then
            yield! digits d]
            
let fifthpowersum l =
    l |> List.map (fun x -> 
        let y = x*x 
        x*y*y) |> List.sum

let eqsumfithpowerdigits n =
    n = (n |> digits |> fifthpowersum)
    

let foo = [1000000..2000000] |> List.fold (fun acc x -> if (eqsumfithpowerdigits x) then (acc + x) else acc) 0