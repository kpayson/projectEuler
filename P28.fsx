

let diagSum k = 
   1 + ([1..k] |> List.map (fun n -> 2*n+1) |> List.map (fun n-> 4*n*n - 6*n + 6) |> List.sum)

