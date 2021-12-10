module scratch

    let nums = [1..100]
    let sum = nums |> List.sum
    printf "The sum=%d" sum

    let median (l:int list) =
        let sortedList = l |> List.sort
        let n = l |> List.length
        let m = List.nth sortedList (n/2)
        m
    
    let rand = new System.Random(System.DateTime.Now.Millisecond)
    let mixedNums = [1..100] |> List.sortBy (fun x -> (rand.Next() % 2) = 0) 

    let shuffle (arr: int []) = 
        let rand = new System.Random(System.DateTime.Now.Millisecond)
        let n = arr |> Array.length
        for i in [0..n-1] do
            let j = rand.Next(n)
            let temp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- temp
        arr
        


            
            
        



        
        
