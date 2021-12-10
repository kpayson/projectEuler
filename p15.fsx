
//Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to the bottom right corner.
//How many routes are there through a 20×20 grid?

let gridPath n m =
    let t = new System.Collections.Generic.Dictionary<int64*int64,int64>()
    let max (x:int64) (y:int64) = System.Math.Max (x,y)
    let min (x:int64) (y:int64) = System.Math.Min (x,y)
    
    let rec T (a:int64) (b:int64) =
        let c = min a b
        let d = max a b
        if t.ContainsKey(c,d) then t.[c,d]
        else
            if c = 0L then 1L
            elif d = 0L then 1L
            elif (c=d) then 2L*T (c-1L) c
            else (T (c-1L) d) + (T c (d-1L))

    T n m
    
let rec sumf (f:int->int) (i:int) (n:int) (tot:int) =
    if (i < n) then
        tot + (f i) + (sumf f (i+1) n tot)
    else
        tot
let pathCount n=         
    let dtable n =
        let dt: int64[,] = (n) |> Array2D.zeroCreate(n)
        dt
    
    let grid = dtable (n+1)
    let nums = [1..n]
    for i in nums do
        grid.SetValue(1L,0,i)
        grid.SetValue(1L,i,0)
    
    for i in [1..n] do
        for j in [i..n] do
            let x = (grid.GetValue((i-1),j) :?> int64) + (grid.GetValue(i,(j-1)) :?> int64)  
            grid.SetValue(x,i,j)
            grid.SetValue(x,j,i)
                    
    grid //.GetValue(n,n) :?> int64
    