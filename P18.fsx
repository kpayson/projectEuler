open System.IO

let file = @"C:\Users\Ken\Documents\Visual Studio 10\Projects\ProjectEuler\ProjectEuler\P67.txt";

let readMatrix file =
    let lines = File.ReadAllLines(file)
    let matrix: int[,] = Array2D.zeroCreate lines.Length lines.Length 
    for i=0 to lines.Length-1 do
        let nums = lines.[i].Split(' ')
        for j=0 to nums.Length-1 do
            matrix.[i,j] <- System.Int32.Parse(nums.[j])
    matrix
    
let m = readMatrix file

let maxPath( m:int [,]) =
    let baseLen  =m.GetLength(0) - 1 
    let max x y =
        if ( x > y) then x
        else y
        
    for i=1 to baseLen do
        let b = baseLen - i
        for j=0 to b do
            m.[b,j] <- m.[b,j] + (max m.[b+1,j] m.[b+1,j+1])

    m

let m2 = maxPath m

m2.[0,0]

//        while(1) { 
//          $y += $x++; $r=0;
//          for $z (1 .. sqrt($y)) {
//            $y % $z == 0 and $r+=2; 
//          }
//          if ($r>499) {print "$x $y\n";die}
//        }
