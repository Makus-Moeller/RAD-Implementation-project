// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let multiply_shift_hashing (x:uint64) (a:uint64) (l: int) : uint64 = 
    (a*x) >>> (64-l)

let hashvalue = multiply_shift_hashing 34921UL 64244UL 35 

printfn "%A" hashvalue