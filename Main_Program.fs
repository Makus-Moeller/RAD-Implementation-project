// For more information see https://aka.ms/fsharp-console-apps

exception InnerError of string

open System
let prime_p : bigint = (bigint 2<<<89)-bigint 1
let rand_numb_bigint: bigint list = [
    961026785849338145764820I
    320523630964915694060580I
] 


//Assignment 1 a
let multiply_shift_hashing (x:uint64) (a:uint64) (l: int) : uint64 = 
    (a*x) >>> (64-l)

let hashvalue = multiply_shift_hashing 34921UL 64244UL 35 
//printfn "%A" hashvalue

//Assignment 1 b

let  multiply_mod_prime (a: bigint) (b: bigint) (x:uint64) (l: int32) : bigint = 
    if a >= prime_p || b >= prime_p then raise(InnerError("To LARGE A or B")) 
    else
        let z : bigint = (a*bigint x + b)  //Big multiplication
        printfn "z value: %A" z
        let y = (z&&&prime_p) + (z>>>89)
        let power_l = bigint 1<<<l
        printfn "y value: %A" y
        if y >= prime_p then  
            (y-prime_p) % power_l //Jeg har ændret denne shift
        else 
            y % power_l


[<EntryPoint>]
let main argv =       
    let a : bigint = 349214000000073I
    let b : bigint = 560000000867I
    let hashvalue2 = multiply_mod_prime a b 403232UL 11
    0



