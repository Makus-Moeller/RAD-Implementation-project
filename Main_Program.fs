// For more information see https://aka.ms/fsharp-console-apps

exception InnerError of string

open System


//Assignment 1 a
let multiply_shift_hashing (x:uint64) (a:uint64) (l: int) : uint64 = 
    (a*x) >>> (64-l)

let hashvalue = multiply_shift_hashing 34921UL 64244UL 35 
//printfn "%A" hashvalue

//Assignment 1 b
let prime_p : bigint = (bigint 2<<<89)-bigint 1

let  multiply_mod_prime (a: bigint) (b: bigint) (x:uint64) (l: int32) : bigint = 
    if a >= prime_p || b >= prime_p then raise(InnerError("To LARGE A or B")) 
    else
        let z : bigint = (a*bigint x + b)  //Big multiplication
        printfn "z value: %A" z
        let y = (z&&&prime_p) + (z>>>89)
        printfn "y value: %A" y
        let power_l = bigint 1<<<l
        if y >= prime_p then  
            (y-prime_p) % power_l //Jeg har ændret denne shift
        else 
            y % power_l
            
let a : bigint = 34921432462I
let b : bigint = 56783642446I


let hashvalue2 = multiply_mod_prime a b 403232UL 11 
printfn "%A" hashvalue2 

