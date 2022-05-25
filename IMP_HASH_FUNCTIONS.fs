// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
exception InnerError of string

open System

let prime_p:bigint = (bigint 2<<<89)-bigint 1 
let random_numbers_64 = [
    uint64 -5189612905158709895L; 
]


let random_numbers_89 = [
]
//Creates uneven number
let unevenize (numb : uint64) : uint64 = 
    uint64(numb) ||| uint64(1)
    
// Assignment 1 a
let hashing_function (a : uint64) (l : int32) (x : uint64) : uint64 =
    (a*x)>>>(64-l)


//Assignment 1 b
let  multiply_mod_prime (a: bigint) (b: bigint) (x:uint64) (l: int32) : bigint = 
    if a = prime_p || a = prime_p then raise(InnerError("To LARGE A or B"))
    else
        let x:bigint = (a*bigint x + b)
        let y = (x&&&prime_p) + (x>>>89)
        if y>=prime_p then (y-prime_p) % bigint 2<<<l
        else y % bigint 2<<<l


[<EntryPoint>]
let main argv =
    let x:uint64 = 1000UL
    let hash_x = hashing_function random_numbers_64.[0] 40 x
    uint64 hash_x |>printf "Hash = %i" 
    0 // return an integer exit code