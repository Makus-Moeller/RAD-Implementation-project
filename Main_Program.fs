// For more information see https://aka.ms/fsharp-console-apps

exception InnerError of string
open System.Linq
open System
type Hash_Table = array<list<uint64*int>>
let prime_p : bigint = (bigint 2<<<89)-bigint 1
let rand_numb_bigint: bigint list = [
    961026785849338145764820I
    320523630964915694060580I
] 

let rand_64: uint64 list = [
    13146228726924412240UL
    3325534366338438573UL
]





//Assignment 1 b
let  multiply_mod_prime (a: bigint) (b: bigint) (x:uint64) (l: int32) : uint64 = 
    if a >= prime_p || b >= prime_p then raise(InnerError("To LARGE A or B")) 
    else
        let z : bigint = (a*bigint x + b)  //Big multiplication
        let y = (z&&&prime_p) + (z>>>89)
        let power_l = bigint 1<<<l
        if y >= prime_p then  
            uint64((y-prime_p) % power_l) //Jeg har ændret denne shift
        else 
            uint64(y % power_l)

//Stream for testing
let createStream (n:int) (l:int) : seq<uint64*int> =
    seq {
        // We generate a random uint64 number .
        let rnd = System.Random ()
        let mutable a = 0UL
        let b : byte [] = Array.zeroCreate 8
        rnd.NextBytes (b)
        let mutable x : uint64 = 0UL
        for i = 0 to 7 do
        a <- (a<<<8) + uint64(b.[ i ])
        // We demand that our random number has 30 zeros on the least
        // significant bits and then a one.
        a <- (a|||((1UL<<<31) - 1UL))^^^((1UL<<<30)- 1UL)
        let mutable x = 0UL
        for i = 1 to (n/3) do
            x <- x + a
            yield (x&&&(((1UL<<<l) - 1UL)<<<30),1)
        for i = 1 to ((n + 1)/3) do
            x <- x + a
            yield (x&&&(((1UL<<<l)-1UL)<<<30),-1)
        for i = 1 to (n + 2) /3 do
            x <- x + a
            yield (x&&&(((1UL<<<l)-1UL)<<<30),1)
}


//Assignment 1 a
let multiply_shift_hashing (x:uint64) (a:uint64) (l: int) : int32 = 
    int32 ((a*x) >>> (64-l))

//hashtablecreator
let create_hashtable (length: int) : Hash_Table = 
    printfn "leftshift %i" (1<<<length)
    Array.create (1<<<length) List.Empty

let get(x:uint64) (table : Hash_Table) : int= 
    let x_hash = multiply_shift_hashing x rand_64.[0] (4)
    printfn("%i,  %i") table.Length x_hash
    try 
        List.find (fun y -> fst y = x) table.[x_hash] |> snd
    with 
        | :? System.Collections.Generic.KeyNotFoundException -> 0

[<EntryPoint>]
let main argv =       
    let a : bigint = 349214000000073I
    let b : bigint = 560000000867I
    

    let hashvalue2 = multiply_mod_prime a b 403232UL 11
    let seq = createStream 1000 20
    
    let h_table = create_hashtable 4
    h_table.[0] <- (100000UL, -1) :: h_table.[0]
    printfn "%A" h_table.[0]
    
    
    
    printfn "%i" (get 100000UL h_table)
    
    let stopWatch1 = System.Diagnostics.Stopwatch.StartNew()
    let mutable sum:uint64 = 0UL
    for i in seq do
        let temp = multiply_mod_prime rand_numb_bigint.[0] rand_numb_bigint.[1] (fst i) 32
        sum <- sum + temp
    stopWatch1.Stop()
    printfn "multiply_mod_prime: %f MS" stopWatch1.Elapsed.TotalMilliseconds
    let stopWatch2 = System.Diagnostics.Stopwatch.StartNew()
    let mutable sum_shift = 0
    for i in seq do
        let temp = multiply_shift_hashing (fst i) rand_64.[0] 32
        sum_shift <-sum_shift + temp
    stopWatch2.Stop()
    printfn "hashshift: %f MS" stopWatch2.Elapsed.TotalMilliseconds
    0



