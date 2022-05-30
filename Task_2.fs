module Task2
open Stream
open System

//g functions: maps key uniform in range [1..p]
let g (x : uint64) (coefficients: bigint array) (prime_p : bigint) : bigint  =
    let q = coefficients.Length
    let mutable y = coefficients[q-1]
    for i = 2 to q do
        y <- y * bigint x + coefficients[q-i]
        y <- (y&&&prime_p) + (y>>>89)
    if y >= prime_p then
        y <- y-prime_p
    y 

//k=m in the assignment text. Input t is the exponent to decide k  opgavebeskrivelsen.
// Assert 2^t <= 2^24  
let computation_h_and_s (x : uint64) (g : uint64 -> bigint array -> bigint -> bigint) (t : int) (coefficients: bigint array) (prime_p : bigint) =
    let k = 1<<<t
    let gx : bigint = g x coefficients prime_p
    let hx : int = int (gx &&& bigint (k-1))
    let bx : int = int (gx >>> 89-1)
    let sx : int = 1 - 2*bx
    (hx, sx)  

//Make algorithms for count sketch 

//assert k is a power of 2 
let BCS_Init (k: int) : int array = 
    //let k = int(ceil(8.0/e**2)) //how we originally choose k 
    let mutable C : int array = Array.zeroCreate k
    C

let BCS_pocess (key_pair : (uint64*int)) (c_array : int array) (g : uint64 -> bigint array -> bigint -> bigint) (coefficients: bigint array) (prime_p: bigint) : unit =
    let mutable x, d = key_pair
    let t = Math.Log(float(c_array.Length), 2.0) 
    let tuple_h_s = computation_h_and_s x g (int t) coefficients prime_p
    c_array[fst tuple_h_s] <- c_array[fst tuple_h_s] + (snd tuple_h_s) * d

let BCS_2nd_moment (c_array : int array) : int = 
    let mutable counter = 0 
    for i in 0..c_array.Length-1 do
        counter <- counter + (c_array[i]*c_array[i])
    counter 

 

