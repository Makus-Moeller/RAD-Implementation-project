module Main

open Rand_array
open Stream
open Task1
open Task2 
open System

//Exercise 1
//Define  
let prime_p : bigint = (bigint 1<<<89)-bigint 1

//Declare random parameters             
let a_bigint : bigint = 65582665042094216432365251I
let b_bigint : bigint = 105704395269750626696406447I
let a_mulshift : uint64 = 4196704446715454703UL
//l Should always be less than 64
let l : int = 13

//Make stream, assert  n > 2^l
let stream = createStream 3000 l

//Counters for sum of hashvalues
let mutable sum_multiply_shift : int = 0
let mutable sum_mod_prime : int = 0

//Take time of stream processing
let watch_mulshift = System.Diagnostics.Stopwatch.StartNew()
for i in stream do  
    sum_multiply_shift <- sum_multiply_shift + multiply_shift_hashing (fst i) a_mulshift l   
watch_mulshift.Stop()
printfn "Time mulshift hashing: %f" watch_mulshift.Elapsed.TotalMilliseconds

let watch_modPrime = System.Diagnostics.Stopwatch.StartNew()
for i in stream do     
    sum_mod_prime <- sum_mod_prime + multiply_mod_prime (fst i) a_bigint b_bigint l prime_p
watch_modPrime.Stop()
printfn "Time modprime hashing: %f" watch_modPrime.Elapsed.TotalMilliseconds

printfn "sum_mutilply_shift: %A" sum_multiply_shift
printfn "sum_mod_prime: %A" sum_mod_prime

//Exercise 2
//Watch code in Task1.fs

//Exercise 3

//Initialize hashtables
let mutable hashtable_mulshift = create_hashtable l
let mutable hashtable_modPrime = create_hashtable l

//get Squared sum Multiply_shift
let mutable kvadratsum_mulShift  = 0
let mutable kvadratsum_modPrime = 0


//Initialize coefficients
let a0 : bigint = 117853593111058875688543295I
let a1 : bigint = 33859604520461393499784185I
let a2 : bigint = 458413945876887069982739372180473I
let a3 : bigint = 18801263508795393760483242194398798939129I
let coeff_lst = [|a0;a1;a2;a3|]
 
//Initialize counter array
let C_array = BCS_Init 128

//Process every keypair, every stream is differenr and must be 
for pair in stream do
    //Insert values in hashtable with Multiply_shift
    increment_value pair Multiply_shift hashtable_mulshift a_bigint b_bigint prime_p a_mulshift
    increment_value pair Multiply_mod_prime hashtable_modPrime a_bigint b_bigint prime_p a_mulshift

    //Squared sum with mulshift 
    let d_value_shift = get_value (fst pair) Multiply_shift hashtable_mulshift a_bigint b_bigint prime_p a_mulshift
    kvadratsum_mulShift <- kvadratsum_mulShift + (d_value_shift * d_value_shift)
    
    //Squared sum with 
    let d_value_modPrime = get_value (fst pair) Multiply_mod_prime hashtable_modPrime a_bigint b_bigint prime_p a_mulshift
    kvadratsum_modPrime <- kvadratsum_modPrime + (d_value_modPrime * d_value_modPrime)

    //Count sketch of stream
    BCS_pocess pair C_array g coeff_lst prime_p

//Print squared sum 
printfn "Squared sum multiply shift: %A" kvadratsum_mulShift
printfn "Squared sum mod prime: %A" kvadratsum_modPrime 

//Get second moment
printfn "Second moment Count sketch: %A" (BCS_2nd_moment C_array)

//create alot of random numbers  
printfn "%A" random_array[0]
