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
let l : int = 22

//Make stream, assert  n > 2^l
let stream = createStream 6000000 l

//Counters for sum of hashvalues
let mutable sum_multiply_shift : int = 0
let mutable sum_mod_prime : int = 0

//Take time of stream processing

let watch_mulshift = System.Diagnostics.Stopwatch.StartNew()
for i in stream do  
    sum_multiply_shift <- sum_multiply_shift + multiply_shift_hashing (fst i) a_mulshift l   
watch_mulshift.Stop()
printfn "Time mulshift hashing: %f ms." watch_mulshift.Elapsed.TotalMilliseconds
printfn "sum_mutilply_shift: %A" sum_multiply_shift


let watch_modPrime = System.Diagnostics.Stopwatch.StartNew()
for i in stream do     
    sum_mod_prime <- sum_mod_prime + multiply_mod_prime (fst i) a_bigint b_bigint l prime_p
watch_modPrime.Stop()
printfn "Time modprime hashing: %f ms." watch_modPrime.Elapsed.TotalMilliseconds
printfn "sum_mod_prime: %A" sum_mod_prime

//Exercise 2
//Watch code in Task1.fs

//Exercise 3

//Initialize hashtables
let mutable hashtable_mulshift = create_hashtable l
let mutable hashtable_modPrime = create_hashtable l

 
//Process every keypair, every stream is differenr and must be
let mutable stream_copy = [] 

for pair in stream do
    //Insert values in hashtables 
    increment_value pair Multiply_shift hashtable_mulshift a_bigint b_bigint l prime_p a_mulshift
    increment_value pair Multiply_mod_prime hashtable_modPrime a_bigint b_bigint l prime_p a_mulshift
    stream_copy <- pair :: stream_copy

//Reverse stream such that it is in correct order
stream_copy <- List.rev stream_copy 

//get Squared sum Multiply_shift
let mutable kvadratsum_mulShift  = 0
let mutable kvadratsum_modPrime = 0

//Get unique keys
let key_lst, d_lst = List.unzip(stream_copy) 
let unique_keys = key_lst |> Seq.distinct |> List.ofSeq


for key in unique_keys do
    //Squared sum with mulshift 
    let d_value_shift = get_value key Multiply_shift hashtable_mulshift a_bigint b_bigint l prime_p a_mulshift
    kvadratsum_mulShift <- kvadratsum_mulShift + (d_value_shift * d_value_shift)
    
    (*
    //Squared sum with 
    let d_value_modPrime = get_value key Multiply_mod_prime hashtable_modPrime a_bigint b_bigint l prime_p a_mulshift
    kvadratsum_modPrime <- kvadratsum_modPrime + (d_value_modPrime * d_value_modPrime)
    *)
//Print squared sum 
printfn "Squared sum multiply shift: %A" kvadratsum_mulShift
//printfn "Squared sum mod prime: %A" kvadratsum_modPrime 


//Initialize coefficients
let a0 : bigint = 117853593111058875688543295I
let a1 : bigint = 33859604520461393499784185I
let a2 : bigint = 458413945876887069982739372180473I
let a3 : bigint = 18801263508795393760483242194398798939129I
let coeff_lst = [|a0;a1;a2;a3|]

//Define k value. Precision of count sketch
let k = 128

//Initialize counter array
let C_array_test = BCS_Init k

for pair in stream_copy do
    //Count sketch of stream
    BCS_Process pair C_array_test g coeff_lst prime_p

//Get second moment
printfn "Second moment Count sketch: %A" (BCS_2nd_moment C_array_test)
*)

//remember to map through every element and take mod p
random_array <- Array.map (fun x -> (x&&&prime_p) + (x>>>89)) random_array


let mutable result_lst = [] 

for i in 0..99 do
    let C_array = BCS_Init k
    for keypair in stream_copy do
        BCS_Process keypair C_array g random_array[i*4..i*4+3] prime_p
    result_lst <- (BCS_2nd_moment C_array) :: result_lst

printfn "Count sketch list: %A" result_lst
printfn "Length: %A" result_lst.Length


let m_lst : int list = [128; 256; 512]


for m in m_lst do
    let watch_mulshift = System.Diagnostics.Stopwatch.StartNew()
    let mutable result_lst = [] 
    for i in 0..99 do
        let C_array = BCS_Init m
        for keypair in stream_copy do
            BCS_Process keypair C_array g random_array[i*4..i*4+3] prime_p
        result_lst <- (BCS_2nd_moment C_array) :: result_lst
        
    watch_mulshift.Stop()
    printfn "Time Countsketch: %f ms, with m: %A." watch_mulshift.Elapsed.TotalMilliseconds m
    printfn "Count sketch list with m: %A \n %A" m result_lst
    

