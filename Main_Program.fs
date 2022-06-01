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
let l : int = 5

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
printfn "Time mulshift hashing: %f" watch_mulshift.Elapsed.TotalMilliseconds

let watch_modPrime = System.Diagnostics.Stopwatch.StartNew()
for i in stream do     
    sum_mod_prime <- sum_mod_prime + multiply_mod_prime (fst i) a_bigint b_bigint l prime_p
watch_modPrime.Stop()
printfn "Time modprime hashing: %f" watch_modPrime.Elapsed.TotalMilliseconds

//printfn "sum_mutilply_shift: %A" sum_multiply_shift
//printfn "sum_mod_prime: %A" sum_mod_prime

//Exercise 2
//Watch code in Task1.fs

//Exercise 3

//Initialize hashtables
let mutable hashtable_mulshift = create_hashtable l
let mutable hashtable_modPrime = create_hashtable l

//get Squared sum Multiply_shift
let mutable kvadratsum_mulShift  = 0
let mutable kvadratsum_modPrime = 0

let new_l = 2

//let mutable copy_of_stream = []

for i in new_l..22 do
    if i%2=0 then
        let stream_new = createStream 6000000 i
        let mutable copy_new_stream = []
        let mutable hashtable_mulshift_new = create_hashtable i
        let mutable hashtable_modPrime_new = create_hashtable i
        for pair in stream_new do
            copy_new_stream <- pair :: copy_new_stream
        
        
        let x_new, y = List.unzip(copy_new_stream)
        let distinct_stream_new = x_new |> Seq.distinct |> List.ofSeq
        let mutable kvadratsum_mulShift_new : int = 0
        let mutable kvadratsum_modPrime_new : int = 0
        
        let watch_mulshift = System.Diagnostics.Stopwatch.StartNew()
        for pair in copy_new_stream do
        //Insert values in hashtable with Multiply_shift
            increment_value pair Multiply_shift hashtable_mulshift_new a_bigint b_bigint prime_p a_mulshift
    
        for pair in distinct_stream_new do
            let d_value_shift = get_value pair Multiply_shift hashtable_mulshift_new a_bigint b_bigint prime_p a_mulshift
            kvadratsum_mulShift_new <- kvadratsum_mulShift_new + (d_value_shift * d_value_shift)
        let mulshift_time :float = watch_mulshift.Elapsed.TotalMilliseconds
        watch_mulshift.Stop()


        let watch_mod_prime = System.Diagnostics.Stopwatch.StartNew()
        for pair in copy_new_stream do
            increment_value pair Multiply_mod_prime hashtable_modPrime_new a_bigint b_bigint prime_p a_mulshift
        for pair in distinct_stream_new do    
            let d_value_modPrime = get_value pair Multiply_mod_prime hashtable_modPrime_new a_bigint b_bigint prime_p a_mulshift
            kvadratsum_modPrime_new <- kvadratsum_modPrime_new + (d_value_modPrime * d_value_modPrime)
        watch_mod_prime.Stop()
        let watch_mod_prime_time : float = watch_mod_prime.Elapsed.TotalMilliseconds
        printfn "L: %i   mulshift_time: %f    modprime: %f   factor %f" i mulshift_time watch_mod_prime_time (float mulshift_time/float watch_mod_prime_time)

(*

//Process every keypair, every stream is differenr and must be 
for pair in stream do
    //Insert values in hashtable with Multiply_shift
    increment_value pair Multiply_shift hashtable_mulshift a_bigint b_bigint prime_p a_mulshift
    increment_value pair Multiply_mod_prime hashtable_modPrime a_bigint b_bigint prime_p a_mulshift
    copy_of_stream <- pair :: copy_of_stream


let x, y = List.unzip(copy_of_stream)
let distinct_stream = x |> Seq.distinct |> List.ofSeq


for pair in distinct_stream do
    let d_value_shift = get_value pair Multiply_shift hashtable_mulshift a_bigint b_bigint prime_p a_mulshift
    kvadratsum_mulShift <- kvadratsum_mulShift + (d_value_shift * d_value_shift)
    
    //Squared sum with 
    let d_value_modPrime = get_value pair Multiply_mod_prime hashtable_modPrime a_bigint b_bigint prime_p a_mulshift
    kvadratsum_modPrime <- kvadratsum_modPrime + (d_value_modPrime * d_value_modPrime)



//Print squared sum 
printfn "Squared sum multiply shift: %A" kvadratsum_mulShift
printfn "Squared sum mod prime: %A" kvadratsum_modPrime 



//create alot of random numbers
let mutable all_tries = []
let Median_method () : unit= 
    for i in 0..99 do
        let C_array_new = BCS_Init 128
        for k in copy_of_stream do
            BCS_pocess k C_array_new g (random_array[i*4..i*4+3]) prime_p
        let temp_val = BCS_2nd_moment C_array_new
        all_tries <-  temp_val :: all_tries
        ()

Median_method ()
printfn("%A") all_tries
*)