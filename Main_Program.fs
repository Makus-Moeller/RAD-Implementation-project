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
let l : int = 12

//Make stream, assert  n > 2^l
let stream = createStream 6000 l

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

let new_l = 2

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
        
        //Initialize squaresums
        let mutable kvadratsum_mulShift_new : int = 0
        let mutable kvadratsum_modPrime_new : int = 0
        
        //First for mulshift
        let watch_mulshift = System.Diagnostics.Stopwatch.StartNew()
        for pair in copy_new_stream do
        //Insert values in hashtable with Multiply_shift
            increment_value pair Multiply_shift hashtable_mulshift_new a_bigint b_bigint prime_p a_mulshift
    
        for pair in distinct_stream_new do
            let d_value_shift = get_value pair Multiply_shift hashtable_mulshift_new a_bigint b_bigint prime_p a_mulshift
            kvadratsum_mulShift_new <- kvadratsum_mulShift_new + (d_value_shift * d_value_shift)
        let mulshift_time :float = watch_mulshift.Elapsed.TotalMilliseconds
        watch_mulshift.Stop()

        //Then for modprime
        let watch_mod_prime = System.Diagnostics.Stopwatch.StartNew()
        for pair in copy_new_stream do
            increment_value pair Multiply_mod_prime hashtable_modPrime_new a_bigint b_bigint prime_p a_mulshift
        for pair in distinct_stream_new do    
            let d_value_modPrime = get_value pair Multiply_mod_prime hashtable_modPrime_new a_bigint b_bigint prime_p a_mulshift
            kvadratsum_modPrime_new <- kvadratsum_modPrime_new + (d_value_modPrime * d_value_modPrime)
        watch_mod_prime.Stop()
        let watch_mod_prime_time : float = watch_mod_prime.Elapsed.TotalMilliseconds
        
        printfn "L: %i   mulshift_time: %f    modprime: %f   factor %f" i mulshift_time watch_mod_prime_time (float mulshift_time/float watch_mod_prime_time)

//Task 4-6 watch Task_2.fs

//Task 7 and 8
//Take mod prime of random bigints array 
random_array <- Array.map (fun x -> (x&&&prime_p) + (x>>>89)) random_array

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
    

