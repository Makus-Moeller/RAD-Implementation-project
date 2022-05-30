module Main

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
//l Should always be less than 32
let l : int = 31

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

//Insert values in hashtable with Multiply_shift 
for pair in stream do
    increment_value pair Multiply_shift hashtable_mulshift a_bigint b_bigint prime_p a_mulshift
    increment_value pair Multiply_mod_prime hashtable_modPrime a_bigint b_bigint prime_p a_mulshift

//get Squared sum Multiply_shift
let mutable kvadratsum_modPrime = 0
let mutable kvadratsum_mulShift  = 0
for pair in stream do
    let d_value = get_value (fst pair) Multiply_shift hashtable_mulshift a_bigint b_bigint prime_p a_mulshift
    kvadratsum_mulShift <- kvadratsum_mulShift + (d_value * d_value)
    let d_value = get_value (fst pair) Multiply_mod_prime hashtable_modPrime a_bigint b_bigint prime_p a_mulshift
    kvadratsum_modPrime <- kvadratsum_modPrime + (d_value * d_value)
printfn("fst  %i") kvadratsum_mulShift


//Insert values in hashtable with mod_prime 
    

//get Squared sum Multiply_shift


printfn("snd %i") kvadratsum_modPrime

//Task 2

//Initialize coefficients
let a0 : bigint = 117853593111058875688543295I
let a1 : bigint = 33859604520461393499784185I
let a2 : bigint = 458413945876887069982739372180473I
let a3 : bigint = 18801263508795393760483242194398798939129I
let coeff_lst = [|a0;a1;a2;a3|]
 
//Initialize counter array
let C_array = BCS_Init 128

//Process every keypair
for keypair in stream do 
    BCS_pocess keypair C_array g coeff_lst prime_p

//Get second moment
printfn "second moment: %A" (BCS_2nd_moment C_array)

//create alot of random numbers  
let obj_random = new Random()

//let red, green, blue = objrandom.Next(256), objrandom.Next(256), objrandom.Next(256)
