module Task1
open System 
exception InnerError of string

//Creates uneven number
let unevenize (numb : uint64) : uint64 = 
    uint64(numb) ||| uint64(1)

//Multiply shift 
let multiply_shift_hashing (x:uint64) (a:uint64) (l: int) : int = 
    int ((a*x) >>> (64-l))

//Multiply mod prime
let  multiply_mod_prime (x:uint64) (a: bigint) (b: bigint) (l: int32) (p: bigint) : int = 
    if a >= p || b >= p then raise(InnerError("To LARGE A or B")) 
    else
        let x : bigint = bigint x
        let z : bigint = (a*x + b)  //Big int multiplication
        let y = (z&&&p) + (z>>>89)
        let power_l = bigint 1<<<l
        if y >= p then  
            int ((y-p) % power_l) 
        else 
            int (y % power_l)

//Function for creating hashtable with size 2^l
let create_hashtable (l: int)  : array<list<uint64*int>> = 
    Array.create (1<<<l) List.Empty

//define types which can be passed as arguments
type Hashfunction = Multiply_mod_prime | Multiply_shift

//Set key
let set_value (key_pair : (uint64*int)) (hashfunction : Hashfunction)  (hashtable: array<list<uint64*int>>) (a_mod: bigint) (b: bigint) (l: int) (p: bigint) (a_shift: uint64) : unit =
    let mutable x, d = key_pair
    let mutable hashvalue = 0

    match hashfunction with 
        Multiply_mod_prime -> hashvalue <- multiply_mod_prime x a_mod b l p 
        | Multiply_shift -> hashvalue <- multiply_shift_hashing x a_shift l 
    
    let mutable new_lst = List.map (fun a -> if (fst a)=x then (x,d) else a) hashtable[hashvalue]
    if hashtable[hashvalue] = new_lst then
        new_lst <- (x,d) :: new_lst
    hashtable[hashvalue] <- new_lst 

//Increment value
let increment_value (key_pair : (uint64*int)) (hashfunction : Hashfunction) (hashtable: array<list<uint64*int>>) (a_mod: bigint) (b: bigint) (l: int) (p: bigint) (a_shift: uint64) : unit =
    let mutable x, d = key_pair
    let mutable hashvalue = 0
    
    match hashfunction with 
        Multiply_mod_prime -> hashvalue <- multiply_mod_prime x a_mod b l p 
        | Multiply_shift -> hashvalue <- multiply_shift_hashing x a_shift l 

    let mutable new_lst = List.map (fun a -> if (fst a)=x then (x,d+snd(a)) else a) hashtable[hashvalue]
    if hashtable[hashvalue] = new_lst then
        new_lst <- (x,d) :: new_lst
    hashtable[hashvalue] <- new_lst 

//Get value 
let get_value (x : uint64) (hashfunction : Hashfunction) (hashtable: array<list<uint64*int>>) (a_mod: bigint) (b: bigint) (l: int) (p: bigint) (a_shift: uint64) : int =
    let mutable hashvalue = 0 
    let mutable ret_val = 0

    match hashfunction with 
        Multiply_mod_prime -> hashvalue <- multiply_mod_prime x a_mod b l p
        | Multiply_shift -> hashvalue <- multiply_shift_hashing x a_shift l 

    for (a,y) in hashtable[hashvalue] do
        if a=x then  
            ret_val <- y
    ret_val