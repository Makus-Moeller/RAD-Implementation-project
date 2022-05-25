// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let add_five (x : int) : int = 
    x+5

let c = add_five 4
printfn "%A" c