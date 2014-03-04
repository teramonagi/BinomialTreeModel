
open BinomialTreeModel
[<EntryPoint>]
let main argv = 
    //Strike:50$, Underlying price:51$, Maturity:1 year Risk-free:1%, Volatility:20%, Tree grid:12, 
    printfn "American option price is %f" (evalOption (American (fun x -> max (50.0-x) 0.0)) 51.0 0.01 1.0 0.2 12)
    printfn "European option price is %f" (evalOption (European (fun x -> max (50.0-x) 0.0)) 51.0 0.01 1.0 0.2 12)

    0 // return an integer exit code