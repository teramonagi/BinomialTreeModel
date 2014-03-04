module BinomialTreeModel

//Binary tree(used as binomial tree)
type BinaryTree = 
    | Empty
    | Node of float*BinaryTree*BinaryTree
//Option type with payoff
type OptionType =
    | American of (float -> float)
    | European of (float -> float)
    member this.Payoff = 
        match this with 
            | American payoff -> payoff
            | European payoff -> payoff
//Make binary tree
let makeTree s0 u d n=
    let rec makeTreeInner m = 
        match m with
            | 0 ->
                [| for i in 0..n -> Node(s0*(u**float(n-i))*(d**float(i)), Empty, Empty) |]
            | _ -> 
                let childs = makeTreeInner (m-1)
                [| for i in 0..(n-m) -> Node(s0*(u**float(n-m-i))*(d**float(i)), childs.[i], childs.[i+1]) |]
    (makeTreeInner n).[0]
//Option Pricer
let evalOption (option:OptionType) s0 r T vol n =
    let dt = T/(float n)
    let u = exp(vol*sqrt(dt))
    let d = 1.0/u
    let tree = makeTree s0 u d n
    let p = (exp(r*dt)-d)/(u-d)
    let rec evalOptionInner (option:OptionType) tree =
        match tree with 
            | Empty -> 0.0
            | Node(value, Empty, Empty) -> option.Payoff value
            | Node(value, lNode, rNode) -> 
                let lValue = evalOptionInner option lNode                
                let rValue = evalOptionInner option rNode
                let currentValue = (exp(-r*dt)*(p*lValue+(1.0-p)*rValue))
                match option with 
                    | American payoff -> max (payoff value) currentValue
                    | European _      -> currentValue
    evalOptionInner option tree

