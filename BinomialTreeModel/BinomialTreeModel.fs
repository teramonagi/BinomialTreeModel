module BinomialTreeModel

//Binary tree(used as binomial tree)
type BinaryTree = 
    | Empty
    | Node of float*BinaryTree*BinaryTree
//Option type with payoff
type OptionType =
    | American
    | European
//Make binary tree
let makeTree s0 u d n =
    let calc (v,i) = s0*(u**float(v))*(d**float(i))
    let rec makeTreeInner m = 
        match m with
            | 0 ->
                [| for i in 0..n -> Node(calc(n-i, i), Empty, Empty) |]
            | _ -> 
                let childs = makeTreeInner (m-1)
                [| for i in 0..(n-m) -> Node(calc(n-m-i, i), childs.[i], childs.[i+1]) |]   
    (makeTreeInner n).[0]

//Option Pricer
let evalOption optionType payoff s0 r T vol n =
    let dt = T/(float n)
    let u = exp(vol*sqrt(dt))
    let d = 1.0/u
    let tree = makeTree s0 u d n
    let p = (exp(r*dt)-d)/(u-d)
    let rec evalOptionInner tree =
        match tree with 
            | Empty -> 0.0
            | Node(value, Empty, Empty) -> payoff value
            | Node(value, lNode, rNode) -> 
                let lValue = evalOptionInner lNode                
                let rValue = evalOptionInner rNode
                let currentValue = (exp(-r*dt)*(p*lValue+(1.0-p)*rValue))
                match optionType with 
                    | American -> max (payoff value) currentValue
                    | European -> currentValue
    evalOptionInner tree

