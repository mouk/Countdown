// Learn more about F# at http://fsharp.net
type Op = Add | Sub | Mul | Div
type Expr = Val of int 
            |  App of Op * Expr * Expr


let apply  op e1 e2  =
    match op with
    | Add -> e1 + e2
    | Sub -> e1 - e2
    | Mul -> e1 * e2
    | Div -> e1 / e2
    
let valid op first second  = 
    match op with
    |Add -> true        
    |Sub -> first >= second
    |Mul -> true   
    |Div-> (second <> 0) && (first % second = 0)    

let rec subSets items =
    match items with
    | [] -> seq{yield []}
    | x::xs -> seq{
                let sub = subSets xs
                yield! sub
                yield! sub |> Seq.map (fun l -> x::l) }
        
let rec permutations items =
    match items with
    | [] -> seq{yield []}
    | _ ->  seq{for x in items do
                let others = [for c in items do 
                                if c <> x then 
                                    yield c ]
                yield! permutations others 
                    |> Seq.map (fun l -> x::l) }
            
let choices items = 
    seq{for x in subSets items do    
        yield! permutations x }

let rec split  items =
    match items with
    |[] -> Seq.empty
    |[_] -> Seq.empty
    |x::xs -> seq{yield  ([x],xs)
                  for (l1,l2) in split xs do
                        yield (x::l1,l2)}


let rec gen_expr nums =
    match nums with
    |[] -> Seq.empty
    |[a] -> seq{yield Val(a)}
    |_->seq{for (f,s) in split nums do
                for first in gen_expr f do
                 for second in gen_expr s do
                    for op in [Add;Sub;Mul;Div] do
                        yield App(op,first,second)}
                        
let gen_all_expr nums =
    seq{for poss in choices nums do
            yield! gen_expr poss}
                        
let rec eval expr =
    match expr with
    |Val(n) -> seq{yield n}
    |App(op, s, f)  -> seq{for se in eval s do
                            for fe in eval f do
                                if valid op se fe then
                                    yield apply op se fe}
                                    
                                    
let solutions nums result =
    seq {for ex in gen_all_expr nums do 
            if (eval ex |> Seq.to_list) = [result] then
                yield ex}
        
let solve nums result =
    solutions nums result
    |> Seq.take 1
    
let countResults nums result =
    solutions nums result
    |> Seq.length
    
    
    
let rec format expr = 
    match expr with
    |Val(n) -> n.ToString()
    |App(Add,f,s) -> "(" + format f + " + " + format s + ")"
    |App(Sub,f,s) -> "(" + format f + " - " + format s + ")"
    |App(Mul,f,s) ->  format f + " * " + format s 
    |App(Div,f,s) ->  format f + " / " + format s 
    
    
    
//let test()  = solve [1; 3; 7; 10; 25; 50] 765    
let res()  = countResults [1; 3; 7; 10; 25; 50] 765 
let test () =
    let sw = new System.Diagnostics.Stopwatch()
    do sw.Start()
    let res = countResults [1; 3; 7; 10; 25; 50] 765
    sw.Stop()
    printfn "Elapsed Time %s\n" (sw.Elapsed.ToString())
    
let rec calcnum num =
    match num with
    | 1 -> 1
    | 2 -> 1
    | _ -> seq{for i in 1..(num-1) do
                yield calcnum i * calcnum (num - i)}
                |>  Seq.fold(fun acc a -> acc + a) 0
        
     
