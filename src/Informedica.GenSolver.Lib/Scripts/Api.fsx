
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"

#load "../Utils.fs"
#load "../Variable.fs"
#load "../Equation.fs"
#load "../Solver.fs"
#load "../Api.fs"

#time

open Informedica.GenSolver.Lib
open Informedica.GenUtils.Lib.BCL
open Informedica.GenSolver.Utils
open MathNet.Numerics

module Api = Informedica.GenSolver.Lib.Api
module Solver = Informedica.GenSolver.Lib.Solver
module Name = Variable.Name
module ValueRange = Variable.ValueRange
module Props = Api.Props

let procss s = "> " + s + " </br> "|> String.replace "*" "\*" |> printfn "%s"

let printEqs = Api.printEqs true procss
let solve n  = 
    let n = n |> Name.createExc
    Api.solve id procss true None n
let init     = Api.init
let nonZeroNegative = Api.nonZeroNegative

let eqs = " = "
let tms = " * "
let add = " + "

// Test set min smaller than incr
["A"]
|> Api.init
|> solve "A" (1N |> Set.singleton |> Props.Increment )
|> solve "A" (Props.MaxIncl (1N / 10N))


// Test set max larget than max
["A"]
|> Api.init
|> solve "A" (Props.MaxIncl 1N)
|> solve "A" (Props.MaxIncl 10N)

// Test set min smaller than incr
["A"]
|> Api.init
|> solve "A" "incr" [1N]
|> solve "A" "vals" [1N / 10N]


// Test sum equation
["a" + eqs + "b" + add + "c"]
|> Api.init
|> Api.nonZeroNegative
|> solve "a" "vals" [5N]
|> solve "b" "incr" [1N]
|> solve "c" "vals" [2N]


// Test sum equation and product equation
[
    "c" + eqs + "d" + tms + "a"
//    "a" + eqs + "b" + add + "c"
    "a" + eqs + "f" + add + "g"
]
|> Api.init
|> Api.nonZeroNegative
|> solve "d" "vals" [1N; 2N]
|> solve "c" "vals" [10N]
|> solve "f" "vals" [5N]


// FAILING CASE
// setting a to 3/50
//a[3/50, 3/25, 6/25] = b[3/50, 3/25, 6/25] * c[1] 
//d[3/50, 3/25, 6/25] = e[3/50, 3/25, 6/25] * f[1] 
[
    "a" + eqs + "b" + tms + "c"
//    "a" + eqs + "b" + add + "c"
    "d" + eqs + "e" + tms + "f"
]
|> Api.init
|> Api.nonZeroNegative
|> solve "a" "vals" [(3N/50N); (3N/25N); (6N/25N)]
|> solve "b" "vals" [(3N/50N); (3N/25N); (6N/25N)]
|> solve "c" "vals" [1N]
|> solve "d" "vals" [(3N/50N); (3N/25N); (6N/25N)]
|> solve "e" "vals" [(3N/50N); (3N/25N); (6N/25N)]
|> solve "f" "vals" [1N]
|> solve "a" "vals" [(3N/50N)]



let vara =
    [
        "a" 
    ]
    |> Api.init
    |> solve "a" "vals" [1N..5N]
    |> function
    | [e1] ->
        e1 |> Equation.findName (Name.createExc "a") |> Seq.head
 

[] |> List.replaceOrAdd (Variable.eqName vara) vara
let varc =  
    [1N..2N]
    |> Set.ofList
    |> ValueRange.createValueSet 
    |> Variable.setValueRange vara
[vara] |> List.replaceOrAdd (Variable.eqName vara) varc

[ { vara with Name = "b" |> Name.createExc }]
|> List.replaceOrAdd (Variable.eqName vara) vara


let avals = [31N/500000000N; 31N/468750000N; 31N/250000000N; 31N/234375000N; 31N/187500000N; 93N/500000000N; 31N/156250000N]
let bvals =[31N/5000000000000N; 31N/4687500000000N; 31N/2500000000000N; 31N/2343750000000N; 31N/1875000000000N; 93N/5000000000000N; 31N/1562500000000N] 
let cvals = [10000N] 

let avar =
    avals
    |> Set.ofList
    |> ValueRange.createValueSet
    |> Variable.createSucc ("a" |> Name.createExc)

let bvar =
    bvals
    |> Set.ofList
    |> ValueRange.createValueSet
    |> Variable.createSucc ("b" |> Name.createExc)

let cvar =
    cvals
    |> Set.ofList
    |> ValueRange.createValueSet
    |> Variable.createSucc ("c" |> Name.createExc)

(avar, [bvar; cvar]) 
|> Equation.createProductEqExc
|> Equation.solve (printfn "%s")

1000N * (7N/3600000N)
|> BigRational.toFloat
|> printfn "%A"

1000N * (531N/61600000N)
|> BigRational.toFloat
|> printfn "%A"

printfn "%A - %A" (1000N * (9N/100000N) |> BigRational.toFloat)
                  (1000N * (1N/2250N) |> BigRational.toFloat)



[
    for x in [1..39999] do 
        for y in [1..39999] do x / y
]
|> List.length

printfn "going to calc list length"
[(1N/500N)..1N/1000N..(2000000N/50000N)]
|> List.length


(10000N*60N*60N*2000N/30000000N) |> BigRational.toFloat
(1000N*60N*60N/36000000N) |> BigRational.toFloat
(60N*60N/(2000N*36000000N)) |> BigRational.toFloat