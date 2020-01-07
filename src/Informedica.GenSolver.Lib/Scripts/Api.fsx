
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"

#load "../Utils.fs"
#load "../Variable.fs"
#load "../Equation.fs"
#load "../Solver.fs"
#load "../Api.fs"

#time

open Informedica.GenUtils.Lib.BCL
open Informedica.GenSolver.Utils
open MathNet.Numerics

module Api = Informedica.GenSolver.Lib.Api
module Solver = Informedica.GenSolver.Lib.Solver

let procss s = "> " + s + " </br> "|> String.replace "*" "\*" |> printfn "%s"

let printEqs = Api.printEqs true procss
let solve    = Api.solve id procss true None
let init     = Api.init
let nonZeroNegative = Api.nonZeroNegative

let eqs = " = "
let tms = " * "
let add = " + "

["A" + eqs + "B"]
|> Api.init
|> solve "A" "foo" [1N]

// Test set min smaller than incr
["A"]
|> Api.init
|> solve "A" "incr" [1N]
|> solve "A" "minincl" [ 1N / 10N ]

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


open Informedica.GenSolver.Lib
module Name = Variable.Name
module ValueRange = Variable.ValueRange

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

a[31N/500000000; 31/468750000; 31/250000000; 31/234375000; 31/187500000; 93/500000000; 31/156250000] = 1.chloride.Item.DoseAdjust.RateAdjust[31/5000000000000; 31/4687500000000; 31/2500000000000; 31/2343750000000; 31/1875000000000; 93/5000000000000; 31/1562500000000] * 1.dopamin infusion.Adjust.Qty[10000] 
