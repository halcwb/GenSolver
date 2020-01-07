
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
let solve    = Api.solve Solver.solveEquation id true procss None
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

open System.Collections.Generic

let s1 = new HashSet<int>()
let s2 = new HashSet<int>()
s1.Add(1)

s1.SetEquals(s2)


open Informedica.GenSolver.Lib
module Name = Variable.Name
module ValueRange = Variable.ValueRange

let vara, varb =
    [
        "a"
        "b"
    ]
    |> Api.init
    |> solve "a" "vals" [1N]
    |> solve "b" "vals" [1N]
    |> function
    | [e1; e2] ->
        e1 |> Equation.findName (Name.createExc "a") |> Seq.head ,
        e2 |> Equation.findName (Name.createExc "b") |> Seq.head

vara = vara
let varx = vara
vara.Values <- vara.Values |> ValueRange.setValues (new HashSet<_>())
(vara.Values |> ValueRange.getValueSet).Add(1N)

varx = vara
