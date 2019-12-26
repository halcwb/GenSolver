
#I __SOURCE_DIRECTORY__

#load "../../../.paket/load/netstandard2.1/main.group.fsx"

#load "../Utils.fs"
#load "../Variable.fs"
#load "../Equation.fs"
#load "../Solver.fs"
#load "../Dtos.fs"
#load "../Api.fs"

#time

open Informedica.GenSolver.Utils
open MathNet.Numerics

module Api = Informedica.GenSolver.Api

let procss s = "> " + s + " </br> "|> String.replace "*" "\*" |> printfn "%s"

let printEqs = Api.printEqs procss
let solve    = Api.solve procss
let init     = Api.init
let nonZeroNegative = Api.nonZeroNegative

let eqs = " = "
let tms = " * "
let add = " + "

["A" + eqs + "B"]
|> Api.init
|> Api.solve (printfn "%s") "A" "foo" [1N]

// Test set min smaller than incr
["A"]
|> Api.init
|> Api.solve (printfn "%s") "A" "incr" [1N]
|> Api.solve (printfn "%s") "A" "minincl" [ 1N / 10N ]

// Test sum equation
["a" + eqs + "b" + add + "c"]
|> Api.init
|> Api.nonZeroNegative
|> Api.solve (printfn "%s") "a" "vals" [5N]
|> Api.solve (printfn "%s") "b" "incr" [1N]
|> Api.solve (printfn "%s") "c" "vals" [2N]


// Test sum equation and product equation
[
    "c" + eqs + "d" + tms + "a"
    "a" + eqs + "b" + add + "c"
//    "e" + eqs + "f" + tms + "g"
]
|> Api.init
|> Api.nonZeroNegative
|> Api.solve (printfn "%s") "a" "vals" [5N]
|> Api.solve (printfn "%s") "b" "incr" [1N]


