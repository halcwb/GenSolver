
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
