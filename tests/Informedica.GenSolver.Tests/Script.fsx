// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#I "../../src/Informedica.GenSolver.Lib/"
#I "Scripts/"
#load "load-references.fsx"
#load "Variable.fs"

// Define your library scripting code here
open Informedica.GenSolver.Lib

open Swensen.Unquote
open FsCheck
open NUnit.Framework


let bigRGen (n, d) = 
        let d = if d = 0 then 1 else d
        let n' = abs(n) |> BigRational.FromInt
        let d' = abs(d) |> BigRational.FromInt
        n'/d'

let bigRGenerator =
    gen {
        let! n = Arb.generate<int>
        let! d = Arb.generate<int>
        return bigRGen(n, d)
    }

type MyGenerators () =
    static member BigRational () =
        { new Arbitrary<BigRational>() with
            override x.Generator = bigRGenerator }

Arb.register<MyGenerators>() |> ignore

Check.Quick(fun  (a:BigRational) -> printfn "%A" (BigRational.ToDouble(a)); true)




printfn "%A" (1N/2N)
