#load "load-project.fsx"

open Swensen.Unquote

open Informedica.GenSolver.Utils
open Informedica.GenSolver.Lib

module VAR = Informedica.GenSolver.Dtos.Variable
module E = Informedica.GenSolver.Dtos.Equation

/// Initialize the solver returning a set of equations
let init eqs = 
    let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
    let createProdEqs = List.map E.createProd
    let createSumEqs  = List.map E.createSum

    let parse eqs op = 
        eqs 
        |> List.map (String.splitAt '=')
        |> List.map (Array.collect (String.splitAt op))
        |> List.map (Array.map String.trim)
        |> List.map (Array.map VAR.createNew)
            
    (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)


/// Print a set of equations to the stdout.
let printEqs eqs = 
    for e in eqs do printfn "%s" (e |> E.toString)


let eqs = 
    init 
        [
            "total = freq * quant"
            "quant = time * rate"
            "total = comp.quant1 + comp.quant2"
        ]
eqs |> printEqs

//Solver.solve eqs "total" "vals" "1,2,4" |> fst |> printEqs
//Solver.solve eqs "total" "min" "1" |> fst |> printEqs
//Solver.solve eqs "total" "max" "1" |> fst |> printEqs
//Solver.solve eqs "total" "incr" "1" |> fst |> printEqs
//Solver.solve eqs "total" "x" "1" |> fst |> printEqs

List.iter2