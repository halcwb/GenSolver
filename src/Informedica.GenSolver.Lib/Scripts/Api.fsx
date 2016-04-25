#load "load-project.fsx"

open Swensen.Unquote

open Informedica.GenSolver.Utils

module VAR = Informedica.GenSolver.Dtos.Variable
module E = Informedica.GenSolver.Dtos.Equation
module Solver = Informedica.GenSolver.Lib.Solver

/// Initialize the solver returning a set of equations
let init eqs = 
    let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
    let createProdEqs = List.map (E.createProd >> E.fromDtoExc)
    let createSumEqs  = List.map (E.createSum  >> E.fromDtoExc)

    let parse eqs op = 
        eqs 
        |> List.map (String.splitAt '=')
        |> List.map (Array.collect (String.splitAt op))
        |> List.map (Array.map String.trim)
        |> List.map (Array.map VAR.createNew)
            
    (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)


/// Print a set of equations to the stdout.
let printEqs eqs = 
    for e in eqs |> List.map E.toDto do 
        printfn "%s" (e |> E.toString)
    printfn "-----"
    eqs


let solve n p v eqs =
    eqs 
    |> List.map E.toDto
    |> List.map (E.setVar n p v)
    |> List.map E.fromDtoExc
    |> Solver.solve
    |> printEqs


let eqs = 
    init 
        [
            "total = freq * quant"
            "quant = time * rate"
            "total = comp.quant1 + comp.quant2"
        ]
eqs |> printEqs |> ignore

eqs
|> solve "total" "vals" "1,2,3,4" 
|> solve "comp.quant1" "minincl" "1"
|> solve "comp.quant1" "minincl" "1"
|> ignore

//    eqs |> Solver.solve

//Solver.solve eqs "total" "vals" "1,2,4" |> fst |> printEqs
//Solver.solve eqs "total" "min" "1" |> fst |> printEqs
//Solver.solve eqs "total" "max" "1" |> fst |> printEqs
//Solver.solve eqs "total" "incr" "1" |> fst |> printEqs
//Solver.solve eqs "total" "x" "1" |> fst |> printEqs

