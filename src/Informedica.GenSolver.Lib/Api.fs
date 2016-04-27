namespace Informedica.GenSolver

module Api =

    open System
    open Informedica.GenSolver.Utils

    module VAR = Informedica.GenSolver.Dtos.Variable
    module E = Informedica.GenSolver.Dtos.Equation
    module Solver = Informedica.GenSolver.Lib.Solver
    module Equation = Informedica.GenSolver.Lib.Equation

    /// Initialize the solver returning a set of equations
    let init eqs = 
        let notempty = String.IsNullOrWhiteSpace >> not
        let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
        let createProdEqs = List.map (E.createProd >> E.fromDtoExc)
        let createSumEqs  = List.map (E.createSum  >> E.fromDtoExc)

        let parse eqs op = 
            eqs 
            |> List.map (String.splitAt '=')
            |> List.map (Array.collect (String.splitAt op))
            |> List.map (Array.map String.trim)
            |> List.map (Array.filter notempty)
            |> List.map (Array.map VAR.createNew)
            
        (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)


    /// Print a set of equations to the stdout.
    let printEqs eqs = 
        for e in eqs |> List.map E.toDto do 
            printfn "%s" (e |> E.toString)
        printfn "-----"
        eqs    

    let solve n p v eqs =
        printfn "Setting variable %s %s with %s" n p v
        eqs 
        |> List.map E.toDto
        |> List.map (E.setVar n p v)
        |> List.map E.fromDtoExc
        |> Solver.solve
        |> printEqs

    let nonZeroNegative eqs =
        eqs 
        |> List.map Equation.nonZeroOrNegative


