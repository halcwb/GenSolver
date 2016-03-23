namespace Informedica.GenSolver.Lib

/// Implementations of solvers for product equations
/// sum equations and a set of product and/or sum
/// equations
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Solver =
    
    open Informedica.GenSolver.Utils
    
    open Equation

    /// Create the equation solver using a 
    /// product equation and a sum equation solver
    /// and function to determine whether an 
    /// equation is solved
    let createSolve (SolveProductEquation solveProd) 
                    (SolveSumEquation solveSum) 
                    isSolved : Solve  =
        fun eqs ->

            let rec solveEqs restEqs accEqs  =
                match restEqs with
                | [] -> accEqs
                | eq::rest ->
                    // If the equation is already solved, just put it to 
                    // the accumulated equations and go on with the rest
                    if eq |> isSolved then
                        [eq] 
                        |> List.append accEqs
                        |> solveEqs rest

                    // Else go solve the equation
                    else
                        // Pick the right solve function
                        let solveEq =
                            match eq with
                            | ProductEquation(_) -> fun _ -> eq |> solveProd
                            | SumEquation(_)     -> fun _ -> eq |> solveSum

                        match solveEq ()  with
                        // Equation is changed, so every other equation can 
                        // be changed as well (if changed vars are in the other
                        // equations, so start new
                        | Yes, eq' -> 
                            solveEqs (accEqs @ [eq'] @ rest)  []
                        // Equation did not in fact change, so put it to
                        // the accumulated equations and go on with the rest
                        | No, eq' ->
                            [eq'] 
                            |> List.append accEqs
                            |> solveEqs rest

            solveEqs eqs []

    /// Initialize the solver returning a set of equations
    let init eqs = 
        let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
        let createProdEqs = List.map Equation.Dto.createProd
        let createSumEqs  = List.map Equation.Dto.createSum

        let parse eqs op = 
            eqs 
            |> List.map (String.splitAt '=')
            |> List.map (Array.collect (String.splitAt op))
            |> List.map (Array.map String.trim)
            |> List.map (Array.map Variable.Dto.createNew)
            
        (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)

    /// Initial dummy implementation of solve
    let solve eqs n p v = 
        eqs
        |> List.map (Equation.Dto.setVar n p v), false

    /// Print a set of equations to the stdout.
    let printEqs eqs = 
        for e in eqs do printfn "%s" (e |> Equation.Dto.toString)


