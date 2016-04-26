namespace Informedica.GenSolver.Lib

/// Implementations of solvers for product equations
/// sum equations and a set of product and/or sum
/// equations
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Solver =
    
    open Informedica.GenSolver.Utils
    
    module VAR = Variable
    module E = Equation
 
    /// The `Result` of solving an `Equation`
    /// is that either the `Equation` is the 
    /// same or has `Changed`.
    type Result =
        | UnChanged
        | Changed   of E.Equation list

    /// Solve the equation `e` and return 
    /// the set of equations `es` it belongs 
    /// to either as `Changed` or `Unchanged`
    let solveEquation e es = 
        let changed, es' = e |> E.solve es
        if changed then es' |> Changed else UnChanged
        
    /// Create the equation solver using a 
    /// product equation and a sum equation solver
    /// and function to determine whether an 
    /// equation is solved
    let createSolve solve isSolvable =
        fun eqs ->

            let rec solveEqs restEqs accEqs  =
                match restEqs with
                | [] -> accEqs
                | eq::rest ->
                    // If the equation is already solved, or not solvable 
                    // just put it to  the accumulated equations and go on with the rest
                    if eq |> isSolvable |> not then
                        [eq] 
                        |> List.append accEqs
                        |> solveEqs rest

                    // Else go solve the equation
                    else
                        match accEqs @ restEqs |> solve eq with
                        // Equation is changed, so every other equation can 
                        // be changed as well (if changed vars are in the other
                        // equations, so start new
                        | Changed(eqs') -> solveEqs eqs' []
                        // Equation did not in fact change, so put it to
                        // the accumulated equations and go on with the rest
                        | UnChanged ->
                            [eq] 
                            |> List.append accEqs
                            |> solveEqs rest

            solveEqs eqs []
    
    /// Solve an `Equation` list and return the 
    /// updated list.
    let solve = createSolve solveEquation E.isSolvable


