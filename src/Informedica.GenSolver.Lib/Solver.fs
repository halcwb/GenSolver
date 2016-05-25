namespace Informedica.GenSolver.Lib

/// Implementations of solvers for product equations
/// sum equations and a set of product and/or sum
/// equations
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Solver =
    
    open Informedica.GenSolver.Utils
    
    module VAR = Variable
    module E = Equation
 
    let replace vs es =
        let rpl, rst = 
            es 
            |> List.partition (fun e -> 
                vs |> List.exists (fun v -> e |> E.contains v))

        vs 
        |> List.fold (fun acc v -> 
            acc |> List.map (fun e -> e |> E.replace v)) rpl
        , rst

    let contains eq eqs = eqs |> List.exists ((=) eq)

    /// The `Result` of solving an `Equation`
    /// is that either the `Equation` is the 
    /// same or has `Changed`.
    type Result =
        | UnChanged
        | Changed   of VAR.Variable list * E.Equation

    /// Solve the equation `e` and return 
    /// the set of equations `es` it belongs 
    /// to either as `Changed` or `Unchanged`
    let solveEquation e = 
        let changed, e = e |> E.solve
        if changed |> List.length > 0 then 
            (changed, e) |> Changed 
        else UnChanged
        
    /// Create the equation solver using a 
    /// product equation and a sum equation solver
    /// and function to determine whether an 
    /// equation is solved
    let createSolve solve isSolvable =
        fun eqs ->

            let rec solveEqs que acc  =
                match que with
                | [] -> acc
                | eq::tail ->
                    // If the equation is already solved, or not solvable 
                    // just put it to  the accumulated equations and go on with the rest
                    if eq |> isSolvable |> not then
                        [eq] 
                        |> List.append acc
                        |> solveEqs tail

                    // Else go solve the equation
                    else
                        match solve eq with
                        // Equation is changed, so every other equation can 
                        // be changed as well (if changed vars are in the other
                        // equations, so start new
                        | Changed(vs, e) ->
                            let que, acc =  
                                let rpl, rst = 
                                    acc
                                    |> List.append que 
                                    |> replace vs

                                let all = rpl @ rst

                                if ((que @ acc) |> List.length) <> ((rpl |> List.length) + (rst |> List.length)) then
                                    failwith "Replace not equal list length"

                                // New que with replaced equations and
                                // equations that were allready in the que
                                let que' = 
                                    rpl
                                    |> List.append (rst |> List.filter (fun e -> 
                                        que |> List.exists (E.equals e)))
                                    
                                // acc = all eqs - que
                                let acc' = 
                                    rst |> List.filter (fun e ->
                                        que' |> List.forall (fun e' -> e' |> E.equals e |> not))

                                if (all |> List.length) <> ((que' |> List.length) + (acc' |> List.length)) then
                                    printfn "Original %A, %A, %A" ((que @ acc) |> List.length) (que |> List.length) (acc |> List.length)
                                    printfn "New %A, %A, %A" (all |> List.length) (que' |> List.length) (acc' |> List.length)
                                    failwith "Que and acc not equal list length"

                                que', acc'

                            printfn "Eqs: %A, Que: %A, Acc: %A" ((que |> List.length) + (acc |> List.length)) (que |> List.length) (acc |> List.length)
                            solveEqs que acc
                        // Equation did not in fact change, so put it to
                        // the accumulated equations and go on with the rest
                        | UnChanged ->
                            [eq] 
                            |> List.append acc
                            |> solveEqs tail

            solveEqs eqs []
    
    /// Solve an `Equation` list and return the 
    /// updated list.
    let solve = createSolve solveEquation E.isSolvable


