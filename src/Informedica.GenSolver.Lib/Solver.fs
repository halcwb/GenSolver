namespace Informedica.GenSolver.Lib

/// Implementations of solvers for product equations
/// sum equations and a set of product and/or sum
/// equations
module Solver =
    
    open Informedica.GenSolver.Utils
    
    module VAR = Variable
    module EQ = Equation
 
    /// Replace a list of `Variable` **vs**
    /// in a list of `Equation` **es**, return
    /// a list of replaced `Equation` and a list
    /// of unchanged `Equation`
    let replace vs es =
        let rpl, rst = 
            es 
            |> List.partition (fun e -> 
                vs |> List.exists (fun v -> e |> EQ.contains v))

        vs 
        |> List.fold (fun acc v -> 
            acc |> List.map (fun e -> e |> EQ.replace v)) rpl
        , rst

    /// Checks whether a list of `Equation` **eqs**
    /// contiains and `Equation` **eq**
    let contains eq eqs = eqs |> List.exists ((=) eq)

    /// The `Result` of solving an `Equation`
    /// is that either the `Equation` is the 
    /// same or has `Changed`.
    type Result =
        | UnChanged
        | Changed   of VAR.Variable list

    /// Solve the equation `e` and return 
    /// the set of equations `es` it belongs 
    /// to either as `Changed` or `Unchanged`
    let solveEquation e = 
        let changed = e |> EQ.solve
        if changed |> List.length > 0 then 
            changed |> Changed 
        else UnChanged
        
    /// Create the equation solver using a 
    /// product equation and a sum equation solver
    /// and function to determine whether an 
    /// equation is solved
    let solve vr eqs =
        
        let rec loop que acc  =
            let que =
                que 
                |> List.sortBy Equation.count

            match que with
            | [] -> acc
            | eq::tail ->
                // If the equation is already solved, or not solvable 
                // just put it to  the accumulated equations and go on with the rest
                if eq |> EQ.isSolvable |> not then
                    [eq] 
                    |> List.append acc
                    |> loop tail

                // Else go solve the equation
                else
                    match eq |> solveEquation with
                    // Equation is changed, so every other equation can 
                    // be changed as well (if changed vars are in the other
                    // equations) so start new
                    | Changed vs ->
                        let all =  
                            let rpl, rst = (que @ acc) |> replace vs

                            // New que with replaced equations and
                            // equations that were allready in the que
                            let que' = 
                                rpl
                                |> List.append (rst |> List.filter (fun e -> 
                                    que |> List.exists (EQ.equals e)))
                                    
                            // acc = all eqs - que
                            let acc' = 
                                rst |> List.filter (fun e ->
                                    que' |> List.forall (fun e' -> e' |> EQ.equals e |> not))

                            que'@ acc'
                        
                        // need to loop over all equations
                        loop all []
                    // Equation did not in fact change, so put it to
                    // the accumulated equations and go on with the rest
                    | UnChanged ->
                        [eq] 
                        |> List.append acc
                        |> loop tail

        let que, acc = eqs |> replace [vr] 
        loop que acc
    


