namespace Informedica.GenSolver.Lib

/// Implementations of solvers for product equations
/// sum equations and a set of product and/or sum
/// equations
module Solver =
    
    open Informedica.GenSolver.Utils

    type Variable = Variable.Variable
     

    /// Checks whether a list of `Equation` **eqs**
    /// contains an `Equation` **eq**
    let contains eq eqs = eqs |> List.exists ((=) eq)

    /// The `Result` of solving an `Equation`
    /// is that either the `Equation` is the 
    /// same or has `Changed`.
    type Result =
        | UnChanged
        | Changed   of Variable list

    /// Solve the equation `e` and return 
    /// the set of equations `es` it belongs 
    /// to either as `Changed` or `Unchanged`
    let solveEquation log e = 
        let changed = e |> Equation.solve log

        if changed |> List.length > 0 then 
            changed |> Changed 
        else UnChanged


    let memSolve f =
        let cache = ref Map.empty
        fun e ->
            match (!cache).TryFind(e) with
            | Some r -> r
            | None ->
                let r = f e
                cache := (!cache).Add(e, r)
                r

    let sortQue que =
        if que |> List.length = 0 then que
        else
            que 
            |> List.sortBy Equation.countProduct

        
    /// Create the equation solver using a 
    /// product equation and a sum equation solver
    /// and function to determine whether an 
    /// equation is solved
    let solve log sortQue vr eqs =

        let solveE = solveEquation log

        let rec loop n que acc =

            que 
            |> function
            | [] -> "loop with empty que" |> log
            | _  ->
                que
                |> List.last
                |> fun e ->
                    e
                    |> Equation.toVars
                    |> List.map (fun v ->
                        v
                        |> Variable.count
                        |> sprintf "%s: [%i]" (v.Name |> Variable.Name.toString)
                    )
                    |> String.concat ", "
                    |> sprintf "loop %i with max count [%i]: %s" n (e |> Equation.countProduct)
                    |> log


            match que with
            | [] -> 
                match acc |> List.filter (Equation.check >> not) with
                | []   -> acc
                | que -> 
                    que
                    |> List.length
                    |> sprintf  "detected %i invalid equations"
                    |> failwith
                
            | eq::tail ->
                // If the equation is already solved, or not solvable 
                // just put it to  the accumulated equations and go on with the rest
                if eq |> Equation.isSolvable |> not then
                    [ eq ] 
                    |> List.append acc
                    |> loop (n + 1) tail

                // Else go solve the equation
                else
                    match eq |> solveE with
                    // Equation is changed, so every other equation can 
                    // be changed as well (if changed vars are in the other
                    // equations) so start new
                    | Changed vars ->
                        vars
                        |> List.length
                        |> sprintf "%i changed vars" 
                        |> log

                        let que =
                            que
                            |> List.append acc
                            |> sortQue

                        loop (n + 1) que [] 

                    // Equation did not in fact change, so put it to
                    // the accumulated equations and go on with the rest
                    | UnChanged ->
                        [eq] 
                        |> List.append acc
                        |> loop (n + 1) tail

        eqs 
        |> List.partition (Equation.contains vr)
        |> function 
        | (rpl, rst) -> loop 0 (rpl |> sortQue) rst
            
            
    


