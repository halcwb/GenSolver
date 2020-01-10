namespace Informedica.GenSolver.Lib

/// Implementations of solvers for product equations
/// sum equations and a set of product and/or sum
/// equations
module Solver =
    
    open Informedica.GenSolver.Utils

    module EQD = Equation.Dto

    type Variable = Variable.Variable
    
    /// Format a set of equations to print.
    /// Using **f** to allow additional processing
    /// of the string.
    let printEqs exact pf eqs = 
        let eqs = 
            eqs 
            |> List.sortBy (fun e ->
                e 
                |> Equation.toVars
                |> List.head
                |> Variable.getName)

        "equations result:\n" |> pf
        eqs
        |> List.map EQD.toDto
        |> List.iteri (fun i dto ->
            dto
            |> EQD.toString exact
            |> sprintf "%i.\t%s" i
            |> pf
        )
//        sprintf "raw: \n%A" eqs |> pf
        "-----" |> pf 

        eqs    
    

    /// Checks whether a list of `Equation` **eqs**
    /// contains an `Equation` **eq**
    let contains eq eqs = eqs |> List.exists ((=) eq)

    /// The `Result` of solving an `Equation`
    /// is that either the `Equation` is the 
    /// same or has `Changed`.
    type Result =
        | UnChanged
        | Changed   of Variable list


    /// Replace a list of `Variable` **vs**
    /// in a list of `Equation` **es**, return
    /// a list of replaced `Equation` and a list
    /// of unchanged `Equation`
    let replace vars es =
        let rpl, rst = 
            es 
            |> List.partition (fun e -> 
                vars 
                |> List.exists (fun v -> 
                    e 
                    |> Equation.contains v
                )
            )

        vars 
        |> List.fold (fun acc v -> 
            acc 
            |> List.map (Equation.replace v)
        ) rpl
        , rst

    /// Solve the equation `e` and return 
    /// the set of equations `es` it belongs 
    /// to either as `Changed` or `Unchanged`
    let solveEquation calc log e = 
        "going to solve equation:\n" |> log
        [e]
        |> printEqs true log
        |> ignore

        let changed = 
            e 
            |> Equation.solve calc log

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
    let solve calc log sortQue vr eqs =

        let solveE = solveEquation calc log

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
                    |> printEqs true log
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

                        let eq = [ eq ] |> replace vars |> fst
                        eq |> printEqs true log |> ignore

                        // find all eqs with vars in acc and put these back on que
                        acc
                        |> replace vars
                        |> function
                        | (rpl, rst) ->
                            // replace vars in tail
                            let que = 
                                tail
                                |> replace vars
                                |> function 
                                | (es1, es2) ->
                                    es1
                                    |> List.append es2
                                    |> List.append rpl

                            rst
                            |> List.append eq
                            |> loop (n + 1) que 

                    // Equation did not in fact change, so put it to
                    // the accumulated equations and go on with the rest
                    | UnChanged ->
                        [eq] 
                        |> List.append acc
                        |> loop (n + 1) tail

        eqs 
        |> replace [vr]
        |> function 
        | (rpl, rst) -> loop 0 (rpl |> sortQue) rst
            
            
    


