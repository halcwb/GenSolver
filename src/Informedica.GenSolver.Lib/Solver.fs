namespace Informedica.GenSolver.Lib

/// Implementations of solvers for product equations
/// sum equations and a set of product and/or sum
/// equations
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Solver =
    
    open Informedica.GenSolver.Utils
    
    open Equation


    // #region ---- SOLVE MIN MAX ----

    let optChoose x1 x2 = if x2 |> Option.isSome then x2.Value else x1

    /// Determine wheter the min and max of
    /// a dependent variable can be set because
    /// of existing min and max values of two 
    /// independent variables in an equation like: </br>
    /// y  = x1 * x2 or </br>
    /// x1 = y / x2 </br>
    /// For example when x1 has a minimum of 1 and
    /// x2 has a minimum of 3 then the dependent y will
    /// have a minimum of at least 3.
    let solveProductMinMax setMin setMax x1 x2 y =

        let getMin = Variable.ValueRange.getMin
//        let setMin = Variable.ValueRange.setMin fs ff

        let getMax = Variable.ValueRange.getMax
//        let setMax = Variable.ValueRange.setMax fs ff

        // Helper function to determine whether 
        // depending on cmp and op functions
        // the result of an operation between
        // two values can be set to the dependent
        // value.
        let set cmp op getf setf x = function
            | Some v1, Some v2 ->
                let r = v1 |> op <| v2
                match x |> getf with
                | Some v3 -> 
                    if cmp r v3 then x |> setf r 
                    else x 
                    |> Some
                | None -> x |> setf r |> Some
            | _ -> None

        let setXmin = set (>) (/) getMin setMin
        let setXmax = set (<) (/) getMax setMax

        let setYmin = set (>) (*) getMin setMin 
        let setYmax = set (<) (*) getMax setMax

        // Rule 1: x2.min = y.min / x1.max if x1.max = set && y.max / x1.max > x2.min || x2.min = not set 
        let x1 = setXmin x1 (y |> getMin, x2 |> getMax) |> optChoose x1
        let x2 = setXmin x2 (y |> getMin, x1 |> getMax) |> optChoose x2

        // Rule 2: x2.max = y.max / x1.min if x1.min = set and vice versa
        let x1 = setXmax x1 (y |> getMax, x2 |> getMin) |> optChoose x1
        let x2 = setXmax x2 (y |> getMax, x1 |> getMin) |> optChoose x2

        // Rule 3: y.min = Product(x.min) if all x.min = set
        let y = setYmin y (x1 |> getMin, x2 |> getMin) |> optChoose y

        // Rule 4: y.max = Product(x.max) if all x.max = set
        let y = setYmax y (x2 |> getMax, x2 |> getMax) |> optChoose y
        
        [y;x1;x2]

    /// Determine wheter the min and max of
    /// a dependent variable can be set because
    /// of existing min and max values of two 
    /// independent variables in an equation like: </br>
    /// y = x1 + x2 or </br>
    /// x1 = y - x2 </br>
    /// For example when x1 has a minimum of 3 and
    /// x2 has a minimum of 2 then the dependent y will
    /// have a minimum of at least 4.
    let solveSumMinMax setMin setMax xs y =

        let zero = 0N |> Variable.ValueRange.Value.Value
        
        let getMin = Variable.ValueRange.getMin
        let getMax = Variable.ValueRange.getMax

        let sumVar getf vars  =
            vars 
            |> List.map getf 
            |> List.filter Option.isSome
            |> List.map Option.get
            |> List.fold (+) zero

        let set cmp getf setf v var =
            match v, var |> getf with
            | Some v', Some x -> 
                if cmp v' x then var |> setf v' |> Some
                else None
            | Some v', None   -> 
                if v' > zero then var |> setf v' |> Some
                else None
            | _ -> None
        
        let setMax = set (<) (getMax) (setMax)
        let setMin = set (>) (getMin) (setMin)
        
        // Rule 1: var.max = sum.max if var.max = not set || sum.max < var.max
        // I.e. the max of an independent variable can never be larger than the
        // max of the dependent variable.
        // ToDo In fact it cannot be equal to the max of the dependent. Adjust 
        // var structure according to that!
        let xs = xs |> List.map (fun v ->
            v |> setMax (y |> getMax) |> optChoose v)

        // Rule 2: var.min = sum.min - Sum(var.min) if n - 1 var.min = set
        let xs = 
            match xs |> List.filter(getMin >> Option.isNone) with
            | [var] -> 
                let min = 
                    match y |> getMin with
                    | None -> None
                    | Some min -> min - (xs 
                                            |> List.map getMin 
                                            |> List.filter Option.isSome 
                                            |> List.map Option.get
                                            |> List.fold (+) zero) 
                                    |> Some
                let var' = var |> setMin min |> optChoose var
                xs |> List.replace ((=) var) var'
            | _ -> xs

        // Rule 3: sum.min = Sum(var.min) if Sum(var.min) > sum.min
        let y =
            setMin (match sumVar getMin xs with | v when v > zero -> v |> Some | _ -> None) y
            |> optChoose y
            
        // Rule 4: sum.max = Sum(var.max) if all var.max = set
        let y =
            if xs |> List.map getMax |> List.exists Option.isNone then y
            else
                setMax (match sumVar getMax xs with | v when v > zero -> v |> Some | _ -> None) y |> optChoose y
        
        y::xs

    // #endregion


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


