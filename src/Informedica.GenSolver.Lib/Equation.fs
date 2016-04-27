namespace Informedica.GenSolver.Lib

/// Functions that handle the `Equation` type that
/// either represents a `ProductEquation` </br>
/// y = x1 * x2 * ... * xn </br>
/// or a `SumEquations` </br>
/// y = x1 * x2 * ... * xn
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Equation =

    open Informedica.GenSolver.Utils

    module VAR = Variable
    
    /// An equation is either a `ProductEquation`
    /// or a `Sumequation`, the first variable is the
    /// dependent variable, i.e. the result of the 
    /// equation, the second part are the independent
    /// variables in the equation
    type Equation = 
        | ProductEquation of Variable.Variable * Variable.Variable list
        | SumEquation     of Variable.Variable * Variable.Variable list

    /// Error messages
    type Message = 
        | DuplicateVariables of VAR.Variable list

    /// Equation exception
    exception EquationException of Message

    /// Raise an `EquationException` with `Message` `m`.
    let raiseExc m = m |> EquationException |> raise

    /// Create an `Equation` with an `y` and
    /// `xs`. Fails if a variable is added more
    /// than one time using the `fail` function.
    /// The type of Equation product or sum
    /// is determined by the constructor `c`.
    let create c succ fail (y, xs) = 
        let vars = y::xs
        match vars |> List.filter (fun v -> vars |> List.filter ((=) v) |> List.length > 1) with
        | [] -> (y, xs) |> c |> succ
        | duplicates -> duplicates |> DuplicateVariables |> fail

    /// Create an `ProductEquation` with an `y` and
    /// `xs`. Fails if a variable is added more
    /// than one time using the `fail` function.
    let createProductEq = create ProductEquation

    /// Create an `SumEquation` with an `y` and
    /// `xs`. Fails if a variable is added more
    /// than one time using the `fail` function.
    let createSumEq = create SumEquation

    /// Create an `ProductEquation` with an `y` and
    /// `xs`. Fails if a variable is added more
    /// than one time raising an exception.
    let createProductEqExc = createProductEq id raiseExc 

    /// Create an `SumEquation` with an `y` and
    /// `xs`. Fails if a variable is added more
    /// than one time raising an exception.
    let createSumEqExc = createSumEq id raiseExc

    /// Apply `fp` to a `ProductEquation` and
    /// `fs` to a `SumEquation`.
    let apply fp fs = function
        | ProductEquation (y,xs) -> fp y xs
        | SumEquation (y, xs)    -> fs y xs

    /// Make sure that the `Variables` in the
    /// `Equation` can only contain positive 
    /// non zero values.
    let nonZeroOrNegative e =
        let set c y xs =
            let y' = y |> VAR.setNonZeroOrNegative
            let xs' = xs |> List.map VAR.setNonZeroOrNegative
            (y', xs') |> c 
        let fp = set ProductEquation
        let fs = set SumEquation
        e |> apply fp fs

    /// Replace a `Variable` `v` in the 
    /// `Equation` `e`.
    let replace v e =
        let r c v vs =
            let vs = vs |> List.replace ((VAR.eqName) v) v
            c id (fun _ -> e) ((vs |> List.head), (vs|> List.tail))
        let fp y xs = r createProductEq v (y::xs)
        let fs y xs = r createSumEq v (y::xs)
        e |> apply fp fs

    // Check whether an equation is solved
    let isSolved = function
        | ProductEquation (y, xs) 
        | SumEquation (y, xs) ->
            [y] @ xs |> List.forall VAR.isSolved

    // Check whether an equation will change by calc
    // This is not the same as `isSolved`!! If all 
    // the variables are unrestricted than the equation
    // is not solvable but is also not solved.
    let isSolvable = function 
        | ProductEquation (y, xs)
        | SumEquation (y, xs) ->
            ([y] @ xs |> List.exists VAR.isSolvable) &&
            ([y] @ xs |> List.forall VAR.isUnrestricted |> not)


    /// Solve an equation `e`, return true when
    /// the equation has changed. And the original
    /// Equation list `es` with the replaced vars.
    let solve es e =
        let replace es v =
            let es = es |> List.map (replace v)
            es, v

        let rec calc changed es op1 op2 y xs rest =
            match rest with 
            | []  -> changed, es, xs
            | x::tail ->
                let xs' = xs |> List.filter ((<>) x)
                let es, x' =
//                    // If x is already solved, it will not change
//                    if x |> VAR.isSolved then es, x 
//                    // Calculate the new x
//                    else
                        match xs' with
                        | [] -> x != y
                        | _  -> x != (y |> op2 <| (xs' |> List.reduce op1))
                        |> replace es
                tail |> calc (x' <> x || changed) es op1 op2 y (x'::xs')

        let y, xs, op1, op2 =
            match e with
            | ProductEquation (y, xs) -> y, xs, (*), (/)
            | SumEquation     (y, xs) -> y, xs, (+), (-)

        let rec loop op1 op2 y xs changed es =
            let x   = xs |> List.head
            let xs' = xs |> List.filter ((<>) x)
            // op1 = (*) or (+) and op2 = (/) or (-)
            // Calculate y = x1 op1 x2 op1 .. op1 xn
            let ychanged, es, y' = calc false es op1 op1 x xs' [y]
            // Replace y with the new y with is in a list
            let y = y' |> List.head
            // Calculate x1 = y op2 (x2 op1 x3 .. op1 xn)
            //       and x2 = y op2 (x1 op1 x3 .. op1 xn)
            //       etc..
            let xchanged, es, xs = calc false es op1 op2 y xs xs
            // If something has changed restart until nothing changes anymore
            if xchanged || ychanged then loop op1 op2 y xs true es
            else
                changed, es
            
        match xs with 
        | [] -> false, es
        | _  -> loop op1 op2 y xs false es

