namespace Informedica.GenSolver.Lib


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Equation =

    open Informedica.GenSolver.Utils

    module VAR = Variable
    
    /// An equation is either a product equation
    /// or a sumequation, the first variable is the
    /// dependent variable, i.e. the result of the 
    /// equation, the second part are the independent
    /// variables in the equation
    type Equation = 
        | ProductEquation of Variable.Variable * Variable.Variable list
        | SumEquation     of Variable.Variable * Variable.Variable list

    let create c fs y xs = (y, xs) |> c |> fs
        
    let createProductEq = create ProductEquation

    let createSumEq = create SumEquation

    let createProductEqSucc = createProductEq id 

    let createSumEqSucc = createSumEq id

    let apply fp fs = function
        | ProductEquation (y,xs) -> fp y xs
        | SumEquation (y, xs)    -> fs y xs

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


    let solve e =
        let rec calc changed op1 op2 y xs rest =
            match rest with 
            | []  -> changed
            | x::tail ->
                let vr = x |> VAR.getValueRange
                let xs' = xs |> List.filter (VAR.notEqual x)
                match xs' with
                | [] -> x != y
                | _  -> x != (y |> op2 <| (xs' |> List.reduce op1))
                tail |> calc (x |> VAR.hasChanged vr) op1 op2 y xs

        let y, xs, op1, op2 =
            match e with
            | ProductEquation (y, xs) -> y, xs, (*), (/)
            | SumEquation     (y, xs) -> y, xs, (+), (-)

        let rec loop op1 op2 y xs changed =
            let x   = xs |> List.head
            let xs' = xs |> List.filter (VAR.notEqual x)
            // op1 = (*) or (+) and op2 = (/) or (-)
            // Calculate y = x1 op1 x2 op1 .. op1 xn
            let ychanged = calc false op1 op1 x xs' [y]
            // Calculate x1 = y op2 (x2 op1 x3 .. op1 xn)
            //       and x2 = y op2 (x1 op1 x3 .. op1 xn)
            //       etc..
            if calc false op1 op2 y xs xs || ychanged then loop op1 op2 y xs true
            else changed
            
        match xs with 
        | [] -> false
        | _  -> loop op1 op2 y xs false

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Dto =

        open Informedica.GenSolver.Utils
        
        [<CLIMutable>]
        type Dto = { Vars: Variable.Dto.Dto[]; IsProdEq: bool }

        let create isProd vars  = { Vars = vars; IsProdEq = isProd }

        let createProd = create true
        let createSum  = create false

        let apply f (dto: Dto) = f dto
        let get = apply id

        /// If equation `eq` contains a variable 
        /// with name `n` then the property `p` of
        /// that variable is updated with value `v`. 
        let setVar n p v eq = 
            let var = 
                match (eq |> get).Vars |> Array.tryFind (fun v -> n = v.Name) with
                | Some var' -> var' |> Variable.Dto.setProp p v |> Some
                | None -> None
            { eq with 
                Vars = 
                    match var with
                    | Some var' -> 
                        eq.Vars 
                        |> Array.replace (fun v -> v.Name = n) var'
                    | None -> eq.Vars }

        let toString e = 
            let op = if (e |> get).IsProdEq then "*" else "+"
            let varToString = Variable.Dto.toString

            match e.Vars |> Array.toList with
            | [] -> ""
            | _::[] -> ""
            | y::xs -> 
                let s = 
                    sprintf "%s = " (y |> varToString) + 
                    (xs |> List.fold (fun s v -> s + (v |> varToString) + " " + op + " ") "")
                s.Substring(0, s.Length - 2)


