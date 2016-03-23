namespace Informedica.GenSolver.Lib


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Equation =

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

    /// An equation is either a product equation
    /// or a sumequation, the first variable is the
    /// dependent variable, i.e. the result of the 
    /// equation, the second part are the independent
    /// variables in the equation
    type Equation = 
        | ProductEquation of Variable.Variable * Variable.Variable list
        | SumEquation     of Variable.Variable * Variable.Variable list


    /// The solve function takes in a list of 
    /// equations in which a variable can participate
    /// in one or more equations, the solve function 
    /// calculates for each variable the possible values
    /// or the possible range, given the other variables 
    /// in the equation and other equations
    type Solve = Equation list -> Equation list


    /// An equation is solved, i.e. if all the variables
    /// in the equation have only one sinlge value, i.e. 
    /// the variables can no further be restricted
    type IsSolved = Equation -> bool

    /// whether the any variable is changed, 
    /// i.e. the range of possible values has
    /// narrowed down.
    type HasChanged = Yes | No

    /// Solving an equation returns the
    /// resulting equation and whether this
    /// has changed from the original equation
    type SolveEquation = Equation -> (HasChanged * Equation)

    /// Solve a product equation
    type SolveProductEquation = SolveProductEquation of SolveEquation

    /// Solve a sum equation
    type SolveSumEquation = SolveSumEquation of SolveEquation