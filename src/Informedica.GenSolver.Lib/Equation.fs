namespace Informedica.GenSolver.Lib


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Equation =

    open Informedica.GenSolver.Utils


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


    // #region ---- SOLVE MIN MAX ----

    let optChoose x1 x2 = if x2 |> Option.isSome then x2.Value else x1


    let solveProductMinMax x1 x2 y =

        // ToDo Temp hack
        let fs = id
        let ff = fun _ -> failwith "Cannot set"

        let getMin = Variable.ValueRange.getMin
        let setMin = Variable.ValueRange.setMin fs ff
        let getMax = Variable.ValueRange.getMax
        let setMax = Variable.ValueRange.setMax fs ff


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

    let solveSumMinMax vars sum =
        
        // ToDo Temp hack
        let fs = id
        let ff = fun _ -> failwith "Cannot set"

        let getMin = Variable.ValueRange.getMin
        let setMin = Variable.ValueRange.setMin fs ff
        let getMax = Variable.ValueRange.getMax
        let setMax = Variable.ValueRange.setMax fs ff

        let sumVar getf vars  =
            vars 
            |> List.map getf 
            |> List.filter Option.isSome
            |> List.map Option.get
            |> List.fold (+) Variable.ValueRange.Value.zero

        let set cmp getf setf v var =
            match v, var |> getf with
            | Some v', Some x -> 
                if cmp v' x then var |> setf v' |> Some
                else None
            | Some v', None   -> 
                if v' > Variable.ValueRange.Value.zero then var |> setf v' |> Some
                else None
            | _ -> None
        
        let setMax = set (<) (getMax) (setMax)
        let setMin = set (>) (getMin) (setMin)
        
        // Rule 1: var.max = sum.max if var.max = not set || sum.max < var.max
        let vars = vars |> List.map (fun v ->
            v |> setMax (sum |> getMax) |> optChoose v)

        // Rule 2: var.min = sum.min - Sum(var.min) if n - 1 var.min = set
        let vars = 
            match vars |> List.filter(getMin >> Option.isNone) with
            | [var] -> 
                let min = 
                    match sum |> getMin with
                    | None -> None
                    | Some min -> min - (vars 
                                            |> List.map getMin 
                                            |> List.filter Option.isSome 
                                            |> List.map Option.get
                                            |> List.fold (+) Variable.ValueRange.Value.zero) 
                                    |> Some
                let var' = var |> setMin min |> optChoose var
                vars |> List.replace ((=) var) var'
            | _ -> vars

        // Rule 3: sum.min = Sum(var.min) if Sum(var.min) > sum.min
        let sum =
            setMin (match sumVar getMin vars with | v when v > Variable.ValueRange.Value.zero -> v |> Some | _ -> None) sum
            |> optChoose sum
            
        // Rule 4: sum.max = Sum(var.max) if all var.max = set
        let sum =
            if vars |> List.map getMax |> List.exists Option.isNone then sum
            else
                setMax (match sumVar getMax vars with | v when v > Variable.ValueRange.Value.zero -> v |> Some | _ -> None) sum |> optChoose sum
        
        sum::vars

        // #endregion


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