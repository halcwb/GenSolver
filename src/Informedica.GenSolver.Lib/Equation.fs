namespace Informedica.GenSolver.Lib

/// Functions that handle the `Equation` type that
/// either represents a `ProductEquation` </br>
/// y = x1 \* x2 * ... \* xn </br>
/// or a `SumEquations` </br>
/// y = x1 \* x2 * ... \* xn
module Equation =

    open Informedica.GenSolver.Utils

    module ValueRange = Variable.ValueRange

    type Variable = Variable.Variable
    
    /// An equation is either a `ProductEquation`
    /// or a `Sumequation`, the first variable is the
    /// dependent variable, i.e. the result of the 
    /// equation, the second part are the independent
    /// variables in the equation
    type Equation = 
        | ProductEquation of Variable * Variable list
        | SumEquation     of Variable * Variable list

    module Exception =

        /// Error messages
        type Message = 
            | DuplicateVariables of Variable list

        /// Equation exception
        exception EquationException of Message

        /// Raise an `EquationException` with `Message` `m`.
        let raiseExc m = m |> EquationException |> raise

    /// Create an `Equation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    /// The type of Equation product or sum
    /// is determined by the constructor **c**.
    let create c succ fail (y, xs) = 
        let vars = y::xs
        match vars |> List.filter (fun v -> vars |> List.filter ((=) v) |> List.length > 1) with
        | [] -> (y, xs) |> c |> succ
        | duplicates -> duplicates |> Exception.DuplicateVariables |> fail

    /// Create an `ProductEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    let createProductEq = create ProductEquation

    /// Create an `SumEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time using the **fail** function.
    let createSumEq = create SumEquation

    /// Create an `ProductEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time raising an exception.
    let createProductEqExc = createProductEq id Exception.raiseExc 

    /// Create an `SumEquation` with an **y** and
    /// **xs**. Fails if a variable is added more
    /// than one time raising an exception.
    let createSumEqExc = createSumEq id Exception.raiseExc

    /// Apply **fp** to a `ProductEquation` and
    /// **fs** to a `SumEquation`.
    let apply fp fs = function
        | ProductEquation (y,xs) -> fp y xs
        | SumEquation (y, xs)    -> fs y xs

    /// Check whether an `Equation` is a product equation
    let isProduct = apply (fun _ _ -> true) (fun _ _ -> false)

    /// Check whether an `Equation` is a sum equation
    let isSum = apply (fun _ _ -> true) (fun _ _ -> false)

    /// Turn an `Equation` into a list of `Variable`
    let toVars = 
        let f y xs = y::xs
        apply f f

    let count e = 
        e
        |> toVars
        |> List.fold (fun acc v ->
            (v |> Variable.count) + acc
        ) 0

    let countProduct e = 
        e
        |> toVars
        |> List.fold (fun acc v ->
            let c = v |> Variable.count
            (if c = 0 then 1 else c) * acc
        ) 1


    let toString exact eq = 
        let op = if eq |> isProduct then "*" else "+"
        let varToString = Variable.toString exact

        match eq |> toVars with
        | [] -> ""
        | _::[] -> ""
        | y::xs -> 
            let s = 
                sprintf "%s = " (y |> varToString) + 
                (xs |> List.fold (fun s v -> s + (v |> varToString) + " " + op + " ") "")
            s.Substring(0, s.Length - 2)


    /// Make sure that the `Variables` in the
    /// `Equation` can only contain positive 
    /// non zero values.
    let nonZeroOrNegative e =
        let set c y xs =
            let y' = y |> Variable.setNonZeroOrNegative
            let xs' = xs |> List.map Variable.setNonZeroOrNegative
            (y', xs') |> c 
        let fp = set ProductEquation
        let fs = set SumEquation
        e |> apply fp fs

    /// Check whether an `Equation` contains
    /// a `Variable` **v**
    let contains v = toVars >> (List.exists (Variable.eqName v))

    /// Check whether `Equation`s 
    /// **eq1** and **eq2** are equal
    let equals eq1 eq2 = 
        let vrs1 = eq1 |> toVars
        let vrs2 = eq2 |> toVars
        vrs1 |> List.forall (fun vr -> 
            vrs2 |> List.exists (Variable.eqName vr)) &&
        ((eq1 |> isProduct) && (eq2 |> isProduct) ||
         (eq1 |> isSum)     && (eq2 |> isSum))

    /// Find a `Variable` **vr** in
    /// an `Equation` **eq** and return
    /// the result in a list
    let find vr eq =
        eq
        |> toVars
        |> List.filter (fun vr' -> vr' |> Variable.getName = (vr |> Variable.getName))

    /// Find a `Variable` with `Name`
    /// **n** in an `Equation` **eq**
    /// and return the result as a list
    let findName n eq =
        eq
        |> toVars
        |> List.filter (fun vr -> vr |> Variable.getName = n)

    /// Replace a `Variable` **v** in the 
    /// `Equation` **e**.
    let replace v e =
        let r c v vs =
            let vs = vs |> List.replace ((Variable.eqName) v) v
            c id (fun _ -> e) ((vs |> List.head), (vs|> List.tail))
        let fp y xs = r createProductEq v (y::xs)
        let fs y xs = r createSumEq v (y::xs)
        e |> apply fp fs


    // Check whether an equation is solved
    let isSolved = function
        | ProductEquation (y, xs) 
        | SumEquation (y, xs) ->
            [y] @ xs |> List.forall Variable.isSolved


    // Check whether an equation will change by calc
    // This is not the same as `isSolved`!! If all 
    // the variables are unrestricted than the equation
    // is not solvable but is also not solved.
    let isSolvable = function 
        | ProductEquation (y, xs)
        | SumEquation (y, xs) ->
            ([y] @ xs |> List.exists Variable.isSolvable) &&
            ([y] @ xs |> List.forall Variable.isUnrestricted |> not)


    let check e = 
        let issub op (y : Variable) (xs : Variable list) =
            xs
            |> function
            | [] -> true
            | _  ->
                if y.Values |> ValueRange.isValueSet &&
                   xs |> List.map Variable.getValueRange
                      |> List.forall ValueRange.isValueSet then
                
                    y.Values
                    |> ValueRange.isSubSetOf (xs |> List.reduce (op)).Values

                else true

        if e |> isSolvable then
            e
            |> function
            | ProductEquation (y, xs) ->
                xs 
                |> issub (*) y
            | SumEquation (y, xs) ->
                xs 
                |> issub (+) y

        else true


    /// Solve an equation **e**, return a list of
    /// changed `Variable`s. 
    /// ToDo change this to be more consistent with mutable values
    let solve calcValues log eq =
        Logger.StartSolvingEquation
        |> Logger.createMessage eq
        |> Logger.logInfo log

        let runOnce y xs =
            let c1 =
                y::xs 
                |> List.filter (Variable.getValueRange >> ValueRange.isValueSet)
                |> List.length
            let c2 =  (y::xs |> List.length)
            
            (c2 - c1 <= 1) 

        if eq |> isSolved then 
            []

        else
            let rec calc changed op1 op2 y xs rest =
                Logger.StartCalulation
                |> Logger.createMessage (y::xs)
                |> Logger.logInfo log

                match rest with 
                | []  -> 
                    Logger.FinishedCalculation
                    |> Logger.createMessage (changed, xs)
                    |> Logger.logInfo log

                    changed, xs
                | x::tail ->
                    let xs'  = xs |> List.filter ((<>) x)

                    let x' =
                        match xs' with
                        | [] -> if calcValues then x <== y else x ^<== y
                        | _  -> 
                            if calcValues then 
                                x <== (y |> op2 <| (xs' |> List.reduce op1))
                            else
                                x ^<== (y |> op2 <| (xs' |> List.reduce op1))

                    let changed = 
                        if x = x' then changed 
                        else 
                            Logger.VariableChanged
                            |> Logger.createMessage x'
                            |> Logger.logInfo log
                            
                            changed 
                            |> List.replaceOrAdd (Variable.eqName x') x'

                    tail |> calc changed op1 op2 y (x'::xs')

            let rec loop b op1 op2 y xs changed =
                let x   = xs |> List.head
                let xs' = xs |> List.filter ((<>) x)
            
                // op1 = (*) or (+) and op2 = (/) or (-)
                // Calculate y = x1 op1 x2 op1 .. op1 xn
                let ychanged, y' = calc [] op1 op1 x xs' [y]
            
                // Replace y with the new y with is in a list
                let y = y' |> List.head
            
                // Calculate x1 = y op2 (x2 op1 x3 .. op1 xn)
                //       and x2 = y op2 (x1 op1 x3 .. op1 xn)
                //       etc..
                let xchanged, xs = calc [] op1 op2 y xs xs

                // If something has changed restart until nothing changes anymore
                // or only has to run once
                match ychanged @ xchanged with
                | [] ->
                    Logger.FinishedSolvingEquation
                    |> Logger.createMessage changed
                    |> Logger.logInfo  log

                    changed
                | _  ->
                    ychanged @ xchanged
                    |> List.fold (fun acc v ->  
                        acc |> List.replaceOrAdd (Variable.eqName v) v
                    ) changed
                    |> fun changed ->
                        // only run once so now is ready
                        if b then changed
                        else
                            Logger.LoopSolvingEquation
                            |> Logger.createMessage (b, y, xs, changed)
                            |> Logger.logInfo log

                            let b = runOnce y xs
                            loop b op1 op2 y xs changed
            
            let b, y, xs, op1, op2 =
                match eq with
                | ProductEquation (y, xs) -> 
                    if calcValues then y, xs, (*), (/)
                    else y, xs, (^*), (^/)
                | SumEquation     (y, xs) -> 
                    if calcValues then y, xs, (+), (-)
                    else y, xs, (^+), (^-)
                |> fun (y, xs, op1, op2) ->
                    // run only once when all but one is a value set
                    runOnce y xs, y, xs, op1, op2
                        
            match xs with 
            | [] -> []
            | _  -> loop b op1 op2 y xs  []


    module Dto =

        type VariableDto = Variable.Dto.Dto

        /// `Dto` for an `Equation`
        type Dto = { Vars: VariableDto[]; IsProdEq: bool }

        /// `Message` type for failures
        type Message =
            | NoVarsInEquation 
            | EquationMessage of Exception.Message
            | VariableMessage of Variable.Exceptions.VariableException

        /// `DtoException type 
        exception DtoException of Message

        /// Raise a `DtoException` with `Message` **m**
        let raiseExc m = m |> DtoException |> raise

        /// Create a `Dto` with `vars` (variable dto array)
        /// that is either a `ProductEquation` or a `SumEquation`
        let create isProd vars  = { Vars = vars; IsProdEq = isProd }

        /// Create a `ProductEquation` `Dto`
        let createProd = create true

        /// Create a `SumEquation` `Dto`
        let createSum  = create false

        /// Return the `string` representation of a `Dto`
        let toString exact (dto: Dto) = 
            let op = if dto.IsProdEq then "*" else "+"
            let varToString = Variable.Dto.toString exact

            match dto.Vars |> Array.toList with
            | [] -> ""
            | _::[] -> ""
            | y::xs -> 
                let s = 
                    sprintf "%s = " (y |> varToString) + 
                    (xs |> List.fold (fun s v -> s + (v |> varToString) + " " + op + " ") "")
                s.Substring(0, s.Length - 2)

        /// Helper function to create a `Variable` from a **dto**
        let fromVarDtoExc dto = 
            try 
                dto |> Variable.Dto.fromDtoExc 
            with 
            | :? Variable.Exceptions.VariableException as m -> m |> VariableMessage |> raiseExc

        /// Create a `Dto` and raise an exception if it fails
        let fromDtoExc dto =
            let succ = id
            let fail = raiseExc

            match dto.Vars |> Array.toList with
            | [] -> NoVarsInEquation |> fail
            | y::xs ->
                let y = y |> fromVarDtoExc
                let e = (y, xs |> List.map fromVarDtoExc)
            
                if dto.IsProdEq then 
                    e 
                    |> createProductEq succ (fun m -> m |> EquationMessage |> fail)
                else 
                    e 
                    |> createSumEq succ (fun m -> m |> EquationMessage |> fail)

        /// Create a `Dto` from an `Equation` **e**
        let toDto e =
            let c isProd y xs =
                { Vars = y::xs |> List.map Variable.Dto.toDto |> List.toArray; IsProdEq = isProd }
            
            let fp = c true 
            let fs = c false 
            
            e |> apply fp fs