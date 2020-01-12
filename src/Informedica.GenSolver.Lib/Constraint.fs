namespace Informedica.GenSolver.Lib


module Props =

    open MathNet.Numerics

    module ValueRange = Variable.ValueRange

    type Property =
        | Vals of BigRational Set
        | Increment of BigRational Set
        | MinIncl of BigRational
        | MinExcl of BigRational
        | MaxIncl of BigRational
        | MaxExcl of BigRational

    let matchProp p =

        match p with
        | Vals vs -> 
            vs
            |> ValueRange.ValueSet
        | _ ->
            match p with
            | Vals _ -> "all ready matched" |> failwith
            | Increment vs -> vs |> ValueRange.createMinIncrRange
            | MinIncl v -> v |> ValueRange.createMinRange true 
            | MinExcl v -> v |> ValueRange.createMinRange false 
            | MaxIncl v -> v |> ValueRange.createMaxRange true 
            | MaxExcl v -> v |> ValueRange.createMaxRange false 
            |> ValueRange.Range


    let getMin = function
    | MinIncl v | MinExcl v -> v |> Some
    | _ -> None


    let getMax = function
    | MaxIncl v | MaxExcl v -> v |> Some
    | _ -> None

    let getIncr = function
    | Increment vs -> vs |> Some
    | _ -> None



module Constraint =

    module ValueRange = Variable.ValueRange

    type Property = Props.Property
    type Name = Variable.Name.Name

    type Limit = 
        | MinLim of int
        | MaxLim of int
        | NoLimit

    type Constraint =
        {
            Name : Name
            Property : Property
            Limit : Limit
        }

    let eqsName (c1 : Constraint) (c2 : Constraint) = c1.Name = c2.Name  


    let scoreConstraint c =
            match c.Property with
            | Property.Vals vs -> 
                let n = vs |> Set.count
                if n = 1 then -3, c
                else n, c
            | Property.MinIncl _
            | Property.MinExcl _   -> -5, c
            | Property.Increment _ -> -4, c
            | _                    -> -2, c


    let orderConstraints log cs =
        cs
        |> List.fold (fun acc c ->
            match c.Property with
            | Property.Vals vs ->
                let min = vs |> Set.minElement |> Property.MinIncl
                let max = vs |> Set.maxElement |> Property.MaxIncl
                [
                    c
                    { c with Property = min }
                    { c with Property = max }
                ]
                |> List.append acc
            | _ -> [c] |> List.append acc
        ) []
        |> fun cs -> cs |> List.map scoreConstraint
        |> List.sortBy fst
        |> fun cs ->
            Logger.ConstraintsOrder
            |> Logger.createMessage cs
            |> Logger.logInfo log

            cs
            |> List.map snd 


    let apply calc log sortQue (c : Constraint) eqs =

        let lim l b vr =
            if vr |> Variable.count <= l then vr
            else
                vr
                |> Variable.getValueRange
                |> ValueRange.getValueSet
                |> function
                | Some vs ->
                    vs
                    |> Set.toList
                    |> fun xs -> 
                        if b then xs |> List.sort 
                        else xs |> List.sortDescending
                    |> List.take l
                    |> Set.ofList
                    |> ValueRange.createValueSet
                    |> Variable.setValueRange calc vr
                | None -> vr

        eqs 
        |> List.collect (Equation.findName c.Name)
        |> function
        | [] -> 
            Logger.ConstraintVariableNotFound
            |> Logger.createMessage c
            |> Logger.logWarning log

            None

        | vr::_ ->

            c.Property
            |> Props.matchProp
            |> Variable.setValueRange calc vr
            |> fun vr ->
                match c.Limit with
                | NoLimit -> vr
                | MaxLim l -> 
                    Logger.ConstraintSettingLimit
                    |> Logger.createMessage c.Limit
                    |> Logger.logInfo log

                    vr |> lim l false  
                | MinLim l -> 
                    Logger.ConstraintSettingLimit
                    |> Logger.createMessage c.Limit
                    |> Logger.logInfo log

                    vr |> lim l true
            |> Some
        |> function
        | None -> eqs
        | Some vr ->
        
            eqs 
            |> Solver.solve calc log sortQue vr
            |> fun eqs ->
                Logger.ConstrainedEquationsSolved
                |> Logger.createMessage eqs
                |> Logger.logInfo log

                eqs

