namespace Informedica.GenSolver.Lib

/// Public funtions to use the library
module Api =

    open System
    open MathNet.Numerics

    open Informedica.GenSolver.Utils
    open Informedica.GenUtils.Lib.BCL

    module VRD = Informedica.GenSolver.Lib.Variable.Dto
    module EQD = Informedica.GenSolver.Lib.Equation.Dto
    
    module ValueRange = Variable.ValueRange 
    module Name = Variable.Name

    type Name = Name.Name

    module Props =

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


    open Props

    /// Initialize the solver returning a set of equations
    let init eqs = 
        let notempty = String.IsNullOrWhiteSpace >> not
        let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
        let createProdEqs = List.map (EQD.createProd >> EQD.fromDtoExc)
        let createSumEqs  = List.map (EQD.createSum  >> EQD.fromDtoExc)

        let parse eqs op = 
            eqs 
            |> List.map (String.splitAt '=')
            |> List.map (Array.collect (String.splitAt op))
            |> List.map (Array.map String.trim)
            |> List.map (Array.filter notempty)
            |> List.map (Array.map VRD.createNew)
        
        (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)



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
        |> List.map (Equation.toString exact)
        |> List.iteri (fun i s ->
            s
            |> sprintf "%i.\t%s" i
            |> pf
        )
        "-----" |> pf 

        eqs    


    let setVariableValues calc lim n p eqs =

        eqs 
        |> List.collect (Equation.findName n)
        |> function
        | [] -> None

        | vr::_ ->

            p
            |> Props.matchProp
            |> Variable.setValueRange calc vr
            |> fun vr ->
                match lim with
                | Some l ->
                    if vr |> Variable.count > l then
                        vr
                        |> Variable.getValueRange
                        |> ValueRange.getValueSet
                        |> function
                        | Some vs -> 
                            vs 
                            |> Seq.sort 
                            |> Seq.take l 
                            |> ValueRange.createValueSet
                            |> Variable.setValueRange calc vr
                        | None -> vr

                    else vr
                | None -> vr
                |> Some




    /// Solve an `Equations` list with
    ///
    /// * f: function used to process string message
    /// * n: the name of the variable to be updated
    /// * p: the property of the variable to be updated
    /// * vs: the values to update the property of the variable
    /// * eqs: the list of equations to solve
    let solve sortQue log exact lim n p eqs =

        eqs 
        |> setVariableValues true lim n p
        |> function
        | None -> eqs
        | Some vr -> 
            vr 
            |> Variable.count 
            |> sprintf "setting %s with %i values" (vr |> Variable.getName |> Name.toString)
            |> log

            sprintf "equations after setting\n" |> log
            eqs
            |> Solver.replace [vr]
            |> function 
            | (rpl, rst) ->
                rpl @ rst
                |> printEqs exact log
                |> ignore
                        
            eqs 
            |> Solver.solve true log sortQue vr
            |> printEqs exact log


    /// Make a list of `EQD`
    /// to contain only positive
    /// values as solutions
    let nonZeroNegative eqs =
        eqs 
        |> List.map Equation.nonZeroOrNegative


    module Constraint =

        type Property = Props.Property

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


        let scoreConstraint c cs =
            let score c =
                match c.Property with
                | Vals vs -> // vs |> Set.count, c
                    let n = vs |> Set.count
                    if n = 1 then -3, c
                    else n, c
                | MinIncl _
                | MinExcl _   -> -5, c
                | Increment _ -> -4, c
                | _ ->           -2, c

            c |> score

            //match cs 
            //      |> List.tryFind (fun c' ->
            //        c' |> eqsName c &&
            //        c'.Property |> (Props.getIncr >> Option.isSome)) with
            //| Some c' ->
            //    match c.Property, c'.Property with
            //    | MaxIncl max, Increment vs
            //    | MaxExcl max, Increment vs ->
            //        let min = vs  |> Set.minElement
            //        let n = 
            //            try ((max - min) / min) |> BigRational.ToInt32 with |_ -> Int32.MaxValue
            //        n, c 
            //    | _ ->  c |> score
            //| None ->   c |> score


        let orderConstraints log cs =
            cs
            |> List.fold (fun acc c ->
                match c.Property with
                | Vals vs ->
                    let min = vs |> Set.minElement |> MinIncl
                    let max = vs |> Set.maxElement |> MaxIncl
                    [
                        c
                        { c with Property = min }
                        { c with Property = max }
                    ]
                    |> List.append acc
                | _ -> [c] |> List.append acc
            ) []
            |> fun cs -> cs |> List.map (fun c -> cs |> scoreConstraint c)
            |> List.sortBy fst
            |> fun cs ->
                "Going to apply constraints in the following order:\n"
                |> log

                cs
                |> List.mapi (fun i c -> 
                    let s, c = c
                    sprintf "%i. Score: %i -- %A: %A" i s c.Name c.Property
                    |> log

                    c
                )


        let apply calc log exact sortQue (c : Constraint) eqs =

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
                c.Name
                |> sprintf "no variable with %A could be found"
                |> log
                None

            | vr::_ ->

                c.Property
                |> matchProp
                |> Variable.setValueRange calc vr
                |> fun vr ->
                    match c.Limit with
                    | NoLimit -> vr
                    | MaxLim l -> sprintf "setting limit to max: %A" l |> log;  vr |> lim l false  
                    | MinLim l -> sprintf "setting limit to min: %A" l |> log;  vr |> lim l true
                |> Some
            |> function
            | None -> eqs
            | Some vr ->
                sprintf "equations after setting\n" |> log
                eqs
                |> Solver.replace [vr]
                |> function 
                | (rpl, rst) ->
                    rpl @ rst
                    |> printEqs exact log
                    |> ignore
            
                eqs 
                |> Solver.solve calc log sortQue vr
                |> printEqs exact log


    let solveConstraints log exact constrains eqs = 
        let apply = 
            Constraint.apply false log exact Solver.sortQue

        constrains
        |> Constraint.orderConstraints log
        |> List.fold (fun acc c ->
            sprintf "applying constraint: %A with %A" c.Name c.Property
            |> log

            acc
            |> apply c
        ) eqs
        