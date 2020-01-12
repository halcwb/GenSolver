namespace Informedica.GenSolver.Lib

/// Public funtions to use the library
module Api =

    open System

    open Informedica.GenSolver.Utils
    open Informedica.GenUtils.Lib.BCL

    module VRD = Informedica.GenSolver.Lib.Variable.Dto
    module EQD = Informedica.GenSolver.Lib.Equation.Dto
    
    module ValueRange = Variable.ValueRange 
    module Name = Variable.Name

    type Name = Variable.Name.Name


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
    let solve sortQue log lim n p eqs =

        eqs 
        |> setVariableValues true lim n p
        |> function
        | None -> eqs
        | Some vr -> 
            Logger.ApiSettingVariable
            |> Logger.createMessage vr
            |> Logger.logInfo log
                        
            eqs 
            |> Solver.solve true log sortQue vr
            |> fun eqs ->
                Logger.ApiEquationsSolved
                |> Logger.createMessage eqs
                |> Logger.logInfo log

                eqs


    /// Make a list of `EQD`
    /// to contain only positive
    /// values as solutions
    let nonZeroNegative eqs =
        eqs 
        |> List.map Equation.nonZeroOrNegative


    let solveConstraints log constrains eqs = 
        let apply = 
            Constraint.apply false log Solver.sortQue

        constrains
        |> Constraint.orderConstraints log
        |> List.fold (fun acc c ->
            acc
            |> apply c
        ) eqs
        |> fun eqs ->
            Logger.ApiAppliedConstraints
            |> Logger.createMessage (constrains, eqs)
            |> Logger.logInfo log

            eqs
        