﻿namespace Informedica.GenSolver.Lib

/// Public funtions to use the library
module Api =

    open System
    open Informedica.GenSolver.Utils
    open Informedica.GenUtils.Lib.BCL

    module VRD = Informedica.GenSolver.Lib.Variable.Dto
    module EQD = Informedica.GenSolver.Lib.Equation.Dto
    
    module ValueRange = Variable.ValueRange 
    module Name = Variable.Name

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
        |> List.map EQD.toDto
        |> List.iteri (fun i dto ->
            dto
            |> EQD.toString exact
            |> sprintf "%i.\t%s" i
            |> pf
        )
        "-----" |> pf 

        eqs    


    let setVariableValues lim n p vs eqs =

        eqs 
        |> List.collect (fun e -> e |> Equation.findName (n |> Variable.Name.createExc))
        |> function
        | [] -> None

        | vr::_ ->

            match p with
            | VRD.Vals -> 
                vs
                |> Set.ofList
                |> ValueRange.ValueSet
            | _ ->
                match vs with
                | [v] ->
                    match p with
                    | VRD.Vals -> "already matched" |> failwith
                    | VRD.Incr -> 
                        "not supported yet" |> failwith
                    | VRD.MinIncl -> v |> ValueRange.minRange true 
                    | VRD.MinExcl -> v |> ValueRange.minRange true 
                    | VRD.MaxIncl -> v |> ValueRange.maxRange true 
                    | VRD.MaxExcl -> v |> ValueRange.maxRange true 
                    | VRD.NoProp ->
                        p
                        |> sprintf "property %s is not supported"
                        |> failwith
                    |> ValueRange.Range
                | _ -> 
                    p
                    |> sprintf "setting of multiple values is not supported for this prop: %s"
                    |> failwith

            |> Variable.setValueRange vr
            |> fun vr ->
                match lim with
                | Some l ->
                    if vr |> Variable.count > l then
                        vr
                        |> Variable.getValueRange
                        |> ValueRange.getValueSet
                        |> Set.toList
                        |> List.sort
                        |> List.take l
                        |> Set.ofList
                        |> ValueRange.createValueSet
                        |> Variable.setValueRange vr

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
    let solve sortQue log exact lim n p vs eqs =

        eqs 
        |> setVariableValues lim n p vs
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
            |> Solver.solve log sortQue vr
            |> printEqs exact log


    /// Make a list of `EQD`
    /// to contain only positive
    /// values as solutions
    let nonZeroNegative eqs =
        eqs 
        |> List.map Equation.nonZeroOrNegative


