namespace Informedica.GenSolver

/// Public funtions to use the library
module Api =

    open System
    open Informedica.GenSolver.Utils

    module VD = Informedica.GenSolver.Lib.Dtos.Variable
    module VR = Informedica.GenSolver.Lib.Variable  
    module ED = Informedica.GenSolver.Lib.Dtos.Equation
    module SV = Informedica.GenSolver.Lib.Solver
    module EQ = Informedica.GenSolver.Lib.Equation

    /// Initialize the solver returning a set of equations
    let init eqs = 
        let notempty = String.IsNullOrWhiteSpace >> not
        let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
        let createProdEqs = List.map (ED.createProd >> ED.fromDtoExc)
        let createSumEqs  = List.map (ED.createSum  >> ED.fromDtoExc)

        let parse eqs op = 
            eqs 
            |> List.map (String.splitAt '=')
            |> List.map (Array.collect (String.splitAt op))
            |> List.map (Array.map String.trim)
            |> List.map (Array.filter notempty)
            |> List.map (Array.map VD.createNew)
            
        (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)


    /// Format a set of equations to print.
    /// Using **f** to allow additional processing
    /// of the string.
    let printEqs f eqs = 
        let eqs = 
            eqs 
            |> List.sortBy (fun e ->
                e 
                |> EQ.toVars
                |> List.head
                |> VR.getName)

        for e in eqs |> List.map ED.toDto do 
            sprintf "%s" (e |> ED.toString) |> f
        "-----" |> f 
        eqs    

    /// Solve an `Equations` list with
    ///
    /// * f: function used to process string message
    /// * n: the name of the variable to be updated
    /// * p: the property of the variable to be updated
    /// * vs: the values to update the property of the variable
    /// * eqs: the list of equations to solve
    let solve f n p vs eqs =
        let vrs = 
            eqs 
            |> List.collect (fun e -> e |> EQ.findName (n |> VR.Name.createExc))
        
        match vrs with
        | vr::_ ->
            sprintf "Setting variable %s %s with %s" n p (vs |> List.map BigRational.toString |> String.concat ", ") |> f

            let vr' = 
                vr
                |> VD.toDto
                |> VD.setProp p vs
                |> VD.fromDtoExc

            eqs 
            |> SV.solve vr'
            |> printEqs f
        | _ -> eqs

    /// Make a list of `Equation`
    /// to contain only positive
    /// values as solutions
    let nonZeroNegative eqs =
        eqs 
        |> List.map EQ.nonZeroOrNegative


