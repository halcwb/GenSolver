namespace Informedica.GenSolver.Lib

module SolverLogging =
    
    open Informedica.GenUtils.Lib.BCL

    open Types
    open Types.Logging

    module Name = Variable.Name
    module ValueRange = Variable.ValueRange

    let printException = function
    | Exceptions.IncrementZeroNegativeOrEmpty vs ->
        if vs |> Set.isEmpty then "Increment has no values"
        else
            vs
            |> Set.map (BigRational.toString)
            |> String.concat ", "
            |> sprintf "Increment contains the zero or negative values: %s"
    | Exceptions.ValueRangeEmptyValueSet -> 
        "ValueRange cannot have an empty value set"
    | Exceptions.EquationEmptyVariableList -> 
        "An equation should at least contain one variable"
    | Exceptions.SolverInvalidEquations eqs ->
        eqs
        |> List.map (Equation.toString true)
        |> String.concat "\n"
        |> sprintf "The following equations are invalid\n%s"
    | Exceptions.ValueRangeMinLargerThanMax (min, max) ->
        sprintf "%A is larger than %A" min max
    | Exceptions.ValueRangeNotAValidOperator ->
        sprintf "The value range operator was invalid or unknown"
    | Exceptions.EquationDuplicateVariables vrs ->
        vrs
        |> List.map (Variable.getName >> Name.toString)
        |> String.concat (", ")
        |> sprintf "The list of variables for the equation contains duplicates:\n%s"
    | Exceptions.NameLongerThan1000 s ->
        sprintf "This name contains more than 1000 chars: %s" s
    | Exceptions.NameNullOrWhiteSpaceException ->
        sprintf "A name cannot be a blank string"
    | Exceptions.VariableCannotSetValueRange (var, vlr) ->
        sprintf "This variable:\n%s\ncannot be set with this range:%s\n"
            (var |> Variable.toString true)
            (vlr |> ValueRange.toString true)


    let printMsg = function
    | ExceptionMessage m ->
        m 
        |> printException
    | EquationCannotSolve eq -> 
        eq
        |> Equation.toString true
        |> sprintf "=== Cannot solve Equation ===\n%s" 
    | EquationStartCalulation vars -> ""
    | EquationStartSolving eq -> 
        eq
        |> Equation.toString true
        |> sprintf "=== Start solving Equation ===\n%s"
    | EquationFinishedCalculation (changed, vars) -> ""
    | EquationVariableChanged var -> ""
    | EquationFinishedSolving vars -> ""
    | EquationLoopSolving (b, var, changed, vars) -> ""
    | SolverLoopQue eqs -> ""
    | ConstraintSortOrder cs -> 
        cs
        |> List.map (fun (i, c) ->
            sprintf "%i: %A" i c
        )
        |> String.concat "\n"
        |> sprintf "=== Constraint sort order ===\n%s"
    | ConstraintVariableNotFound (c, eqs) -> ""
    | ConstraintSetLimitToVariable (l, var) -> ""
    | ConstraintApplyToVariable (c, var) -> ""
    | ConstrainedEquationsSolved (c, eqs) -> ""
    | ApiSettingVariable (var, eqs) -> ""
    | ApiEquationsSolved eqs -> ""
    | ApiAppliedConstraints (cs, eqs) -> ""



