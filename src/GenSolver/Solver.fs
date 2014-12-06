namespace GenSolver

/// A Variable has a name 
/// and values
type Variable =
    {
        Name: Name
        Values: Values
    }
and Name = Name of string
/// Values is a discrete set of 
/// rational numbers, the set is 
/// either limited and then it 
/// is a list or
/// it is unlimited and then it 
/// is a range
and Values =
    | Values of Value list
    | Range of Range
and Value = Value of BigRational

/// A range is an unlimited set of
/// rational numbers, when a set has
/// both a minimum, maximum and an 
/// increment then it is not a range
/// anymore but a list of values
and Range = 
    | AnyValue
    | Increment of Increment
    | Minimum of Minimum
    | Maximum of Maximum
    | MinimumMaximum of Minimum * Maximum   
    | MinimumIncrement of Minimum * Increment
    | MaximumIncrement of Maximum * Increment
and Increment = Increment of BigRational
and Minimum = Minimum of BigRational
and Maximum = Maximum of BigRational

/// An equation is either a product equation
/// or a sumequation, the first variable is the
/// dependent variable, i.e. the result of the 
/// equation, the second part are the independent
/// variables in the equation
type Equation = 
    | ProductEquation of Variable * Variable list
    | SumEquation     of Variable * Variable list


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


module Solver =   
    
    let createSolve solveEq isSolved : Solve  =
        fun eqs ->

            let rec solveEqs restEqs accEqs  =
                match restEqs with
                | [] -> accEqs
                | eq::rest ->
                    // Get the operators to be used to solve the equation
                    let (op1, op2) =
                        match eq with
                        | ProductEquation(_, _) -> (*), (/)
                        | SumEquation(_, _)     -> (+), (-)

                    // If the equation is already solved, just put it to 
                    // the accumulated equations and go on with the rest
                    if eq |> isSolved then
                        [eq] 
                        |> List.append accEqs
                        |> solveEqs rest

                    // Else go solve the equation
                    else
                        match eq |> solveEq op1 op2 with
                        // Equation is changed, so every other equation can 
                        // be changed as well (if changed vars are in the other
                        // equations, so start new
                        | true, eq' -> 
                            solveEqs (accEqs @ [eq'] @ rest)  []
                        // Equation did not in fact change, so put it to
                        // the accumulated equations and go on with the rest
                        | false, eq' ->
                            [eq'] 
                            |> List.append accEqs
                            |> solveEqs rest

            solveEqs eqs []