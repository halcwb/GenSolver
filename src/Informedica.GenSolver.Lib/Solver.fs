namespace Informedica.GenSolver.Lib

/// Contains the `Variable` type 
/// and creation and update methods for that 
/// type
module Variables =

    /// A Variable has a name 
    /// and values
    type Variable =
        {
            Name: Name
            Values: NonZeroPositiveValues
        }
    and Name = Name of string
    /// Values is a discrete set of 
    /// non-zero positive rational numbers,
    /// the set is either limited
    /// and then it is a list or
    /// it is unlimited and then it 
    /// is a range.
    and NonZeroPositiveValues =
        | Values of NonZeroPositiveValue list
        | Range of Range
    and NonZeroPositiveValue = Value of BigRational

    /// A range is an unlimited set of
    /// rational numbers, when a set has
    /// both a minimum, maximum and an 
    /// increment then it is not a range
    /// anymore but a list of values
    and Range = 
        | AnyPositiveValue
        | Increment of Increment
        | Minimum of Minimum
        | Maximum of Maximum
        | MinimumMaximum of Minimum * Maximum
        | MinimumIncrement of Minimum * Increment
        | MaximumIncrement of Maximum * Increment
    /// The increment of a series of values
    and Increment = Increment of BigRational
    /// The minimum value
    and Minimum = Minimum of BigRational
    /// The maximum value
    and Maximum = Maximum of BigRational

    /// Create a variable
    type CreateVariable = Name -> NonZeroPositiveValues -> Variable
    let createVariable: CreateVariable = fun n vs -> { Name = n; Values = vs }

    /// Create a Name that
    /// is a non empty string
    type CreateName = string -> Name
    let createName: CreateName = fun n -> n |> Name

    /// Create Values that is
    /// either a list of values
    /// or a range, if both Increment
    /// Minimum and Maximum or an Increment
    /// and a Maximum is given, the 
    /// Range is automatically converted
    /// to a list of Values
    type CreateValues = NonZeroPositiveValue list option -> Increment option -> Minimum option -> Maximum option-> NonZeroPositiveValues

    /// Create a Value that 
    /// is a non-zero positive
    /// number
    type CreateValue = BigRational -> NonZeroPositiveValue


/// Contains equation types 
/// and methods to create and
/// solve the equations
module Equations =

    open Variables

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

/// Implementations of solvers for product equations
/// sum equations and a set of product and/or sum
/// equations
module Solver =
    
    open Equations

    /// Create the equation solver using a 
    /// product equation and a sum equation solver
    /// and function to determine whether an 
    /// equation is solved
    let createSolve (SolveProductEquation solveProd) 
                    (SolveSumEquation solveSum) 
                    isSolved : Solve  =
        fun eqs ->

            let rec solveEqs restEqs accEqs  =
                match restEqs with
                | [] -> accEqs
                | eq::rest ->
                    // If the equation is already solved, just put it to 
                    // the accumulated equations and go on with the rest
                    if eq |> isSolved then
                        [eq] 
                        |> List.append accEqs
                        |> solveEqs rest

                    // Else go solve the equation
                    else
                        // Pick the right solve function
                        let solveEq =
                            match eq with
                            | ProductEquation(_) -> fun _ -> eq |> solveProd
                            | SumEquation(_)     -> fun _ -> eq |> solveSum

                        match solveEq ()  with
                        // Equation is changed, so every other equation can 
                        // be changed as well (if changed vars are in the other
                        // equations, so start new
                        | Yes, eq' -> 
                            solveEqs (accEqs @ [eq'] @ rest)  []
                        // Equation did not in fact change, so put it to
                        // the accumulated equations and go on with the rest
                        | No, eq' ->
                            [eq'] 
                            |> List.append accEqs
                            |> solveEqs rest

            solveEqs eqs []