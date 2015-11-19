namespace Informedica.GenSolver.Lib

/// A non empty/null string
type name = Name of string
/// A non zero positive rational number
type value = Value of BigRational

/// A range is an unlimited set of
/// rational numbers, when a set has
/// both a minimum, maximum and an 
/// increment then it is not a range
/// anymore but a list of values
type range = 
    | All
    | Increment of increment
    | Minimum of minimum
    | Maximum of maximum
    | MinimumMaximum of minimum * maximum
    | MinimumIncrement of minimum * increment
    | MaximumIncrement of maximum * increment
/// The increment of a series of values
and increment = Increment of value
/// The minimum value
and minimum = Minimum of value
/// The maximum value
and maximum = Maximum of value

/// Values is a discrete set of 
/// non-zero positive rational numbers,
/// the set is either limited
/// and then it is a list or
/// it is unlimited and then it 
/// is a range.
type values =
    | Values of value list
    | Range of range


/// A Variable has a name 
/// and values
type variable =
    {
        Name: name
        Values: values
    }


/// Create a variable
type CreateVariable = name -> values -> variable

/// Create a Name that
/// is a non empty string
type CreateName = string -> name

/// Create Values that is
/// either a list of values
/// or a range, if both Increment
/// Minimum and Maximum or an Increment
/// and a Maximum is given, the 
/// Range is automatically converted
/// to a list of Values
type CreateValues = value list option -> increment option -> minimum option -> maximum option-> values

/// Create a Value that 
/// is a non-zero positive
/// number
type CreateValue = BigRational -> value


module Variable =

    let create: CreateVariable = fun n vs -> { Name = n; Values = vs }


