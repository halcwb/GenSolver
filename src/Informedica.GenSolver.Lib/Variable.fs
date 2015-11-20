namespace Informedica.GenSolver.Lib

open System

/// Contains functions to handle 
/// the `variable` type and the types
/// `variable` depends on.
module Variable =

    /// Funcions to handle `name`
    module Name =

        /// Represents a non empty/null string identifying a `variable`
        type name = Name of string

        /// Create a Name that
        /// is a non empty string
        let create n = n |> Name

    /// Functions to handle `value`
    module Value =
        
        exception NonZeroOrPositiveValueException

        /// Represents a non zero positive rational number.
        type value = Value of BigRational

        /// Create a Value that 
        /// is a non-zero positive
        /// number
        let create n = 
            if n <= 0N then raise NonZeroOrPositiveValueException
            n |> Value

        /// Apply a function `f` to value `x`
        let apply f (Value x) = f x

        /// Apply an infix operation `op` to
        /// two values `v1` and `v2`
        let calc op (Value v1) (Value v2) =
            v1 |> op <| v2 |> create 

        /// Get the `BigRational` from `value`
        let getValue = apply id

        /// Filter increment
        let isIncr incr (Value v) = 
            match incr with
            | Some(Value(x)) -> (v.Numerator * x.Denominator) % (x.Numerator * v.Denominator) = 0I
            | None   -> v > 0N

        /// Filter max
        let isST max (Value v) =
            match max with
            | Some(Value(x)) -> v <= x
            | None -> true

        /// Filter min
        let isLT max (Value v) =
            match max with
            | Some(Value(x)) -> v >= x
            | None -> true

        /// Overload basic arrhythmic operations
        type value with

            static member (*) (v1, v2) = calc (*) v1 v2
            static member (/) (v1, v2) = calc (/) v1 v2
            static member (+) (v1, v2) = calc (+) v1 v2
            static member (-) (v1, v2) = calc (-) v1 v2


    /// Functions to handle `values`
    module Values =

        open System.Collections.Generic

        open Value

        /// Values is a discrete set of 
        /// non-zero positive rational numbers,
        /// the set is either limited
        /// and then it is a list or
        /// it is unlimited and then it 
        /// is a range.
        type values =
            | Values of value list
            | Range of range
        /// A range is an unlimited set of
        /// rational numbers, when a set has
        /// both a minimum, maximum and an 
        /// increment then it is not a range
        /// anymore but a list of values
        and range = 
            | All
            | Incr    of value
            | Min     of value
            | Max     of value
            | MinMax  of value * value
            | IncrMin of value * value


        /// Convert `BigRational` list to 
        /// `value` list
        let toValues = List.map Value.create

        /// Create `values` from either a list of
        /// `BigRational` or an incr, min, max combi
        let create incr min max vals =
            if vals |> List.isEmpty |> not then vals |> Values
            else
                match incr, min, max with
                | None,      None,     None     -> All                    |> Range
                | Some incr, None,     None     -> incr        |> Incr    |> Range
                | None,      Some min, None     -> min         |> Min     |> Range
                | None,      None,     Some max -> max         |> Max     |> Range
                | None,      Some min, Some max -> (min, max)  |> MinMax  |> Range
                | Some incr, Some min, None     -> (incr, min) |> IncrMin |> Range

                | Some (Value(incr)), None, Some(Value( max)) -> 
                    [incr..incr..max] |> toValues |> Values
                | Some (Value(incr)), Some(Value(min)), Some(Value( max)) -> 
                    [min..incr..max]  |> toValues |> Values

        /// Create values directly from a list of 
        /// `BigRational`.
        let createValues = toValues >> (create None None None)

        /// Create from a range with increment `incr`,
        /// minimum `min` and maximum `max`.</br>
        /// Note that if both increment and maximum 
        /// are given a list of values is created with minimum
        /// of increment and if all arguments values then 
        /// likewise a list of values is generated, i.e. 
        /// `[min..incr..max]`
        let createRange incr min max = create incr min max []

        /// Aply the give functions to `values`
        /// where fv is used for `value list` and
        /// fr is used for `range`
        let apply fv fr = function
            | Values x -> x |> fv
            | Range x  -> x |> fr

        /// Apply given functions to `range`.
        let applyRange fAll fIncr fMin fMax fMinMax fIncrMin = function 
            | All -> fAll
            | Incr incr -> incr |> fIncr
            | Min min   -> min  |> fMin
            | Max max   -> max  |> fMax
            | MinMax (min, max) -> fMinMax min max
            | IncrMin (incr, min) -> fIncrMin incr min

        /// Count the number of values
        /// returns 0 when `values` is
        /// `range`
        let count = 
            let fv = List.length
            let fr = fun _ -> 0
            apply fv fr

        /// Applies an infix operator
        /// to two `values`.
        let calc op = function
            | Values s1, Values s2 ->
                let s1 = new ResizeArray<_>(s1)
                let s2 = new ResizeArray<_>(s2)
                let s3 = new ResizeArray<_>()

                for x1 in s1 do
                    for x2 in s2 do
                        s3.Add(x1 |> op <| x2) 
                new HashSet<_>(s3, HashIdentity.Structural) |> List.ofSeq |> Values
            | _ -> range.All |> Range

        /// Get the intersection of
        /// two sequences
        let intersect v1 v2 =
            v1 |> Set.ofSeq |> Set.intersect (v2 |> Set.ofSeq)

        /// Filter a list of values according
        /// to increment, min and max constraints
        let filter incr min max = 
            let fv = List.filter (fun v -> v |> Value.isIncr incr &&
                                           v |> Value.isLT min &&
                                           v |> Value.isST max)
                     >> Values
            let fr = Range
            apply fv fr

        let constrainRangeWith incr min max = failwith "Not implemented yet"
            

        /// Set values `v2` to values `v1`. Returns
        /// the intersection of both.
        let setTo v1 v2 = 
            match v1, v2 with
            | Values v1', Values v2' -> 
                intersect v1' v2' |> Set.toList |> Values
            | Range r, Values v
            | Values v, Range r ->
                let vs = v |> Values
                let fAll = vs
                let fIncr incr = vs |> filter (Some incr) None None
                let fMin min   = vs |> filter None (Some min) None
                let fMax max   = vs |> filter None None (Some max)
                let fMinMax min max = vs |> filter None (Some min) (Some max)
                let fIncrMin incr min = vs |> filter (Some incr) (Some min) None
                // Filter the values with r
                r |> applyRange fAll fIncr fMin fMax fMinMax fIncrMin

            | Range r1, Range r2 -> failwith "Not implemented yet"


    open Name
    open Values

    /// Represents a variable in an
    /// `equation`. The variable is 
    /// identified by `Name` and has
    /// a set of possible `Values`.
    type variable =
        {
            Name: name
            Values: values
        }

    /// Create a variable
    let create n vs = { Name = n; Values = vs }


