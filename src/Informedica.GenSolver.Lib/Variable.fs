namespace Informedica.GenSolver.Lib


open System

/// Contains functions to handle 
/// the `Variable` type and the types
/// `Variable` depends on:
///
/// * `Name`
/// * `Value`
/// * `Values`
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Variable =


    /// Funcions to handle `Name`
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Name =

        exception NullOrWhiteSpaceException


        // ---- TYPES -----

        /// Represents a non empty/null string identifying a `variable`
        type Name = Name of string


        // ---- CREATE -----

        /// Create with `succ` function when success
        /// and `fail` function when failure
        let createCont succ fail n =
            if n |> String.IsNullOrWhiteSpace then n |> fail
            else n |> Name |> succ

        /// Returns a `Name` option when creation
        /// succeeds
        let createSome = createCont Some (fun _ -> None)

        /// Create a Name that
        /// is a non empty string
        /// Note: this function will fail
        /// when string is null or white space
        let create = createCont id (fun _ -> raise NullOrWhiteSpaceException)


    /// Functions to handle `Value`
    /// A `Value` is a non zero 
    /// positive value.
    /// Basic arrhythmic operations
    /// can be performed with this type.
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Value =
        
        // ---- EXCEPTIONS ----

        exception NonZeroOrPositiveValueException of BigRational

        // ---- TYPES ----

        /// Represents a non zero positive rational number.
        type Value = Value of BigRational

        // ---- APPLY -----

        /// Apply a function `f` to value `x`.
        let apply f (Value x): 'a = f x

        // ----- CREATE ----

        /// Creates a `Value` and calls 
        /// `succ` when success and `fail` when
        /// failure.
        let createCont succ fail n =
            if n <= 0N then n |> fail
            else n |> Value |> succ

        /// Create `Value` option when
        /// success.
        let createSome = createCont Some (fun _ -> None)

        /// Create a Value that 
        /// is a non-zero positive
        /// number.
        let create = 
            let fail n = n |> NonZeroOrPositiveValueException |> raise
            createCont id fail

        /// Zero value
        let zero = 0N |> Value

        /// One value
        let one = 1N |> Value

        /// Two value
        let two = 2N |> Value

        // ---- GETTERS ----

        /// Get the `BigRational` from `value`
        let get = apply id

        // ---- CALCULATION ---- 

        /// Apply an infix operation `op` to
        /// two values `v1` and `v2`
        let calc op (Value v1) (Value v2) =
            v1 |> op <| v2 |> create 

        /// Check whether a value `v` is 
        /// an increment of `incr`.
        let isIncr (Value incr) (Value v) = 
            (v.Numerator * incr.Denominator) % (incr.Numerator * v.Denominator) = 0I

        /// Overload basic arrhythmic operations
        type Value with
            /// Multiplication
            static member (*) (v1, v2) = calc (*) v1 v2
            /// Division</br>
            /// Note that because the type cannot be zero
            /// this operation always succeds.
            static member (/) (v1, v2) = calc (/) v1 v2
            /// Addition
            static member (+) (v1, v2) = calc (+) v1 v2
            /// Subtraction </br>
            /// Note that because the result 
            /// has to be larger than zero, the
            /// operation can fail.
            static member (-) (v1, v2) = calc (-) v1 v2


    /// Functions to handle `Values`
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Values =

        open System.Collections.Generic

        exception MinLargerThanMaxException of BigRational * BigRational

        open Value

        // ---- TYPES ----

        /// `Values` is a discrete set of 
        /// non-zero positive rational numbers,
        /// the set is either limited
        /// and then it is a list or
        /// it is unlimited and then it 
        /// is a range.
        type Values =
            | ValueSet of Value Set
            | Range of Range
        /// A `Range` is an unlimited set of
        /// rational numbers, when a set has
        /// both a minimum, maximum and an 
        /// increment then it is not a range
        /// anymore but a list of values
        and Range = 
            | All
            | Incr    of Value
            | Min     of Value
            | Max     of Value
            | MinMax  of Value * Value
            | IncrMin of Value * Value

        // ---- APPLY -----

        /// Aply the give functions to `Values`
        /// where fv is used for `Value list` and
        /// fr is used for `Range`
        let apply fv fr vs : 'a =
            match vs with
            | ValueSet x -> x |> fv
            | Range x  -> x |> fr

        /// Apply given functions to `Range`.
        let applyRange all fIncr fMin fMax fMinMax fIncrMin r : 'a = 
            match r with 
            | All -> all
            | Incr incr -> incr |> fIncr
            | Min min   -> min  |> fMin
            | Max max   -> max  |> fMax
            | MinMax (min, max) -> fMinMax min max
            | IncrMin (incr, min) -> fIncrMin incr min

        // ----- CREATE ----

        let rangeAll = Range.All |> Range

        /// Convert `BigRational` list to 
        /// `Value` list
        let bigRtoValueList = List.map Value.create

        /// Small helper function to turn a sequence
        /// of `Value` to a `Vaue Set`.
        let inline seqToValueSet vs = vs |> Set.ofSeq |> ValueSet


        /// Small helper function to turn a `Value Set`
        /// to a `Value list`.
        let valueSetToList = apply Set.toList (fun _ -> [])


        let createCont succ fail  incr min max vals =
            // create range all
            let all = All   |> Range |> succ 
            // create range incr, min or max
            let range n = n |> Range |> succ
            // create range MinMax
            let minMax (min, max) =
                if min > max then (min, max)       |> fail
                else (min, max) |> MinMax |> Range |> succ
            // create range IncrMin
            let incrMin (incr, min) = (incr, min) |> IncrMin |> Range |> succ
            // create valueset from incr, max
            let incrMax (incr, max) =
                if incr > max then (incr, max) |> fail
                else 
                    let Value(incr'), Value(max') = incr, max
                    [incr'..incr'..max'] |> bigRtoValueList |> seqToValueSet |> succ
            // create valueset from incr, min, max
            let incrMinMax (incr, min, max) =
                match incr, min, max with
                | _ when min > max -> (min, max)   |> fail
                | _ when incr > max -> (incr, max) |> fail
                | _ -> 
                    let Value(min'), Value(incr'), Value(max') = min, incr, max
                    [min'..incr'..max'] |> bigRtoValueList |> seqToValueSet |> succ

            if vals |> List.isEmpty |> not then vals |> seqToValueSet |> succ
            else
                match incr, min, max with
                | None,      None,     None     -> all
                | Some incr, None,     None     -> incr |> Incr |> range
                | None,      Some min, None     -> min  |> Min  |> range
                | None,      None,     Some max -> max  |> Max  |> range
                | None,      Some min, Some max -> (min, max)   |> minMax
                | Some incr, Some min, None     -> (incr, min)  |> incrMin
                | Some incr, None,     Some max -> (incr, max)  |> incrMax
                | Some incr, Some min, Some max -> (incr, min, max) |> incrMinMax

        /// Create `Values` from either a list of
        /// `BigRational` or an incr, min, max combi
        let create = 
            let fail ((Value min), (Value max)) = raise (MinLargerThanMaxException(min, max))
            createCont id fail

        /// Create `Values` directly from a list of 
        /// `BigRational`.
        let createValues = bigRtoValueList >> (create None None None)

        /// Create a `Range` with increment `incr`,
        /// minimum `min` and maximum `max`.</br>
        /// Note that if both increment and maximum 
        /// are given a list of values is created with minimum
        /// of increment and if all arguments values then 
        /// likewise a list of values is generated, i.e. 
        /// `[min..incr..max]`
        let createRange incr min max = create incr min max []

        /// Count the number of values
        /// returns 0 when `values` is
        /// `Range`.
        let count = 
            let fv = Set.count
            let fr = fun _ -> 0
            apply fv fr

        /// Get the increment from
        /// values if there is one</br>
        /// *Note: a set of values has no increment* 
        let getIncr =
            let none = fun _ -> None
            let fMinMax = fun _ _ -> None
            let fIncrMin = fun incr _ -> Some incr

            let fr = applyRange None Some none none fMinMax fIncrMin
            apply none fr

        /// Get the minimum from
        /// values if there is one
        /// *Note when no minimum is specified 
        /// but there is an increment, then
        /// the increment is the minimum*
        let getMin =
            let none = fun _ -> None
            let fMinMax  = fun min _ -> Some min
            let fIncrMin = fun _ min -> Some min

            let fr = applyRange None Some Some none fMinMax fIncrMin
            apply (fun vs -> vs.MinimumElement |> Some ) fr

        /// Get the maximum from
        /// values if there is one
        let getMax =
            let none = fun _ -> None
            let fMinMax = fun _ max -> max |> Some
            let fIncrMin = fun _ _ -> None

            let fr = applyRange None none none Some fMinMax fIncrMin
            apply (fun vs -> vs.MaximumElement |> Some) fr

        /// Applies an infix operator
        /// to two `Values`. Only add values
        /// to the result set if > 0.
        let calc op = function
            | ValueSet s1, ValueSet s2 ->
                let s1 = new ResizeArray<_>(s1)
                let s2 = new ResizeArray<_>(s2)
                let s3 = new ResizeArray<_>()
                // Check whether the operand is subtraction
                let opIsSubtr = (Value.two |> op <| Value.one) = Value.one

                for x1 in s1 do
                    for x2 in s2 do
                        // prevent subtraction resulting in a zero or negative result
                        if opIsSubtr && x1 > x2 || (not opIsSubtr) then 
                            // perform the arrhythmic operation and to the result set
                            s3.Add(x1 |> op <| x2) 
                new HashSet<_>(s3, HashIdentity.Structural) |> seqToValueSet
            // Do not perform any calcuation when one of the args is not
            // a list of values
            | _ -> Range.All |> Range

        /// Filter a set of values according
        /// to increment, min and max constraints
        let filter incr min max = 
            let returnTrue = fun _ -> true
            let fIncr = function | None -> returnTrue | Some incr -> Value.isIncr incr
            let fMin  = function | None -> returnTrue | Some min ->  (<) min
            let fMax  = function | None -> returnTrue | Some max ->  (>) max
            let fv = Set.filter (fun v -> v |> fIncr incr &&
                                          v |> fMin min &&
                                          v |> fMax max)
                     >> seqToValueSet
            let fr = Range
            apply fv fr

        /// Function to determine how one range
        /// constraints another range.
        let constrainRangeWith incr min max = failwith "Not implemented yet"
            
        /// Set values `v2` to values `v1`. Returns
        /// the intersection of both.
        let setTo v1 v2 = 
            match v1, v2 with
            | ValueSet v1', ValueSet v2' -> v1' |> Set.intersect v2' |> ValueSet
            | Range r, ValueSet v
            | ValueSet v, Range r ->
                let vs                = v |> ValueSet
                let fAll              = vs
                let fIncr incr        = vs |> filter (Some incr) None None
                let fMin min          = vs |> filter None (Some min) None
                let fMax max          = vs |> filter None None (Some max)
                let fMinMax min max   = vs |> filter None (Some min) (Some max)
                let fIncrMin incr min = vs |> filter (Some incr) (Some min) None
                // Filter the values with r
                r |> applyRange fAll fIncr fMin fMax fMinMax fIncrMin

            | Range _, Range _ -> failwith "Not implemented yet"

        // Extend type with basic arrhythmic operations.
        type Values with
            /// Multiply 
            static member (*) (vs1, vs2) = calc (*) (vs1, vs2)
            /// Divide
            static member (/) (vs1, vs2) = calc (/) (vs1, vs2)
            /// Add
            static member (+) (vs1, vs2) = calc (+) (vs1, vs2)
            /// Subtract
            static member (-) (vs1, vs2) = calc (-) (vs1, vs2)
            /// Add `expr` to `res`
            static member (=!) (res, expr) = expr |> setTo res


    open Name
    open Values

    /// Represents a variable in an
    /// `Equation`. The variable is 
    /// identified by `Name` and has
    /// a set of possible `Values`.
    type Variable =
        {
            Name: Name
            Values: Values
        }

    /// Create a variable
    let create n vs = { Name = n; Values = vs }


