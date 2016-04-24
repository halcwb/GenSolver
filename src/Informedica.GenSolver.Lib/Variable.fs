namespace Informedica.GenSolver.Lib

open System

open Informedica.GenSolver.Utils

/// Contains functions to handle 
/// the `Variable` type and the types
/// `Variable` depends on:
///
/// * `Name`
/// * `Value`
/// * `Range`
/// * `ValueRange`
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Variable =


    /// Funcions to handle `Name`
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Name =

        // #region ---- TYPES -----

        /// Represents a non empty/null string identifying a `Variable`.
        /// `Name` can be no longer than 30 characters.
        type Name = Name of string

        // #endregion

        // #region ---- EXCEPTIONS ----

        /// Error messages for type `Name`.
        type Message = 
            | NullOrWhiteSpaceException
            | LongerThan30 of int

        /// Exception type for `Name`.
        exception NameException of Message

        /// Raise an exception with `m`.
        let raiseExc msg = msg |> NameException |> raise

        // #endregion

        // #region ---- CREATE -----

        /// Create with `succ` function when success
        /// and `fail` function when failure
        let create succ fail n =
            if n |> String.IsNullOrWhiteSpace then NullOrWhiteSpaceException |> fail
            else 
                match n |> String.trim with
                | n' when n' |> String.length <= 30 -> n' |> Name |> succ
                | n' -> n' |> String.length |> LongerThan30 |> fail

        /// Returns a `Name` option if creation
        /// succeeds else `None`.
        let createOpt = create Some Option.none

        /// Create a Name that
        /// is a non empty string
        /// Note: this function will fail
        /// when string is null or white space
        let createExc = create id raiseExc

        // #endregion

    /// Functions to handle `ValueRange`.
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ValueRange =

        open System.Collections.Generic
        open Informedica.GenSolver.Utils

        /// Functions to handle `Value`
        /// A `Value` is a non zero 
        /// positive value implemented as a BigRational.
        /// Basic arrhythmic operations
        /// can be performed with this type.
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Value =
        
            // #region ---- TYPES ----

            /// Represents a non zero positive rational number.
            type Value = Value of BigRational

            // #endregion

            // #region ---- EXCEPTIONS ----

            /// Error messages for `Value`.
            type Message = 
                | ZeroOrNegativeValue of BigRational
                | StringCannotBeParsedToValue of string
                | InvalidOperatorForValue

            /// 'Value` exception.
            exception ValueException of Message

            /// Raise an exception with message `m`.
            let raiseExc msg = msg |> ValueException |> raise

            // #endregion

            // #region ----- CREATORS ----

            /// Creates a `Value` and calls 
            /// `succ` when success and `fail` when
            /// failure.
            let create succ fail n =
                if n <= 0N then n |> ZeroOrNegativeValue |> fail
                else n |> Value |> succ

            let createOption = create Some Option.none

            /// Create function that will raise
            /// a `ZeroOrNegativeValueException`
            let createExc = create id raiseExc

            /// One value
            let one = 1N |> Value

            /// Two value
            let two = 2N |> Value

            /// Three value
            let three = 3N |> Value

            // #endregion

            // #region ---- UTILS -----

            /// Apply a function `f` to value `x`.
            let apply f (Value x): 'a = f x

            /// Convert a `Value` to a string
            let toString (Value v) = v |> BigRational.toString

            /// Convert a `string` to a `Value`.
            /// If this fails use the `fail` function, 
            /// else use the success function.
            let fromString succ fail s = 
                match s |> BigRational.tryParse with
                | Some br -> br |> create succ fail
                | None -> s |> StringCannotBeParsedToValue |> fail                    

            /// Convert an optional `Value` to a `string`.
            /// If `None` then return empty `string`.
            let optToString = function
                | Some v' -> v' |> toString
                | None    -> ""

            // #endregion

            // #region ---- GETTERS ----

            /// Get the `BigRational` from `value`
            
            let get = apply id

            // #endregion

            // #region ---- CALCULATION ---- 

            /// Apply an infix operation `op` to
            /// two values `v1` and `v2` using a 
            /// `succ` and `fail` functions.
            let calc succ fail op (Value v1) (Value v2) =
                v1 |> op <| v2 |> create succ fail

            /// Check whether a value `v` is 
            /// an increment of `incr`.
            let isMultiple (Value incr) (Value v) = 
                (v.Numerator * incr.Denominator) % (incr.Numerator * v.Denominator) = 0I

            /// Determine the greatest common divisor 
            /// of two values.
            let gcd (Value v1) (Value v2) = BigRational.gcd v1 v2 |> Value

            type Value with
                
                static member (*) (v1, v2) = calc id raiseExc (*) v1 v2

                static member (/) (v1, v2) = calc id raiseExc (/) v1 v2

                static member (+) (v1, v2) = calc id raiseExc (+) v1 v2

                static member (-) (v1, v2) = calc id raiseExc (-) v1 v2

            /// Check whether the operator is subtraction
            let opIsSubtr op = (three |> op <| two) = three - two // = 1

            /// Check whether the operator is addition
            let opIsAdd op   = (three |> op <| two) = three + two // = 5

            /// Check whether the operator is multiplication
            let opIsMult op  = (three |> op <| two) = three * two // = 6

            /// Check whether the operator is divsion
            let opIsDiv op   = (three |> op <| two) = three / two // = 3/2

            let (|Mult|Div|Add|Subtr|NoOp|) op =
                match op with
                | _ when op |> opIsMult  -> Mult
                | _ when op |> opIsDiv   -> Div
                | _ when op |> opIsAdd   -> Add
                | _ when op |> opIsSubtr -> Subtr
                | _ -> NoOp

            // #endregion

        module V = Value

        // #region ---- TYPES ----
           
        /// The minimal `Value` in 
        /// a `Range`. Can be inclusive
        /// or exclusive.
        type Minimum =
            | MinIncl of V.Value
            | MinExcl of V.Value

        /// The maximum `Value` in 
        /// a `Range`. Can be inclusive
        /// or exclusive.
        type Maximum =
            | MaxIncl of V.Value
            | MaxExcl of V.Value

        /// `ValueRange` represents a discrete set of 
        /// non-zero positive rational numbers.
        /// A `ValueRange` is either unrestricted,
        /// a finite set of values or a `Range`.
        type ValueRange =
            | Unrestricted
            | ValueSet of V.Value Set
            | Range of Range
        /// A range is restricted by either a 
        /// `Minimum`, a `Maximum`, a `Minimum` 
        /// and a increment of `Minimum` and 
        /// `Maximum`.
        and Range =
            | Min of Minimum
            | Max of Maximum
            | MinIncr of Minimum * V.Value
            | MinMax  of Minimum * Maximum

        // #endregion

        // #region --- EXCEPTIONS ---

        /// Failure messages.
        type Message =
            | MinLargerThanMax of Minimum * Maximum
            | IncrementLargerThanMax of V.Value * Maximum
             
        /// `ValueRange` exception type
        exception ValueRangeException of Message

        /// Raise an exception with message `m`.
        let raiseExc m = m |> ValueRangeException |> raise

        // #endregion
        
        // #region ---- UTILS -----

        /// Aply the give functions to `Values`
        /// where `unr` is used for an unrestricted 
        /// `ValueRange`, fv is used for `ValueSet` and
        /// fr is used for `Range`
        let apply unr fValueSet fRange = function
            | Unrestricted        -> unr
            | ValueSet vs         -> vs |> fValueSet
            | Range r             -> r  |> fRange

        /// Aply the give functions to `Range`
        /// where `fMin` is used for a range with only
        /// a `Minimum`, `fMax` is used for maximum `fMinIncr` 
        /// is used for `MinIncr` and `fMinMax` for `MinMax`. 
        let applyRange fMin fMax fMinIncr fMinMax = function
            | Min m               -> m |> fMin
            | Max m               -> m |> fMax
            | MinIncr (min, incr) -> (min, incr) |> fMinIncr
            | MinMax (min, max)   -> (min, max)  |> fMinMax

        /// Count the number of values in a `ValueRange`.
        /// Returns 0 if not a `ValueSet`.
        let count = 
            let zero _ = 0
            apply 0 (fun s -> s |> Set.count) zero

        /// Checks whether a `ValueRange` is `Unrestricted`
        let isUnrestricted = 
            let false' _ = false
            apply true false' false' 

        /// Checks whether a `ValueRange` is a `ValueSet`
        let isValueSet =
            let false' _ = false
            apply false (fun _ -> true) false'           

        /// Checks whether a `ValueRange` is a `Range`
        let isRange =
            let false' _ = false
            apply false false' (fun _ -> true)           

        /// Checks whether a `ValueRange` is `ValueSet`
        /// and doesn't contain any `Value`.
        let isEmpty vr = (vr |> count = 0) && (vr |> isValueSet)

        /// Checks whether a `Value` is between a 'min'
        /// a `max` and a multiple of `incr`. When min, 
        /// incr and max are `None` then this will return
        /// `true` for any `Value`.
        let isBetween min incr max v =
            let fTrue = fun _ -> true

            let fMin  = function | None -> fTrue  | Some(MinIncl m) -> (<=) m | Some(MinExcl m) -> (<) m
            let fMax  = function | None -> fTrue  | Some(MaxIncl m) -> (>=) m | Some(MaxExcl m) -> (>) m

            let fIncr = function | None -> fTrue  | Some(incr) -> V.isMultiple incr 

            v |> fIncr incr &&
            v |> fMin min &&
            v |> fMax max

        /// Checks whether minimum `m1` > `m2`
        let minLTmin m1 m2 = 
            match m2, m1 with
            | MinIncl m2', MinIncl m1' 
            | MinExcl m2', MinExcl m1' 
            | MinIncl m2', MinExcl m1' -> m2' > m1' 
            | MinExcl m2', MinIncl m1' -> m2' >= m1'

        /// Checks whether minimum `m1` <= `m2`
        let minSTEmin m1 m2 = m2 |> minLTmin m1 |> not

        /// Checks whether maximum `m1` > `m2`
        let maxLTmax m1 m2 = 
            match m2, m1 with
            | MaxIncl m2', MaxIncl m1' 
            | MaxExcl m2', MaxExcl m1' 
            | MaxExcl m2', MaxIncl m1' -> m2' > m1'
            | MaxIncl m2', MaxExcl m1' -> m2' >= m1' 

        /// Checks whether maximum `m1` <= `m2`
        let maxSTEmax m1 m2 = m2 |> maxLTmax m1 |> not

        /// Checks whether minimum `min` > maximum `max`
        let minLTmax max min =
            match min, max with
            | MinIncl min', MaxIncl max' -> min' > max'
            | MinExcl min', MaxIncl max' 
            | MinExcl min', MaxExcl max' 
            | MinIncl min', MaxExcl max' -> min' >= max' 

        /// Checks whether minimum `min` <= maximum `max`
        let minSTEmax max min = min |> minLTmax max |> not

        /// Checks whether minimum `min` = maximum `max`
        let minEQmax max min = 
            match min, max with
            | MinIncl min', MaxIncl max' -> min' = max'
            | _ -> false

        /// Filter a set of values according
        /// to increment, min and max constraints
        let filter min incr max = Set.filter (fun v -> v |> isBetween min incr max)

        /// Get the minimal `Value` in a `Value Set`. Returns `None` if an empty set.
        let getSetMin s = if s |> Set.isEmpty then None else s.MinimumElement |> MinIncl |> Some

        /// Get the maximum `Value` in a `Value Set`. Returns `None` if an empty set.
        let getSetMax s = if s |> Set.isEmpty then None else s.MaximumElement |> MaxIncl |> Some

        /// Convert a `Minimum` to a `Value`.
        let minToValue = function | MinIncl v | MinExcl v -> v
        
        /// Convert a `Maximum` to a `Value`.
        let maxToValue = function | MaxIncl v | MaxExcl v -> v    

        /// Checks whether minimum is exclusive.
        let isMinExcl = function | MinIncl _ -> false | MinExcl _ -> true

        /// Checks whether minimum is inclusive.
        let isMinIncl = isMinExcl >> not

        /// Checks whether maximum is exclusive.
        let isMaxExcl = function | MaxIncl _ -> false | MaxExcl _ -> true

        /// Checks whether maximum is inclusive.
        let isMaxIncl = isMaxExcl >> not
        
        // Calculate minimum as a multiple of incr
        let calcMin min incr =
            let n = match min with | MinIncl m | MinExcl m -> m |> V.get
            let (V.Value(d)) = incr
            let n' = n |> BigRational.toMultipleOf d
            if min |> isMinExcl && n' <= n then n' + d else n'

        /// Create a set of values using `min`, `incr` and a `max`.
        let minIncrMaxToValueSet min incr max =
            let min' = calcMin min incr
            let incr' = incr |> V.get
            let max' = match max with | MaxIncl m | MaxExcl m -> m |> V.get

            let vs = [min'..incr'..max'] |> Set.ofList
            // Take of the maximimum value if it equals to maximum when maximum is exclusive
            if vs |> Set.isEmpty |> not && 
               max |> isMaxExcl &&
               max' <= vs.MaximumElement then vs.Remove(vs.MaximumElement)
            else 
                vs
            |> Set.map V.createExc
            |> ValueSet
            
        /// Create a string (to print).
        let print unr vals min minincl incr max maxincl = 
            let printRange min incr max =
                let left  = if minincl then "[" else "<"
                let right = if maxincl then "]" else ">"

                match min, incr, max with
                | Some min, None, None      -> sprintf "%s%s..>" left min
                | Some min, None, Some max  -> sprintf "%s%s..%s%s" left min max right
                | None,     None, Some max  -> sprintf "<0..%s%s" max right
                | Some min, Some incr, None -> sprintf "%s%s..%s..>" left min incr 
                | _ -> "[Not a valid range]"

            let printVals vals =
                if unr then "<0..>"
                else
                    "[" + (vals |> Array.fold (fun s v -> if s = "" then v else s + ", " + v) "") + "]"

            let vals = 
                if min = "" && incr = "" && max = "" then vals |> printVals
                else
                    let min  = if min = ""  then None else Some min
                    let incr = if incr = "" then None else Some incr
                    let max  = if max = ""  then None else Some max
                    printRange min incr max

            sprintf "%s" vals

        /// Convert a `ValueRang` to a `string`.
        let toString vr =
            let fVs  vs = 
                let vs = vs |> Seq.map V.toString |> Seq.toArray
                (false, vs, "", false, "", "", false)

            let fMin min =
                let min, minincl = 
                    match min with
                    | MinIncl v -> v |> V.toString, true
                    | MinExcl v -> v |> V.toString ,false  
                (false, [||], min, minincl, "", "", false)

            let fMax max =
                let max, maxincl = 
                    match max with
                    | MaxIncl v -> v |> V.toString, true
                    | MaxExcl v -> v |> V.toString ,false  

                (false, [||], "", false, "", max, maxincl)

            let fMinIncr (min, incr)  = 
                let min, minincl = 
                    match min with
                    | MinIncl v -> v |> V.toString, true
                    | MinExcl v -> v |> V.toString ,false  

                let incr = incr |> V.toString
                
                (false, [||], min, minincl, incr, "", false)

            let fMinMax (min, max) =
                let min, minincl = 
                    match min with
                    | MinIncl v -> v |> V.toString, true
                    | MinExcl v -> v |> V.toString ,false  

                let max, maxincl = 
                    match max with
                    | MaxIncl v -> v |> V.toString, true
                    | MaxExcl v -> v |> V.toString ,false  

                (false, [||], min, minincl, "", max, maxincl)

            let (unr, vs, min, minincl, incr, max, maxinl) =  
                match vr with
                | Unrestricted        -> (true, [||], "", false, "", "", false)
                | ValueSet vs         -> vs |> fVs
                | Range r ->
                    match r with
                    | Min min             -> min  |> fMin
                    | Max max             -> max  |> fMax
                    | MinIncr (min, incr) -> (min, incr) |> fMinIncr
                    | MinMax (min, max)   -> (min, max) |> fMinMax
                            
            print unr vs min minincl incr max maxinl

        // #endregion

        // #region ----- CREATORS ----
            
        /// An unrestricted `ValueRange`.
        let unrestricted = Unrestricted

        /// An empty `ValueSet`.
        let empty = Set.empty |> ValueSet

        /// Create a 'ValueSet`.
        let createValueSet = ValueSet

        /// Create a `Minimum` that is 
        /// either inclusive or exclusive.
        let createMin isExcl m = if isExcl then m |> MinExcl else m |> MinIncl

        /// Create a `Maximum` that is 
        /// either inclusive or exclusive.
        let createMax isExcl m = if isExcl then m |> MaxExcl else m |> MaxIncl

        /// Create a `Minimum` `Range` that is 
        /// either inclusive or exclusive.
        let minRange isExcl m = m |> createMin isExcl |> Min 

        /// Create a `Maximum` `Range` that is 
        /// either inclusive or exclusive.
        let maxRange isExcl m = m |> createMax isExcl |> Max

        /// Create a `MinIncr` `ValueRange`.
        let minIncrValueRange min incr =
            let min' = calcMin min incr |> V.createExc |> MinIncl
            (min', incr) |> MinIncr |> Range
            
        /// Create a `MinMax` `ValueRang`, if min > max
        /// pass message to `fail` else pass
        /// to `succ`.
        let minMaxValueRange succ fail min max = 
            if min |> minLTmax max then (min, max) |> MinLargerThanMax |> fail
            elif min |> minEQmax max then 
                [min |> minToValue]
                |> Set.ofList
                |> ValueSet 
                |> succ
            else (min, max) |> MinMax |> Range |> succ

        /// Create a `ValueRange` using a `ValueSet` `vs`
        /// an optional `min`, `incr` and `max`. If the `vs`
        /// is an empty set then create a `Range`. If `vs`
        /// is an empty set and both min, incr and max are 
        /// `None` than an empty `ValueSet` will be returned.
        /// If `vs` is not empty the set will be filtered 
        /// by `min`, `incr` and `max`. If `unr` then an
        /// unrestricted `ValueRange` will be returned to `succ`.
        let create succ fail unr vs min incr max =
            if unr then unrestricted |> succ
            elif vs |> Set.isEmpty then 
                match min, incr, max with
                | None,      None,       None      -> empty |> succ
                | Some min', None,       None      -> min' |> Min |> Range |> succ
                | None,      None,       Some max' -> max' |> Max |> Range |> succ 
                | Some min', None,       Some max' -> minMaxValueRange succ fail min' max'
                | Some min', Some incr', None      -> minIncrValueRange min' incr' |> succ
                | None,      Some incr', Some max' -> minIncrMaxToValueSet (incr' |> MinIncl) incr' max' |> succ
                | None,      Some incr', None      -> minIncrValueRange (incr' |> MinIncl) incr' |> succ
                | Some min', Some incr', Some max' -> minIncrMaxToValueSet min' incr' max' |> succ
            else
                vs
                |> filter min incr max
                |> ValueSet 
                |> succ
                 
        /// Create `ValueRang` and raises 
        /// an exception if it fails.
        let createExc = create id raiseExc 

        /// Return an optional `ValueRange`.
        let createOpt = create Some Option.none

        // #endregion

        // #region ---- GETTERS ----

        /// Get a `Value Set`, returns an empty set
        /// when `ValueRange` is not a `ValueSet`.
        let getValueSet = apply Set.empty id (fun _ -> Set.empty) 

        /// Get the minimum from
        /// values if there is one
        /// *Note when no minimum is specified 
        /// but there is an increment, then
        /// the increment is the minimum*
        let getMin = 
            let fRange = applyRange Some Option.none (fst >> Some) (fst >> Some) 
            apply None getSetMin fRange

        // Get an optional increment
        let getIncr = 
            let fRange = applyRange Option.none Option.none (snd >> Some) Option.none 
            apply None Option.none fRange

        /// Get the maximum from
        /// values if there is one
        let getMax = 
            let fRange = applyRange  Option.none Some Option.none (snd >> Some)
            apply None getSetMax fRange

        // #endregion

        // #region --- CONTAINS ---

        /// Check whether a `ValueRang` `vr` contains
        /// a `Value` `v`.
        let contains v vr = 
            match vr with
            | ValueSet vs when vs |> Set.isEmpty -> false
            | _ ->
                let min = vr |> getMin
                let max = vr |> getMax

                let incr = vr |> getIncr
                v |> isBetween min incr max

        // #endregion

        // #region ---- SETTERS ----

        /// Apply a `Minimum` `min` to a `ValueRang` `vr`.
        /// If minimum cannot be set the original vr is returned.
        let setMin min vr =
            let succ = id
            let fail _ = vr
            // Check whether the new min is more restrictive than the old min
            let checkMin f min' = if min |> minLTmin min' then min |> f else vr
                
            let fValueSet = 
                let max = vr  |> getMax
                let incr = vr |> getIncr
                filter (Some min) incr max >> ValueSet

            let fRange = 
                let fMax max = minMaxValueRange (id) (fun _ -> vr) min max

                let fMin min' =             min' |> checkMin (Min >> Range)
                let fMinIncr (min', incr) = min' |> checkMin (fun m   -> minIncrValueRange m incr)
                let fMinMax (min', max) =   min' |> checkMin (fun min -> minMaxValueRange succ fail min max) 

                applyRange fMin fMax fMinIncr fMinMax

            vr |> apply (Min min |> Range) fValueSet fRange

        /// Apply a `Maximum` `max` to a `ValueRang` `vr`.
        /// If maximum cannot be set the original vr is returned.
        let setMax max vr =
            let succ = id
            let fail _ = vr
            // Check whether the new max is more restrictive than the old max
            let checkMax f max' = if max' |> maxLTmax max then max |> f else vr

            let fValueSet = 
                let min = vr |> getMin
                let incr = vr |> getIncr
                filter min incr (Some max) >> ValueSet

            let fRange =
                let fMin min = minMaxValueRange (id) (fun _ -> vr) min max 

                let fMax max' =             max' |> checkMax (Max >> Range)
                let fMinMax (min, max') =   max' |> checkMax (fun max -> minMaxValueRange id (fun _ -> vr) min max) 

                let fMinIncr (min, incr) = create succ fail false Set.empty (Some min) (Some incr) (Some max)

                applyRange fMin fMax fMinIncr fMinMax

            vr |> apply (Max max |> Range) fValueSet fRange

        /// Apply a `incr` to a `ValueRang` `vr`.
        /// If increment cannot be set the original vr is returned.
        let setIncr incr vr =
            let succ = id
            let fail _ = vr
            let cr = create succ fail false Set.empty
            // Check whether the new incr is more restrictive than the old incr
            let checkIncr f incr' = if incr |> V.isMultiple incr' then incr |> f else vr

            let unr = minIncrValueRange (createMin true incr) incr

            let fValueSet = 
                let min = vr |> getMin
                let max = vr |> getMax
                filter min (Some incr) max >> ValueSet

            let fRange =
                let fMin min = minIncrValueRange min incr
                let fMax max =           cr None (Some incr) (Some max)
                let fMinMax (min, max) = cr (Some min) (Some incr) (Some max)

                let fMinIncr (min, incr') = incr' |> checkIncr (fun i -> minIncrValueRange min i) 

                applyRange fMin fMax fMinIncr fMinMax

            vr |> apply unr fValueSet fRange

        /// Appy a `Value Set` to a `ValueRange` `vr`.
        let setValues vs vr =
            let succ = id
            let fail _ = vr
            
            if vr |> isEmpty then vr
            else
                let vs1, min, incr, max = vr |> getValueSet, vr |> getMin, vr |> getIncr, vr |> getMax
                    
                let vs2 = 
                    if vs1 |> Set.isEmpty then
                        vs 
                        |> filter min incr max
                    else
                        vs 
                        |> filter min incr max
                        |> Set.intersect vs1
            
                if vs2 |> Set.isEmpty then vs2 |> ValueSet
                else
                    create succ fail false vs2 min incr max

        // #endregion
        
        // #region ---- CALCULATION -----

        /// Applies an infix operator
        /// to `ValueRange` `x1` and `x2`. 
        /// Only add values to the result set if > 0. 
        /// Calculates minimum, increment or maximum
        /// if either `x1` or `x2` is not a `ValueSet`.
        let calc op (x1, x2) =
            let calcOpt c v1 v2 =
                match op with
                | V.Mult  
                | V.Div   
                | V.Add   -> v1 |> op <| v2 |> c |> Some
                // prevent subtraction resulting in a zero or negative result
                | V.Subtr -> if v1 > v2 then (v1 - v2) |> c |> Some else None
                | V.NoOp  -> None
             
            match x1, x2 with
            | Unrestricted, Unrestricted -> unrestricted         
            | ValueSet s1, ValueSet s2 ->
                // When one of the sets does not contain any value then the result of 
                // of the calculation cannot contain any value either
                if s1 |> Set.isEmpty || s2 |> Set.isEmpty then empty
                else
                    let s1 = new ResizeArray<_>(s1)
                    let s2 = new ResizeArray<_>(s2)
                    let s3 = new ResizeArray<_>()
                    for x1 in s1 do
                        for x2 in s2 do
                            match calcOpt id x1 x2 with
                            | Some v -> s3.Add(v)
                            | None -> () 
                    new HashSet<_>(s3, HashIdentity.Structural) 
                    |> Set.ofSeq
                    |> ValueSet   
            // In any other case calculate min, incr and max
            | _ ->
                let min1, incr1, max1 = x1 |> getMin, x1 |> getIncr, x1 |> getMax
                let min2, incr2, max2 = x2 |> getMin, x2 |> getIncr, x2 |> getMax
                
                let min = 
                    match op with
                    | V.Mult ->
                        match min1, min2 with
                        // y.min = x1.min * x2.min
                        | Some m1, Some m2 -> 
                            let v1, v2  = m1 |> minToValue, m2 |> minToValue
                            let excl = m1 |> isMinExcl || m2 |> isMinExcl
                            let cmin = createMin excl
                            calcOpt cmin v1 v2 
                        | _ -> None

                    | V.Div ->
                        match min1, max2 with
                        // y.min = x1.min / x2.max
                        | Some m1, Some m2 ->
                            let v1, v2 = m1 |> minToValue, m2 |> maxToValue
                            let excl = m1 |> isMinExcl || m2 |> isMaxExcl
                            let cmin = createMin excl
                            calcOpt cmin v1 v2
                        | _ -> None

                    | V.Add -> 
                        match min1, min2 with
                        // y.min = x1.min + x2.min
                        | Some m1, Some m2 ->
                            let v1, v2  = m1 |> minToValue, m2 |> minToValue
                            let minExcl = m1 |> isMinExcl || m2 |> isMinExcl
                            let cmin = createMin minExcl
                            calcOpt cmin v1 v2 
                        // y.min = x1.min || x2.min
                        | Some m, None 
                        | None, Some m -> m |> minToValue |> createMin true |> Some
                        | None, None -> None

                    | V.Subtr -> 
                        match min1, max2 with
                        // y.min = x1.min - x2.min
                        | Some m1, Some m2 ->
                            let v1, v2 = m1 |> minToValue, m2 |> maxToValue
                            let excl = m1 |> isMinExcl || m2 |> isMaxExcl
                            let cmin = createMin excl
                            calcOpt cmin v1 v2 
                        | _ -> None

                    | V.NoOp -> None
                       
                let trueMax, max = 
                    match op with
                    | V.Mult | V.Add -> 
                        // y.max = x1.max * x2.max
                        match max1, max2 with
                        | Some m1, Some m2 -> 
                            let v1, v2  = m1 |> maxToValue, m2 |> maxToValue
                            let excl = m1 |> isMaxExcl || m2 |> isMaxExcl
                            let cmax = createMax excl
                            true, calcOpt cmax v1 v2 
                        | _ -> true, None    

                    | V.Div -> 
                        match max1, min2 with
                        // y.max = x1.max / x2.min
                        | Some m1, Some m2 ->
                            let v1, v2 = m1 |> maxToValue, m2 |> minToValue
                            let excl = m1 |> isMaxExcl || m2 |> isMinExcl
                            let cmax = createMax excl
                            true, calcOpt cmax v1 v2
                        | _ -> true, None

                    | V.Subtr -> 
                        match max1, min2 with
                        // y.max = x1.max - x2.min
                        | Some m1, Some m2 -> 
                            let v1, v2  = m1 |> maxToValue, m2 |> minToValue
                            let excl = m1 |> isMaxExcl || m2 |> isMinExcl
                            let cmax = createMax excl
                            match calcOpt cmax v1 v2 with
                            | Some m -> true, m |> Some
                            | None   -> false, None // if x1.max < x2.max then max is not really None
                        // y.max = x1.max - (None, i.e. x2.min ~ 0) 
                        | Some m1, None -> true, m1 |> maxToValue |> createMax true |> Some
                        | _ -> true, None

                    | V.NoOp -> true, None

                let incr = 
                    match incr1, incr2 with
                    | Some i1, Some i2 ->
                        match op with
                        // y.incr = x1.incr * x2.incr
                        | V.Mult -> i1 * i2 |> Some
                        // when y = x1 + x2 then y.incr = gcd of x1.incr and x2.incr
                        | V.Add | V.Subtr -> V.gcd i1 i2 |> Some
                        |  _ -> None
                    | _ -> None

                if not trueMax then empty // if not trueMax then there is no valid possible value for y
                else
                    match min, incr, max with
                    | None, None, None -> unrestricted
                    | _ -> createExc false Set.empty min incr max


        // Extend type with basic arrhythmic operations.
        type ValueRange with

            static member (*) (vr1, vr2) = calc (*) (vr1, vr2)

            static member (/) (vr1, vr2) = calc (/) (vr1, vr2)

            static member (+) (vr1, vr2) = calc (+) (vr1, vr2)

            static member (-) (vr1, vr2) = calc (-) (vr1, vr2)

            /// Apply the expression `expr` to a `ValueRang` `y`.
            static member (!=) (y, expr) = 
                let set get set vr = 
                    match expr |> get with 
                    | Some m -> vr |> set m | None -> vr 
                match expr with 
                | Unrestricted -> y
                | ValueSet vs  -> y |> setValues vs
                | _ ->
                    y 
                    |> set getMin setMin 
                    |> set getMax setMax
                    |> set getIncr setIncr

        // #endregion


    module N = Name
    module VR = ValueRange

    // #region ---- TYPES ----

    /// Represents a variable in an
    /// `Equation`. The variable is 
    /// identified by `Name` and has
    /// a set of possible `Values`.
    type Variable =
        {
            Name: Name.Name
            mutable ValueRange: ValueRange.ValueRange
        }

    // #endregion

    // #region ---- CREATORS -----

    /// Create a variable

    let create succ n vs = { Name = n; ValueRange = vs } |> succ

    let createSucc = create id

    let createRes = createSucc ("Result" |> N.createExc)

    let apply f (var: Variable) = var |> f

    let get = apply id

    // #endregion

    // #region GETTERS

    let getName v = (v |> get).Name

    let getValueRange v = (v |> get).ValueRange

    // #endregion

    // #region SETTERS

    let setName n v = { v with Name = n }

    let setValueRange v vr = 
        let vr' = (v |> get).ValueRange != vr
        v.ValueRange <- vr'

    // #endregion

    let equals v1 v2 = (v1 |> getName) = (v2 |> getName)

    let notEqual v1 v2 = v1 |> equals v2 |> not

    let hasChanged vr v = (v |> get).ValueRange = vr |> not

    let count v = v |> getValueRange |> VR.count

    let isSolved v = 
        (v |> count <= 1) &&
        (v |> getValueRange |> VR.isValueSet)

    let isSolvable = isSolved >> not

    let isUnrestricted = getValueRange >> VR.isUnrestricted

    let calc op (v1, v2) =
        (v1 |> getValueRange) |> op <| (v2 |> getValueRange) |> createRes

    // Extend type with basic arrhythmic operations.
    type Variable with

        static member (*) (v1, v2) = calc (*) (v1, v2)

        static member (/) (v1, v2) = calc (/) (v1, v2)

        static member (+) (v1, v2) = calc (+) (v1, v2)

        static member (-) (v1, v2) = calc (-) (v1, v2)

        static member (!=) (y, expr) = expr |> getValueRange |> setValueRange y

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Dto =

        open Informedica.GenSolver.Utils

        module N = Name
        module VR = ValueRange
        module V = VR.Value
        
        [<CLIMutable>]
        type Dto = 
            { 
                Name: string
                Unr: bool
                Vals: string[]
                Min: string
                MinIncl: bool
                Incr: string
                Max: string
                MaxIncl: bool 
            }

        type Message = 
            | ParseFailure of Dto
            | NameMessage of N.Message
            | ValueMessage of V.Message
            | ValueRangeMessage of VR.Message

        exception DtoException of Message

        let raiseExc m = m |> DtoException |> raise

        let createDto n unr vals min minincl incr max maxincl =  { Name = n; Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Incr = incr; Max = max; MaxIncl = maxincl }

        let createNew n = createDto n true [||] "" false "" "" false

        let apply f (d: Dto) = f d

        let setVals vals dto = { dto with Unr = false; Vals = vals }

        let setMin  min incl dto = { dto with Unr = false; Min = min; MinIncl = incl }
        let setMax  max incl dto = { dto with Unr = false; Max = max; MaxIncl = incl } 

        let setIncr incr dto = { dto with Unr = false; Incr = incr }

        let (|Vals|MinIncl|MinExcl|Incr|MaxIncl|MaxExcl|NoProp|) p =  
            match p |> String.toLower with
            | "vals"     -> Vals
            | "minincl"  -> MinIncl
            | "minexcl"  -> MinExcl
            | "incr"     -> Incr
            | "maxincl"  -> MaxIncl
            | "maxexcl"  -> MaxExcl
            | _          -> NoProp

        let setProp p v var =
            match p with 
            | Vals -> var |> setVals (v |> String.splitAt ',')
            | MinIncl  -> var |> setMin v true
            | MinExcl  -> var |> setMin v false
            | Incr -> var |> setIncr v 
            | MaxIncl  -> var |> setMax v true
            | MaxExcl  -> var |> setMax v false
            | NoProp -> var

        let toString { Name = name; Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Incr = incr; Max = max; MaxIncl = maxincl } = 

            let vals = VR.print unr vals min minincl incr max maxincl 

            sprintf "%s%s" name vals
    
        ///  Create a variable from `Variable.Dto.Dto`.
        let fromDto fail fName fValueRange fVariable (dto: Dto) =
            let parseOpt s = 
                if s |> String.IsNullOrWhiteSpace then None
                else s |> BigRational.parse |> Some

            try
                let min, incr, max = dto.Min |> parseOpt, dto.Incr |> parseOpt, dto.Max |> parseOpt
                let vals = dto.Vals |> Seq.map BigRational.parse

                let name = fName dto.Name
                
                let vr = fValueRange dto.Unr vals min dto.MinIncl incr max dto.MaxIncl
                
                fVariable name vr
            with 
            | _ -> dto |> ParseFailure |> fail            

        let fromDtoExc =
            let succ = id
            let fail = raiseExc

            let fName = N.create succ (fun m -> m |> NameMessage |> fail)
        
            let fValueRange unr vals min minincl incr max maxincl =
                if unr then VR.unrestricted
                else
                    let minRange = V.createExc >> VR.createMin (minincl |> not) >> Some
                    let maxRange = V.createExc >> VR.createMax (maxincl |> not) >> Some

                    let vs = vals |> Seq.map V.createExc |> Set.ofSeq
                    let min' = min |> Option.bind minRange
                    let max' = max |> Option.bind maxRange
                    let incr' = incr |> Option.bind (fun i ->  i |> V.createExc |> Some)

                    VR.create succ (fun m -> m |> ValueRangeMessage |> fail) false vs min' incr' max'

            let fVariable n vr = create succ n vr

            fromDto fail fName fValueRange fVariable

        let fromDtoOpt =
            let succ = Some
            let fail = Option.none

            let fName = N.create succ fail
        
            let fValueRange unr vals min minincl incr max maxincl =
                if unr then VR.unrestricted |> Some
                else
                    let minRange = V.createExc >> VR.createMin (minincl |> not) >> Some
                    let maxRange = V.createExc >> VR.createMax (maxincl |> not) >> Some

                    let vs = vals |> Seq.map V.createExc |> Set.ofSeq
                    let min' = min |> Option.bind minRange
                    let max' = max |> Option.bind maxRange
                    let incr' = incr |> Option.bind (fun i ->  i |> V.createExc |> Some)

                    VR.create succ fail false vs min' incr' max'

            let fVariable n vr = 
                match n, vr with
                | Some n', Some vr' -> create succ n' vr'
                | _ -> None

            fromDto fail fName fValueRange fVariable

        let toDto (v: Variable) =
            let optToString = V.optToString

            let dto = createNew (let (N.Name n) = v.Name in n)

            let unr = v .ValueRange |> VR.isUnrestricted

            let minincl = 
                match v.ValueRange |> VR.getMin with
                | Some m -> m |> VR.isMinExcl |> not | None -> false
            
            let maxincl = 
                match v.ValueRange |> VR.getMax with
                | Some m -> m |> VR.isMaxExcl |> not | None -> false

            let min  = 
                v.ValueRange 
                |> VR.getMin 
                |> Option.bind (VR.minToValue >> Some) 
                |> optToString

            let max  = 
                v.ValueRange 
                |> VR.getMax 
                |> Option.bind (VR.maxToValue >> Some) 
                |> optToString

            let incr = 
                v.ValueRange
                |> VR.getIncr
                |> optToString

            let vals = 
                v.ValueRange 
                |> VR.getValueSet 
                |> Set.map V.get
                |> Set.map (fun n -> n.ToString()) 
                |> Set.toArray

            { dto with Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Incr = incr; Max = max; MaxIncl = maxincl }

