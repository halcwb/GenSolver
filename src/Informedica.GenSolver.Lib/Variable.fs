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

        let toString (Name s) = s

        // #endregion

    /// Functions to handle `ValueRange`.
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ValueRange =

        open System.Collections.Generic
        open Informedica.GenSolver.Utils

        module BR = BigRational

        // #region ---- TYPES ----
           
        /// The minimal `Value` in 
        /// a `Range`. Can be inclusive
        /// or exclusive.
        type Minimum =
            | MinIncl of BigRational
            | MinExcl of BigRational

        /// The maximum `Value` in 
        /// a `Range`. Can be inclusive
        /// or exclusive.
        type Maximum =
            | MaxIncl of BigRational
            | MaxExcl of BigRational

        /// `ValueRange` represents a discrete set of 
        /// non-zero positive rational numbers.
        /// A `ValueRange` is either unrestricted,
        /// a finite set of values or a `Range`.
        type ValueRange =
            | Unrestricted
            | ValueSet of BigRational Set
            | Range of Range

        /// A range is restricted by either a 
        /// `Minimum`, a `Maximum`, a `Minimum` 
        /// and a increment of `Minimum` and 
        /// `Maximum`.
        and Range =
            | Min of Minimum
            | Max of Maximum
            | MinIncr of Minimum * BigRational
            | IncrMax of BigRational * Maximum
            | MinMax  of Minimum * Maximum

        // #endregion

        // #region --- EXCEPTIONS ---

        /// Failure messages.
        type Message =
            | MinLargerThanMax of Minimum * Maximum
             
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
        let applyRange fMin fMax fMinIncr fIncrMax fMinMax = function
            | Min m               -> m |> fMin
            | Max m               -> m |> fMax
            | MinIncr (min, incr) -> (min, incr) |> fMinIncr
            | IncrMax (incr, max) -> (incr, max) |> fIncrMax
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

            let fIncr = function | None -> fTrue  | Some(incr) -> BR.isMultiple incr 

            v |> fIncr incr &&
            v |> fMin min &&
            v |> fMax max

        /// Checks whether minimum `m2` > `m1`
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
        let minMultipleOf incr min =
            let n = match min with | MinIncl m | MinExcl m -> m 
            let d = incr
            let n' = n |> BR.toMultipleOf d
            if min |> isMinExcl && n' <= n then n' + d else n'

        // Calculate minimum as a multiple of incr
        let maxMultipleOf incr max =
            let n = match max with | MaxIncl m | MaxExcl m -> m 
            let d = incr
            let n' = n |> BR.toMultipleOf d
            if max |> isMaxExcl && n' >= n then n' - d else n'

        /// Create a set of values using `min`, `incr` and a `max`.
        let minIncrMaxToValueSet min incr max =
            let min' = min |> minMultipleOf incr
            let max' = match max with | MaxIncl m | MaxExcl m -> m 

            let vs = [min'..incr..max'] |> Set.ofList
            // Remove the maximimum value if it equals to maximum when maximum is exclusive
            if vs |> Set.isEmpty |> not && 
               max |> isMaxExcl &&
               max' <= vs.MaximumElement then vs.Remove(vs.MaximumElement)
            else 
                vs
            |> ValueSet
            
        /// Create a string (to print).
        let print unr vals min minincl incr max maxincl = 
            let printRange min incr max =
                if unr then "<..>"
                else
                    let left  = if minincl then "[" else "<"
                    let right = if maxincl then "]" else ">"

                    match min, incr, max with
                    | Some min, None, None          -> sprintf "%s%s..>" left min
                    | Some min, None, Some max      -> sprintf "%s%s..%s%s" left min max right
                    | None,     None, Some max      -> sprintf "<..%s%s" max right
                    | Some min, Some incr, None     -> sprintf "%s%s..%s..>" left min incr 
                    | None,     Some incr, Some max -> sprintf "<..%s..%s%s" incr max right
                    | _ -> "[]"

            let printVals vals =
                let vals = vals |> Array.map BR.parse |> Array.sort |> Array.map BR.toString
                "[" + (vals |> Array.fold (fun s v -> if s = "" then v else s + ", " + v) "") + "]"

            let vals = 
                if vals |> Array.isEmpty |> not then vals |> printVals
                else
                    let min  = if min = ""  then None else Some min
                    let incr = if incr = "" then None else Some incr
                    let max  = if max = ""  then None else Some max
                    printRange min incr max

            sprintf "%s" vals

        /// Convert a `ValueRang` to a `string`.
        let toString vr =
            let fVs vs = 
                let vs = vs |> Seq.map BR.toString |> Seq.toArray
                print false vs "" false "" "" false
            
            let fRange =
                let print min minincl incr max maxincl = print false [||] min minincl incr max maxincl

                let fMin min =
                    let min, minincl = 
                        match min with
                        | MinIncl v -> v |> BR.toString, true
                        | MinExcl v -> v |> BR.toString ,false  
                    print min minincl "" "" false

                let fMax max =
                    let max, maxincl = 
                        match max with
                        | MaxIncl v -> v |> BR.toString, true
                        | MaxExcl v -> v |> BR.toString ,false  

                    print "" false "" max maxincl

                let fMinIncr (min, incr)  = 
                    let min, minincl = 
                        match min with
                        | MinIncl v -> v |> BR.toString, true
                        | MinExcl v -> v |> BR.toString ,false  

                    let incr = incr |> BR.toString
                
                    print min minincl incr "" false

                let fIncrMax (incr, max)  = 
                    let max, maxincl = 
                        match max with
                        | MaxIncl v -> v |> BR.toString, true
                        | MaxExcl v -> v |> BR.toString ,false  

                    let incr = incr |> BR.toString
                
                    print "" false incr max maxincl

                let fMinMax (min, max) =
                    let min, minincl = 
                        match min with
                        | MinIncl v -> v |> BR.toString, true
                        | MinExcl v -> v |> BR.toString ,false  

                    let max, maxincl = 
                        match max with
                        | MaxIncl v -> v |> BR.toString, true
                        | MaxExcl v -> v |> BR.toString ,false  

                    print min minincl "" max maxincl

                applyRange fMin fMax fMinIncr fIncrMax fMinMax

            let unr = print true [||] "" false "" "" false
            
            vr |> apply unr fVs fRange 

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
        let createMin isIncl m = if isIncl then m |> MinIncl else m |> MinExcl

        /// Create a `Maximum` that is 
        /// either inclusive or exclusive.
        let createMax isIncl m = if isIncl then m |> MaxIncl else m |> MaxExcl

        /// Create a `Minimum` `Range` that is 
        /// either inclusive or exclusive.
        let minRange isIncl m = m |> createMin isIncl |> Min 

        /// Create a `Maximum` `Range` that is 
        /// either inclusive or exclusive.
        let maxRange isIncl m = m |> createMax isIncl |> Max

        /// Create a `MinIncr` `ValueRange`.
        let minIncrValueRange min incr =
            let min' = min |> minMultipleOf incr |> MinIncl
            (min', incr) |> MinIncr |> Range
            
        /// Create a `MinIncr` `ValueRange`.
        let incrMaxValueRange incr max =
            let max' = max |> maxMultipleOf incr |> MaxIncl
            (incr, max') |> IncrMax |> Range
            
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

        // ToDo improve doc
        /// Create a `ValueRange` using a `ValueSet` `vs`
        /// an optional `min`, `incr` and `max`. If the `vs`
        /// is an empty set then create a `Range`. If `vs`
        /// is an empty set and both min, incr and max are 
        /// `None` than an empty `ValueSet` will be returned.
        /// If `vs` is not empty the set will be filtered 
        /// by `min`, `incr` and `max`. If `unr` then an
        /// unrestricted `ValueRange` will be returned to `succ`.
        let create succ fail unr vs min incr max =
            if vs |> Set.isEmpty then 
                match min, incr, max with
                | None,      None,       None      -> (if unr then unrestricted else empty) |> succ
                | Some min', None,       None      -> min' |> Min |> Range |> succ
                | None,      None,       Some max' -> max' |> Max |> Range |> succ 
                | Some min', None,       Some max' -> minMaxValueRange succ fail min' max'
                | Some min', Some incr', None      -> minIncrValueRange min' incr' |> succ
                | None,      Some incr', Some max' -> incrMaxValueRange incr' max' |> succ
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
            let fRange = applyRange Some Option.none (fst >> Some) Option.none (fst >> Some) 
            apply None getSetMin fRange

        // Get an optional increment
        let getIncr = 
            let fRange = applyRange Option.none Option.none (snd >> Some) Option.none Option.none
            apply None Option.none fRange

        /// Get the maximum from
        /// values if there is one
        let getMax = 
            let fRange = applyRange  Option.none Some Option.none (snd >> Some) (snd >> Some)
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

                let fMin min'             = min' |> checkMin (Min >> Range)
                let fMinIncr (min', incr) = min' |> checkMin (fun m   -> minIncrValueRange m incr)
                let fIncrMax (incr, max)  = create succ fail false Set.empty (Some min) (Some incr) (Some max)
                let fMinMax (min', max)   = min' |> checkMin (fun min -> minMaxValueRange succ fail min max) 

                applyRange fMin fMax fMinIncr fIncrMax fMinMax

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

                let fMax max'             = max' |> checkMax (Max >> Range)
                let fMinMax (min, max')   = max' |> checkMax (fun max -> minMaxValueRange id (fun _ -> vr) min max) 
                let fIncrMax (incr, max') = max' |> checkMax (fun max -> incrMaxValueRange incr max)
                let fMinIncr (min, incr) = create succ fail false Set.empty (Some min) (Some incr) (Some max)

                applyRange fMin fMax fMinIncr fIncrMax fMinMax

            vr |> apply (Max max |> Range) fValueSet fRange

        /// Apply a `incr` to a `ValueRang` `vr`.
        /// If increment cannot be set the original vr is returned.
        let setIncr incr vr =
            let succ = id
            let fail _ = vr
            let cr = create succ fail false Set.empty
            // Check whether the new incr is more restrictive than the old incr
            let checkIncr f incr' = if incr |> BigRational.isMultiple incr' then incr |> f else vr

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
                let fIncrMax (incr', max) = incr' |> checkIncr (fun i -> incrMaxValueRange i max)

                applyRange fMin fMax fMinIncr fIncrMax fMinMax

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

        let calcOpt op c v1 v2 =
            match op with
            | BR.Mult  
            | BR.Subtr   
            | BR.Add  -> v1 |> op <| v2 |> c |> Some
            // prevent division by zero
            | BR.Div  -> if v2 <> BR.zero then (v1 |> op <| v2) |> c |> Some else None
            | BR.NoOp -> None

        let calcMin op min1 min2 max2 = 
            let calcOpt = calcOpt op
            match op with
            | BR.Mult ->
                match min1, min2 with
                // y.min = x1.min * x2.min
                | Some m1, Some m2 -> 
                    let v1, v2 = m1 |> minToValue, m2 |> minToValue
                    let incl = m1 |> isMinIncl && m2 |> isMinIncl
                    let cmin = createMin incl
                    calcOpt cmin v1 v2 
                | _ -> None

            | BR.Div ->
                match min1, max2 with
                // y.min = x1.min / x2.max
                | Some m1, Some m2 ->
                    let v1, v2 = m1 |> minToValue, m2 |> maxToValue
                    let incl = m1 |> isMinIncl && m2 |> isMaxIncl
                    let cmin = createMin incl
                    calcOpt cmin v1 v2
                | _ -> None

            | BR.Add -> 
                match min1, min2 with
                // y.min = x1.min + x2.min
                | Some m1, Some m2 ->
                    let v1, v2 = m1 |> minToValue, m2 |> minToValue
                    let incl = m1 |> isMinIncl && m2 |> isMinIncl
                    let cmin = createMin incl
                    calcOpt cmin v1 v2 
                | _ -> None

            | BR.Subtr -> 
                match min1, max2 with
                // y.min = x1.min - x2.min
                | Some m1, Some m2 ->
                    let v1, v2 = m1 |> minToValue, m2 |> maxToValue
                    let incl = m1 |> isMinIncl && m2 |> isMaxIncl
                    let cmin = createMin incl
                    calcOpt cmin v1 v2 
                | _ -> None

            | BR.NoOp -> None
                       
        let calcMax op max1 max2 min2 = 
            let calcOpt = calcOpt op
            match op with
            | BR.Mult | BR.Add -> 
                // y.max = x1.max * x2.max
                match max1, max2 with
                | Some m1, Some m2 -> 
                    let v1, v2 = m1 |> maxToValue, m2 |> maxToValue
                    let incl = m1 |> isMaxIncl && m2 |> isMaxIncl
                    let cmax = createMax incl
                    true, calcOpt cmax v1 v2 
                | _ -> true, None    

            | BR.Div -> 
                match max1, min2 with
                // y.max = x1.max / x2.min
                | Some m1, Some m2 ->
                    let v1, v2 = m1 |> maxToValue, m2 |> minToValue
                    let incl = m1 |> isMaxIncl || m2 |> isMinIncl
                    let cmax = createMax incl
                    true, calcOpt cmax v1 v2
                | _ -> true, None

            | BR.Subtr -> 
                match max1, min2 with
                // y.max = x1.max - x2.min
                | Some m1, Some m2 -> 
                    let v1, v2 = m1 |> maxToValue, m2 |> minToValue
                    let incl = m1 |> isMaxIncl || m2 |> isMinIncl
                    let cmax = createMax incl
                    match calcOpt cmax v1 v2 with
                    | Some m -> true, m |> Some
                    // if x1.max < x2.max then the range do not overlap
                    // so the result cannot have any values
                    | None   -> false, None 
                | _ -> true, None

            | BR.NoOp -> true, None

        let calcIncr op incr1 incr2 = 
            match incr1, incr2 with
            | Some i1, Some i2 ->
                match op with
                // y.incr = x1.incr * x2.incr
                | BR.Mult -> i1 * i2 |> Some
                // when y = x1 + x2 then y.incr = gcd of x1.incr and x2.incr
                | BR.Add | BR.Subtr -> BR.gcd i1 i2 |> Some
                |  _ -> None
            | _ -> None

        /// Applies an infix operator
        /// to `ValueRange` `x1` and `x2`. 
        /// Only add values to the result set if > 0. 
        /// Calculates minimum, increment or maximum
        /// if either `x1` or `x2` is not a `ValueSet`.
        let calc op (x1, x2) =
            let calcOpt = calcOpt op

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
                
                let min = calcMin op min1 min2 max2                       
                let trueMax, max = calcMax op max1 max2 min2

                let incr = calcIncr op incr1 incr2

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
                    |> set getMin  setMin 
                    |> set getIncr setIncr
                    |> set getMax  setMax

        // #endregion

    module BR = BigRational
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
            ValueRange: ValueRange.ValueRange
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
        { v with ValueRange = vr'}

    let setNonZeroOrNegative v = 
        let vr = (v |> get).ValueRange |> VR.setMin (BR.zero |> VR.createMin false)
        { v with ValueRange = vr }
    
    // #endregion

    let count v = v |> getValueRange |> VR.count

    let eqName v1 v2 = v1 |> getName = (v2).Name

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


