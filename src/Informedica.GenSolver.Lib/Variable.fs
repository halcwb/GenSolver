namespace Informedica.GenSolver.Lib

open System

module BigRational = 
    
    let apply f (x: BigRational) = f x

    let get = apply id

    let parse = BigRational.Parse

    let tryParse s = 
        try 
            s |> parse |> Some 
        with 
        | _ -> None

    let rec gcd a b =
        match b with
        | _  when b = 0N -> abs a
        | _ -> gcd b ((a.Numerator % b.Numerator) |> BigRational.FromBigInt)

    let toString v = (v |> get).ToString()

module String = 
    
    let apply f (s: string) = f s

    let get = apply id

    let trim s = (s |> get).Trim()

    let length s = (s |> get).Length

module Option = 

    let none _ = None


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

        // #region ---- Exceptions ----

        type Message = 
            | NullOrWhiteSpaceException
            | LongerThan30 of int

        exception NameException of Message

        let raiseExc msg = msg |> NameException |> raise

        // #endregion

        // #region ---- TYPES -----

        /// Represents a non empty/null string identifying a `variable`
        type Name = Name of string

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

        /// Returns a `Name` option when creation
        /// succeeds
        let createOpt = create Some Option.none

        /// Create a Name that
        /// is a non empty string
        /// Note: this function will fail
        /// when string is null or white space
        let createExc = create id raiseExc

        // #endregion

    /// Functions to handle `Values`
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
        
            // #region ---- EXCEPTIONS ----

            type Message = | ZeroOrNegativeValue of BigRational
            
            exception ValueException of Message

            let raiseExc msg = msg |> ValueException |> raise

            // #endregion

            // #region ---- TYPES ----

            /// Represents a non zero positive rational number.
            type Value = Value of BigRational

            // #endregion

            // #region ---- UTILS -----

            /// Apply a function `f` to value `x`.
            let apply f (Value x): 'a = f x

            let toString (Value v) = v |> BigRational.toString

            let valueOptToString = function
                | Some v' -> v' |> toString
                | None    -> ""

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

            /// Zero value, used for
            /// checking purposes. Is
            /// actually not a valid value.
//            let zero = 0N |> Value

            /// One value
            let one = 1N |> Value

            /// Two value
            let two = 2N |> Value

            let three = 3N |> Value

            // #endregion

            // #region ---- GETTERS ----

            /// Get the `BigRational` from `value`
            let get = apply id

            // #endregion

            // #region ---- CALCULATION ---- 

            /// Apply an infix operation `op` to
            /// two values `v1` and `v2`
            let calc succ fail op (Value v1) (Value v2) =
                v1 |> op <| v2 |> create succ fail

            /// Check whether a value `v` is 
            /// an increment of `incr`.
            let isMultiple (Value incr) (Value v) = 
                (v.Numerator * incr.Denominator) % (incr.Numerator * v.Denominator) = 0I

            let gcd (Value v1) (Value v2) = BigRational.gcd v1 v2 |> Value

            type Value with
                
                static member (*) (v1, v2) = calc id raiseExc (*) v1 v2

                static member (/) (v1, v2) = calc id raiseExc (/) v1 v2

                static member (+) (v1, v2) = calc id raiseExc (+) v1 v2

                static member (-) (v1, v2) = calc id raiseExc (-) v1 v2

            let opIsSubtr op = (three |> op <| two) = three - two // = 1
            let opIsAdd op   = (three |> op <| two) = three + two // = 5
            let opIsMult op  = (three |> op <| two) = three * two // = 6
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
           
        type Minimum =
            | MinIncl of V.Value
            | MinExcl of V.Value

        type Maximum =
            | MaxIncl of V.Value
            | MaxExcl of V.Value

        /// `ValueRange` represents a discrete set of 
        /// non-zero positive rational numbers,
        /// the set is either finite
        /// and then it is a set or
        /// it is infinite and then it 
        /// is a range.
        type ValueRange =
            | Unrestricted
            | ValueSet of V.Value Set
            | Min of Minimum
            | Max of Maximum
            | MinIncr of Minimum * V.Value
            | MinMax  of Minimum * Maximum

        // #endregion

        // #region Exceptions

        type Message =
            | MinLargetThanMax of Minimum * Maximum
             
        exception ValueRangeException of Message

        let raiseExc m = m |> ValueRangeException |> raise

        // #endregion
        
        // #region ---- UTILS -----

        /// Aply the give functions to `Values`
        /// where fv is used for `Value Set` and
        /// fr is used for `Range`
        let apply unr fValueSet fMin fMax fMinIncr fMinMax = function
            | Unrestricted        -> unr
            | ValueSet vs         -> vs |> fValueSet
            | Min m               -> m  |> fMin
            | Max m               -> m  |> fMax
            | MinIncr (min, incr) -> (min, incr) |> fMinIncr
            | MinMax (min, max)   -> (min, max) |> fMinMax

        let count = 
            let zero _ = 0
            apply 0 (fun s -> s |> Set.count) zero zero zero zero

        let isUnrestricted = 
            let false' _ = false
            apply true false' false' false' false' false' 

        let isValueSet =
            let false' _ = false
            apply false (fun _ -> true) false' false' false' false'             

        let isEmpty vr = (vr |> count = 0) && (vr |> isValueSet)

        let isBetween min incr max v =
            let fTrue = fun _ -> true

            let fMin  = function | None -> fTrue  | Some(MinIncl m) -> (<=) m | Some(MinExcl m) -> (<) m
            let fMax  = function | None -> fTrue  | Some(MaxIncl m) -> (>=) m | Some(MaxExcl m) -> (>) m

            let fIncr = function | None -> fTrue  | Some(incr) -> V.isMultiple incr 

            v |> fIncr incr &&
            v |> fMin min &&
            v |> fMax max

        let minLTmin m1 m2 = 
            match m2, m1 with
            | MinIncl m2', MinIncl m1' 
            | MinExcl m2', MinExcl m1' 
            | MinIncl m2', MinExcl m1' -> m2' > m1' 
            | MinExcl m2', MinIncl m1' -> m2' >= m1'

        let minSTEmin m1 m2 = m2 |> minLTmin m1 |> not

        let maxLTmax m1 m2 = 
            match m2, m1 with
            | MaxIncl m2', MaxIncl m1' 
            | MaxExcl m2', MaxExcl m1' 
            | MaxExcl m2', MaxIncl m1' -> m2' > m1'
            | MaxIncl m2', MaxExcl m1' -> m2' >= m1' 

        let maxSTEmax m1 m2 = m2 |> maxLTmax m1 |> not

        let minLTmax max min =
            match min, max with
            | MinIncl min', MaxIncl max' -> min' > max'
            | MinExcl min', MaxIncl max' 
            | MinExcl min', MaxExcl max' 
            | MinIncl min', MaxExcl max' -> min' >= max' 

        let minSTEmax max min = min |> minLTmax max |> not

        let minEQmax max min = 
            match min, max with
            | MinIncl min', MaxIncl max' -> min' = max'
            | _ -> false

        /// Filter a set of values according
        /// to increment, min and max constraints
        let filter min incr max = Set.filter (fun v -> v |> isBetween min incr max)

        let getSetMin s = if s |> Set.isEmpty then None else s.MinimumElement |> MinIncl |> Some

        let getSetMax s = if s |> Set.isEmpty then None else s.MaximumElement |> MaxIncl |> Some

        let minToValue = function | MinIncl v | MinExcl v -> v
        
        let maxToValue = function | MaxIncl v | MaxExcl v -> v    

        let toMultipleOf d n  =
            let m = (n / d) |> BigRational.ToInt32 |> BigRational.FromInt
            if m * d < n then (m + 1N) * d else m * d

        let isMinExcl = function | MinIncl _ -> false | MinExcl _ -> true

        let isMaxExcl = function | MaxIncl _ -> false | MaxExcl _ -> true
        
        // Calculate minimum as a multiple of incr
        let calcMin min incr =
            let n = match min with | MinIncl m | MinExcl m -> m |> V.get
            let (V.Value(d)) = incr
            let n' = n |> toMultipleOf d
            if min |> isMinExcl && n' <= n then n' + d else n'

        let minIncrMaxToVs min incr max =
            let min' = calcMin min incr
            let incr' = incr |> V.get
            let max' = match max with | MaxIncl m | MaxExcl m -> m |> V.get

            let vs = [min'..incr'..max'] |> Set.ofList
            // Take of the maximimum value if it equals to maximum when maximum is exclusive
            if vs |> Set.isEmpty |> not && 
               max |> isMaxExcl &&
               max' <= vs.MaximumElement then vs.Remove(vs.MaximumElement)
            else vs
            |> Set.map V.createExc
            |> ValueSet
            
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


        // #endregion

        // #region ----- CREATORS ----
            
        let unrestricted = Unrestricted

        let empty = Set.empty |> ValueSet

        let createValueSet = ValueSet

        let createMin isExcl m = if isExcl then m |> MinExcl else m |> MinIncl 

        let createMax isExcl m = if isExcl then m |> MaxExcl else m |> MaxIncl 

        let createMinIncr min incr =
            let min' = calcMin min incr |> V.createExc |> MinIncl
            (min', incr) |> MinIncr
            
        let createMinMax succ fail min max = 
            if min |> minLTmax max then (min, max) |> MinLargetThanMax |> fail
            elif min |> minEQmax max then 
                [min |> minToValue ]
                |> Set.ofList
                |> ValueSet 
                |> succ
            else (min, max) |> MinMax |> succ

        let create succ fail vs min incr max =
            if vs |> Set.isEmpty then 
                match min, incr, max with
                | None,      None,       None      -> vs |> ValueSet |> succ
                | Some min', None,       None      -> min' |> Min |> succ
                | None,      None,       Some max' -> max' |> Max |> succ 
                | Some min', None,       Some max' -> createMinMax succ fail min' max'
                | Some min', Some incr', None      -> createMinIncr min' incr' |> succ
                | None,      Some incr', Some max' -> minIncrMaxToVs (incr' |> MinIncl) incr' max' |> succ
                | None,      Some incr', None      -> createMinIncr (incr' |> MinIncl) incr' |> succ
                | Some min', Some incr', Some max' -> minIncrMaxToVs min' incr' max' |> succ
            else
                vs
                |> filter min incr max
                |> ValueSet 
                |> succ
                 
        let createExc = create id raiseExc 

        let createOpt = create Some Option.none

        // #endregion

        // #region ---- GETTERS ----

        let getValueSet = apply Set.empty id (fun _ -> Set.empty) (fun _ -> Set.empty) (fun _ -> Set.empty) (fun _ -> Set.empty)

        /// Get the minimum from
        /// values if there is one
        /// *Note when no minimum is specified 
        /// but there is an increment, then
        /// the increment is the minimum*
        let getMin = apply None getSetMin Some Option.none (fst >> Some) (fst >> Some) 

        let getIncr = apply None Option.none Option.none Option.none (snd >> Some) Option.none 

        /// Get the maximum from
        /// values if there is one
        let getMax = apply None getSetMax Option.none Some Option.none (snd >> Some)

        // #endregion

        let contains v vr = 
            match vr with
            | ValueSet vs when vs |> Set.isEmpty -> false
            | _ ->
                let min = vr |> getMin
                let max = vr |> getMax

                let incr = vr |> getIncr
                v |> isBetween min incr max

        // #region ---- SETTERS ----

        let setMin min vr =
            let succ = id
            let fail _ = vr
            // Check whether the new min is more restrictive than the old min
            let checkMin f min' = if min |> minLTmin min' then min |> f else vr
                
            let fValueSet = 
                let max = vr  |> getMax
                let incr = vr |> getIncr
                filter (Some min) incr max >> ValueSet

            let fMax max = createMinMax (id) (fun _ -> vr) min max

            let fMin min' =             min' |> checkMin Min
            let fMinIncr (min', incr) = min' |> checkMin (fun m   -> createMinIncr m incr)
            let fMinMax (min', max) =   min' |> checkMin (fun min -> createMinMax succ fail min max) 

            vr |> apply (Min min) fValueSet fMin fMax fMinIncr fMinMax

        let setMax max vr =
            let succ = id
            let fail _ = vr
            // Check whether the new max is more restrictive than the old max
            let checkMax f max' = if max' |> maxLTmax max then max |> f else vr

            let fValueSet = 
                let min = vr |> getMin
                let incr = vr |> getIncr
                filter min incr (Some max) >> ValueSet

            let fMin min = createMinMax (id) (fun _ -> vr) min max 

            let fMax max' =             max' |> checkMax Max
            let fMinMax (min, max') =   max' |> checkMax (fun max -> createMinMax id (fun _ -> vr) min max) 

            let fMinIncr (min, incr) = create succ fail Set.empty (Some min) (Some incr) (Some max)

            vr |> apply (Max max) fValueSet fMin fMax fMinIncr fMinMax

        let setIncr incr vr =
            let succ = id
            let fail _ = vr
            let cr = create succ fail Set.empty
            // Check whether the new incr is more restrictive than the old incr
            let checkIncr f incr' = if incr |> V.isMultiple incr' then incr |> f else vr

            let unr = createMinIncr (createMin true incr) incr

            let fValueSet = 
                let min = vr |> getMin
                let max = vr |> getMax
                filter min (Some incr) max >> ValueSet

            let fMin min = createMinIncr min incr
            let fMax max =           cr None (Some incr) (Some max)
            let fMinMax (min, max) = cr (Some min) (Some incr) (Some max)

            let fMinIncr (min, incr') = incr' |> checkIncr (fun i -> createMinIncr min i) 

            vr |> apply unr fValueSet fMin fMax fMinIncr fMinMax

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
                    create succ fail vs2 min incr max

        // #endregion
        
        // #region ---- CALCULATION -----

        /// Applies an infix operator
        /// to two `Values`. Only add values
        /// to the result set if > 0.
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
            | ValueSet s, _ | _, ValueSet s  when s |> Set.isEmpty -> 
                createExc Set.empty None None None
            | _ -> 
                let min1, incr1, max1 = x1 |> getMin, x1 |> getIncr, x1 |> getMax
                let min2, incr2, max2 = x2 |> getMin, x2 |> getIncr, x2 |> getMax
                
                let min = 
                    match op with
                    | V.Mult ->
                        match min1, min2 with
                        | Some m1, Some m2 -> 
                            let v1, v2  = m1 |> minToValue, m2 |> minToValue
                            let excl = m1 |> isMinExcl || m2 |> isMinExcl
                            let cmin = createMin excl
                            calcOpt cmin v1 v2 
                        | _ -> None

                    | V.Div ->
                        match min1, max2 with
                        | Some m1, Some m2 ->
                            let v1, v2 = m1 |> minToValue, m2 |> maxToValue
                            let excl = m1 |> isMinExcl || m2 |> isMaxExcl
                            let cmin = createMin excl
                            calcOpt cmin v1 v2
                        | _ -> None

                    | V.Add -> 
                        match min1, min2 with
                        | Some m1, Some m2 ->
                            let v1, v2  = m1 |> minToValue, m2 |> minToValue
                            let minExcl = m1 |> isMinExcl || m2 |> isMinExcl
                            let cmin = createMin minExcl
                            calcOpt cmin v1 v2 
                        | Some m, None 
                        | None, Some m -> m |> minToValue |> createMin true |> Some
                        | None, None -> None

                    | V.Subtr -> 
                        match min1, max2 with
                        | Some m1, Some m2 ->
                            let v1, v2 = m1 |> minToValue, m2 |> maxToValue
                            let excl = m1 |> isMinExcl || m2 |> isMaxExcl
                            let cmin = createMin excl
                            calcOpt cmin v1 v2 
                        | _ -> None

                    | V.NoOp -> None
                       
                let unr, max = 
                    match op with
                    | V.Mult | V.Add ->
                        match max1, max2 with
                        | Some m1, Some m2 -> 
                            let v1, v2  = m1 |> maxToValue, m2 |> maxToValue
                            let excl = m1 |> isMaxExcl || m2 |> isMaxExcl
                            let cmax = createMax excl
                            true, calcOpt cmax v1 v2 
                        | _ -> true, None    

                    | V.Div ->
                        match max1, min2 with
                        | Some m1, Some m2 ->
                            let v1, v2 = m1 |> maxToValue, m2 |> minToValue
                            let excl = m1 |> isMaxExcl || m2 |> isMinExcl
                            let cmax = createMax excl
                            true, calcOpt cmax v1 v2
                        | _ -> true, None

                    | V.Subtr -> 
                        match max1, min2 with
                        | Some m1, Some m2 ->
                            let v1, v2  = m1 |> maxToValue, m2 |> minToValue
                            let excl = m1 |> isMaxExcl || m2 |> isMinExcl
                            let cmax = createMax excl
                            match calcOpt cmax v1 v2 with
                            | Some m -> true, m |> Some
                            | None   -> false, None
                        | Some m1, None -> true, m1 |> maxToValue |> createMax true |> Some
                        | _ -> true, None

                    | V.NoOp -> true, None

                let incr = 
                    match incr1, incr2 with
                    | Some i1, Some i2 ->
                        match op with
                        | V.Mult -> i1 * i2 |> Some
                        | V.Add | V.Subtr -> V.gcd i1 i2 |> Some
                        |  _ -> None
                    | _ -> None

                match min, incr, max with
                | None, None, None when unr -> unrestricted
                | _ -> createExc Set.empty min incr max


        // Extend type with basic arrhythmic operations.
        type ValueRange with

            static member (*) (vr1, vr2) = calc (*) (vr1, vr2)

            static member (/) (vr1, vr2) = calc (/) (vr1, vr2)

            static member (+) (vr1, vr2) = calc (+) (vr1, vr2)

            static member (-) (vr1, vr2) = calc (-) (vr1, vr2)

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
                | Min min             -> min  |> fMin
                | Max max             -> max  |> fMax
                | MinIncr (min, incr) -> (min, incr) |> fMinIncr
                | MinMax (min, max)   -> (min, max) |> fMinMax
                            
            print unr vs min minincl incr max maxinl


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

    let createId = create id

    let apply f (var: Variable) = var |> f

    let get = apply id

    // #endregion

    // #region GETTERS

    let getMin var = (var |> get).ValueRange |> ValueRange.getMin

    let getMax var = (var |> get).ValueRange |> ValueRange.getMax

    // #endregion

    // #region SETTERS

    let setMin v var = var.ValueRange <- var.ValueRange |> ValueRange.setMin v; var

    let setMax v var = var.ValueRange <- var.ValueRange |> ValueRange.setMax v; var

    let setValueSet vs var = var.ValueRange <- var.ValueRange |> ValueRange.setValues vs; var

     // #endregion

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
                    let createMin = V.createExc >> VR.createMin (minincl |> not) >> Some
                    let createMax = V.createExc >> VR.createMax (maxincl |> not) >> Some

                    let vs = vals |> Seq.map V.createExc |> Set.ofSeq
                    let min' = min |> Option.bind createMin
                    let max' = max |> Option.bind createMax
                    let incr' = incr |> Option.bind (fun i ->  i |> V.createExc |> Some)

                    VR.create succ (fun m -> m |> ValueRangeMessage |> fail) vs min' incr' max'

            let fVariable n vr = create succ n vr

            fromDto fail fName fValueRange fVariable

        let fromDtoOpt =
            let succ = Some
            let fail = Option.none

            let fName = N.create succ fail
        
            let fValueRange unr vals min minincl incr max maxincl =
                if unr then VR.unrestricted |> Some
                else
                    let createMin = V.createExc >> VR.createMin (minincl |> not) >> Some
                    let createMax = V.createExc >> VR.createMax (maxincl |> not) >> Some

                    let vs = vals |> Seq.map V.createExc |> Set.ofSeq
                    let min' = min |> Option.bind createMin
                    let max' = max |> Option.bind createMax
                    let incr' = incr |> Option.bind (fun i ->  i |> V.createExc |> Some)

                    VR.create succ fail vs min' incr' max'

            let fVariable n vr = 
                match n, vr with
                | Some n', Some vr' -> create succ n' vr'
                | _ -> None

            fromDto fail fName fValueRange fVariable

        let toDto (v: Variable) =
            let valueOptToString = V.valueOptToString

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
                |> valueOptToString

            let max  = 
                v.ValueRange 
                |> VR.getMax 
                |> Option.bind (VR.maxToValue >> Some) 
                |> valueOptToString

            let incr = 
                v.ValueRange
                |> VR.getIncr
                |> valueOptToString

            let vals = 
                v.ValueRange 
                |> VR.getValueSet 
                |> Set.map V.get
                |> Set.map (fun n -> n.ToString()) 
                |> Set.toArray

            { dto with Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Incr = incr; Max = max; MaxIncl = maxincl }

