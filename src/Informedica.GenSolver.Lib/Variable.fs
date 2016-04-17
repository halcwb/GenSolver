namespace Informedica.GenSolver.Lib

open System

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

            type Value with
                
                static member (*) (v1, v2) = calc id raiseExc (*) v1 v2

                static member (/) (v1, v2) = calc id raiseExc (/) v1 v2

                static member (+) (v1, v2) = calc id raiseExc (+) v1 v2

                static member (-) (v1, v2) = calc id raiseExc (-) v1 v2

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
        let apply fValueSet fMin fMax fMinIncr fMinMax = function
            | ValueSet vs         -> vs |> fValueSet
            | Min m               -> m  |> fMin
            | Max m               -> m  |> fMax
            | MinIncr (min, incr) -> (min, incr) |> fMinIncr
            | MinMax (min, max)   -> (min, max) |> fMinMax

        let count = 
            let zero _ = 0
            apply (fun s -> s |> Set.count) zero zero zero zero

        let isEmpty = count >> ((=) 0)

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
            

        // #endregion

        // #region ----- CREATORS ----

        let createMinIncl = MinIncl

        let createMinExcl = MinExcl

        let createMaxIncl = MaxIncl

        let createMaxExcl = MaxExcl
            
        let empty = Set.empty |> ValueSet

        let createValueSet = ValueSet

        let createMin = Min

        let createMax = Max

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
            let vs' = vs |> filter min incr max

            if vs' |> Set.isEmpty then 
                match min, incr, max with
                | None,      None,       None      -> empty |> succ
                | Some min', None,       None      -> min' |> createMin |> succ
                | None,      None,       Some max' -> createMax max' |> succ 
                | Some min', None,       Some max' -> createMinMax succ fail min' max'
                | Some min', Some incr', None      -> createMinIncr min' incr' |> succ
                | None,      Some incr', Some max' -> minIncrMaxToVs (incr' |> MinIncl) incr' max' |> succ
                | None,      Some incr', None      -> createMinIncr (incr' |> MinIncl) incr' |> succ
                | Some min', Some incr', Some max' -> minIncrMaxToVs min' incr' max' |> succ
            else
                vs
                |> ValueSet 
                |> succ
                 
        let createExc = create id raiseExc 

        let createOpt = create Some Option.none

        // #endregion

        // #region ---- GETTERS ----

        let getValueSet = apply id (fun _ -> Set.empty) (fun _ -> Set.empty) (fun _ -> Set.empty) (fun _ -> Set.empty)

        /// Get the minimum from
        /// values if there is one
        /// *Note when no minimum is specified 
        /// but there is an increment, then
        /// the increment is the minimum*
        let getMin = apply getSetMin Some Option.none (fst >> Some) (fst >> Some) 

        let getIncr = apply Option.none Option.none Option.none (snd >> Some) Option.none 

        /// Get the maximum from
        /// values if there is one
        let getMax = apply getSetMax Option.none Some Option.none (snd >> Some)

        // #endregion


        let contains v vr = 
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
                let max = vr |> getMax
                let incr = vr |> getIncr
                filter (Some min) incr max >> ValueSet

            let fMax max = createMinMax (id) (fun _ -> vr) min max

            let fMin min' =             min' |> checkMin createMin
            let fMinIncr (min', incr) = min' |> checkMin (fun m -> createMinIncr m incr)
            let fMinMax (min', max) =   min' |> checkMin (fun min -> createMinMax succ fail min max) 

            vr |> apply fValueSet fMin fMax fMinIncr fMinMax

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

            let fMax max' =             max' |> checkMax createMax
            let fMinMax (min, max') =   max' |> checkMax (fun max -> createMinMax id (fun _ -> vr) min max) 

            let fMinIncr (min, incr) = create succ fail Set.empty (Some min) (Some incr) (Some max)

            vr |> apply fValueSet fMin fMax fMinIncr fMinMax

        let setIncr incr vr =
            let succ = id
            let fail _ = vr
            // Check whether the new incr is more restrictive than the old incr
            let checkIncr f incr' = if incr |> V.isMultiple incr' then incr |> f else vr

            let fValueSet = 
                let min = vr |> getMin
                let max = vr |> getMax
                filter min (Some incr) max >> ValueSet

            let fMin min = createMinIncr min incr
            let fMax max = create succ fail Set.empty None (Some incr) (Some max)
            let fMinMax (min, max) = create succ fail Set.empty (Some min) (Some incr) (Some max)

            let fMinIncr (min, incr') = incr' |> checkIncr (fun i -> createMinIncr min i) 

            vr |> apply fValueSet fMin fMax fMinIncr fMinMax

        let setValues vs vr =
            let succ = id
            let fail _ = vr
            
            let vs1, min, incr, max = vr |> getValueSet, vr |> getMin, vr |> getIncr, vr |> getMax

            let intersect vs1 vs2 =
                match vs1, vs2 with
                | _ when vs1 |> Set.isEmpty -> vs2
                | _ when vs2 |> Set.isEmpty -> vs1
                | _ -> vs1 |> Set.intersect vs2
                    
            let vs2 = vs |> filter min incr max
            
            create succ fail (intersect vs1 vs2) min incr max

        // #endregion
        
        // #region ---- CALCULATION -----

        /// Applies an infix operator
        /// to two `Values`. Only add values
        /// to the result set if > 0.
        let calc op = function
            | ValueSet s1, ValueSet s2 ->
                let s1 = new ResizeArray<_>(s1)
                let s2 = new ResizeArray<_>(s2)
                let s3 = new ResizeArray<_>()
                // Check whether the operand is subtraction
                let opIsSubtr = (V.two |> op <| V.one) = V.one

                for x1 in s1 do
                    for x2 in s2 do
                        // prevent subtraction resulting in a zero or negative result
                        if opIsSubtr && x1 > x2 || (not opIsSubtr) then 
                            // perform the arrhythmic operation and to the result set
                            s3.Add(x1 |> op <| x2) 
                new HashSet<_>(s3, HashIdentity.Structural) 
                |> Set.ofSeq
                |> ValueSet
            // Do not perform any calcuation when one of the args is not
            // a list of values
            | _ -> empty            

        // Extend type with basic arrhythmic operations.
        type ValueRange with

            static member (*) (vs1, vs2) = calc (*) (vs1, vs2)

            static member (/) (vs1, vs2) = calc (/) (vs1, vs2)

            static member (+) (vs1, vs2) = calc (+) (vs1, vs2)

            static member (-) (vs1, vs2) = calc (-) (vs1, vs2)

        // #endregion

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
        
        [<CLIMutable>]
        type Dto = { Name: string; Vals: string[]; Min: string; Max: string }

        let createDto n vals min max =  { Name = n; Vals = vals; Min = min; Max = max}

        let createNew n = createDto n [||] "" ""

        let apply f (d: Dto) = f d

        let setVals vals dto = { dto with Vals = vals }
        let setMin  min  (dto: Dto) = { dto with Min = min }
        let setMax  max  (dto: Dto) = { dto with Max = max } 

        let (|Vals|Min|Max|NoProp|) p =  
            match p |> String.toLower with
            | "vals" -> Vals
            | "min"  -> Min
            | "max"  -> Max
            | _      -> NoProp

        let setProp p v var =
            match p with 
            | Vals -> var |> setVals (v |> String.splitAt ',')
            | Min  -> var |> setMin v
            | Max  -> var |> setMax v
            | NoProp -> var

        let toString { Name = name; Vals = vals; Min = min; Max = max } = 
            let printRange min max =
                match min, max with
                | Some min, None     -> sprintf "[%s..]" min
                | Some min, Some max -> sprintf "[%s..%s]" min max
                | None,     Some max -> sprintf "[..%s]" max
                | None,     None     -> "[]"

            let printVals vals =
                "[" + (vals |> Array.fold (fun s v -> if s = "" then v else s + ", " + v) "") + "]"

            let vals = 
                if vals |> Array.isEmpty then
                    let min = if min = "" then None else Some min
                    let max = if max = "" then None else Some max
                    printRange min max
                else vals |> printVals

            sprintf "%s%s" name vals
    
        ///  Create a variable from `Variable.Dto.Dto`.
        let fromDto fName fValue fValueRange fVariable (dto: Dto) =
            let parse s =
                if   s |> String.IsNullOrWhiteSpace then None
                else s |> fValue |> Some

            let name = dto.Name |> fName
            let min  = dto.Min |> parse 
            let max  = dto.Max |> parse
        
            let vr = fValueRange (dto.Vals |> Set.ofSeq) min max

            fVariable name vr

        let fromDtoExc =
            let fName = Name.createExc
        
            let fValue = BigRational.Parse >> ValueRange.Value.createExc

            let fValueRange vals min max =
                // ToDo temp hack
                let incr = None

                let min, max = 
                    min |> Option.bind (ValueRange.MinIncl >> Some), 
                    max |> Option.bind (ValueRange.MaxIncl >> Some) 
                let vals' =
                    vals
                    |> Set.map fValue

                ValueRange.createExc vals' min incr max

            let fVariable = create id

            fromDto fName fValue fValueRange fVariable

        let fromDtoOpt =
            let fName = Name.createOpt

            let fValue v =
                try
                    v 
                    |> BigRational.Parse 
                    |> Some
                with
                | _ -> None
                |> Option.bind (ValueRange.Value.createOption)

            let fValueRange vals min max = 
                let min' = match min with | Some min' -> min' | None -> None
                let max' = match max with | Some max' ->max' | None -> None
                try
                    // ToDo temp hack
                    let incr = None

                    let min'', max'' = 
                        min' |> Option.bind (ValueRange.MinIncl >> Some), 
                        max' |> Option.bind (ValueRange.MaxIncl >> Some) 
                    let vals' =
                        vals 
                        |> Set.map fValue
                        |> Set.map Option.get
                    ValueRange.createOpt vals' min'' incr max''
                with
                | _ -> None

            let fVariable n vr = 
                match n, vr with 
                | Some n', Some vr' -> create Some n' vr'
                | _ -> None

            fromDto fName fValue fValueRange fVariable

        let toDto (v: Variable) =
            let someValueToBigR = function
                | Some v' -> let (ValueRange.Value.Value v) = v' in v.ToString()
                | None    -> ""

            let dto = createNew (let (Name.Name n) = v.Name in n)

            let min  = 
                v.ValueRange 
                |> ValueRange.getMin 
                |> Option.bind (ValueRange.minToValue >> Some) 
                |> someValueToBigR

            let max  = 
                v.ValueRange 
                |> ValueRange.getMax 
                |> Option.bind (ValueRange.maxToValue >> Some) 
                |> someValueToBigR

            let vals = 
                v.ValueRange 
                |> ValueRange.getValueSet 
                |> Set.map ValueRange.Value.get
                |> Set.map (fun n -> n.ToString()) 
                |> Set.toArray

            { dto with Vals = vals; Min = min; Max = max }

