namespace Informedica.GenSolver.Lib

open System

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

        type Message = | NullOrWhiteSpaceException

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
            else n |> Name |> succ

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
            let calc fs ff op (Value v1) (Value v2) =
                v1 |> op <| v2 |> create fs ff

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

        // #region ---- TYPES ----
           
        type Minimum =
            | MinIncl of Value.Value
            | MinExcl of Value.Value

        type Maximum =
            | MaxIncl of Value.Value
            | MaxExcl of Value.Value

        type Increment = 
            | Incr of Value.Value

        /// `ValueRange` represents a discrete set of 
        /// non-zero positive rational numbers,
        /// the set is either finite
        /// and then it is a set or
        /// it is infinite and then it 
        /// is a range.
        type ValueRange =
            | ValueSet of Value.Value Set
            | Min of Minimum
            | Max of Maximum
            | MinIncr of Minimum * Increment
            | MinMax  of Minimum * Maximum

        // #endregion
        
        // #region ---- UTILS -----

        /// Aply the give functions to `Values`
        /// where fv is used for `Value Set` and
        /// fr is used for `Range`
        let apply fvs fm fmi fmm fmim = function
            | ValueSet vs                 -> vs |> fvs
            | Min m                       -> m  |> fm
            | MinIncr (min, incr)         -> (min, incr) |> fmi
            | MinMax (min, max)           -> (min, max)  |> fmm
            | MinIncrMax (min, incr, max) -> (min, incr, max) |> fmim

        let count = 
            let zero _ = 0
            apply (fun s -> s |> Set.count) zero zero zero zero

        let isEmpty = count >> ((=) 0)

        let contains min incr max v =
            let fTrue = fun _ -> true
            let fMin  = function | None -> fTrue  | Some(MinIncl m) -> (<=) m | Some(MinExcl m) -> (<) m
            let fMax  = function | None -> fTrue  | Some(MaxIncl m) -> (>=) m | Some(MaxExcl m) -> (>) m
            let fIncr = function | None -> fTrue  | Some(Incr i)    -> Value.isMultiple i

            v |> fMin min &&
            v |> fIncr incr &&
            v |> fMax max

        /// Filter a set of values according
        /// to increment, min and max constraints
        let filter min incr max = Set.filter (fun v -> v |> contains min incr max)

        let getSetMin s = if s |> Set.isEmpty then None else s.MinimumElement |> MinIncl |> Some

        let getSetMax s = if s |> Set.isEmpty then None else s.MaximumElement |> MaxIncl |> Some

        // #endregion

        // #region ----- CREATORS ----

        let createMinIncl = MinIncl
        let createMinExcl = MinExcl

        let createIncr = Incr

        let createMaxIncl = MaxIncl
        let createMaxExcl = MaxExcl
            
        let empty = Set.empty |> ValueSet

        // #endregion

        // #region ---- GETTERS ----

        /// Get the minimum from
        /// values if there is one
        /// *Note when no minimum is specified 
        /// but there is an increment, then
        /// the increment is the minimum*
        let getMin = apply getSetMin Some (fst >> Some) (fst >> Some) (fun (m, _, _) -> m |> Some)

        /// Get the increment from
        /// values if there is one</br>
        /// *Note: a set of values has no increment* 
        let getIncr = apply Option.none Option.none (snd >> Some) Option.none (fun (_, i, _) -> i |> Some)

        /// Get the maximum from
        /// values if there is one
        let getMax = apply getSetMax Option.none Option.none (snd >> Some) (fun (_, _, m) -> m |> Some)

        // #endregion

        // #region ---- SETTERS ----

        let setMin min vr =

            let lt m1 m2 = 
                match m1, m2 with
                | MinIncl m1', MinIncl m2' 
                | MinIncl m1', MinExcl m2' 
                | MinExcl m1', MinExcl m2' -> m1' > m2'
                | MinExcl m1', MinIncl m2' -> m1' >= m2'

            let fvs = 
                let incr, max = vr |> getIncr, vr |> getMax
                filter (Some min) incr max >> ValueSet

            let fm min' = if min |> lt min' then min |> Min else vr 
            let fmi (min', i) = if min |> lt min' then (min, i) |> MinIncr else vr 
            let fmm (min', max) = if min |> lt min' then (min, max) |> MinMax else vr 
            let fmim (min', incr, max) = if min |> lt min' then (min, incr, max) |> MinIncrMax else vr

            apply fvs fm fmi fmm fmim

        let setIncr fs ff incr vr =
            let vals, min, incr', max = vr |> getAll
            
            if incr' |> Option.isSome then vr |> fs
            else
                create fs ff vals min (Some incr) max

        let setMin fs ff min vs =
            let create = create fs ff
            let vals, min', incr, max = vs |> getAll

            match min' with
            | Some min'' -> 
                if min > min'' then create vals (Some min) incr max 
                else vs |> fs
            | None -> create vals (Some min) incr max

        let setMax fs ff max vs =
            let create = create fs ff
            let vals, min, incr, max' = vs |> getAll

            match max' with
            | Some max'' -> 
                if max < max'' then create vals min incr (Some max) 
                else vs |> fs
            | None -> create vals min incr (Some max)

        let setValues vs vr =
            let fs = id
            let ff = fun _ -> vr

            let vs1, min, incr, max = vr |> getAll

            let intersect vs1 vs2 =
                match vs1, vs2 with
                | _ when vs1 |> Set.isEmpty -> vs2
                | _ when vs2 |> Set.isEmpty -> vs1
                | _ -> vs1 |> Set.intersect vs2
                    
            let vs2 = 
                vs
                |> ValueSet
                |> filter min incr max
                |> getValueSet
            
            create fs ff (intersect vs1 vs2) min incr max

        // #endregion

        // #region PREDICATES

        let isUnrestricted = function 
            | Unrestricted -> true
            | _ -> false

        let contains v = function
            | Unrestricted  -> true
            | Restricted(r) -> r |> Range.rangeContains v
            | ValueSet(vs)  -> vs |> Set.contains v

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
                let opIsSubtr = (Value.two |> op <| Value.one) = Value.one

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
            | _ -> unrestricted            

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
            ValueRange: ValueRange.ValueRange
        }

    // #endregion

    // #region ---- CREATORS -----

    /// Create a variable

    let create fs n vs = { Name = n; ValueRange = vs } |> fs

    let createId = create id

    let apply f (var: Variable) = var |> f

    let get = apply id

    // #endregion

    // #region GETTERS

    let getMin var = (var |> get).ValueRange |> ValueRange.getMin

    let getIncr var = (var |> get).ValueRange |> ValueRange.getIncr

    let getMax var = (var |> get).ValueRange |> ValueRange.getMax

    // #endregion

    // #region SETTERS

    let set setf fs ff v var =
        let fs' vr = { var with ValueRange = vr } |> fs
        let ff' minmax = var |> ff minmax
        (var |> get).ValueRange |> setf fs' ff' v

    let setMin fs ff v var = set ValueRange.setMin fs ff v var

    let setIncr fs ff v var = set ValueRange.setIncr fs ff v var

    let setMax fs ff v var = set ValueRange.setMax fs ff v var

     // #endregion

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Dto =

        open Informedica.GenSolver.Utils
        
        [<CLIMutable>]
        type Dto = { Name: string; Vals: string[]; Min: string; Incr: string; Max: string }

        let createDto n vals min incr max =  { Name = n; Vals = vals; Min = min; Incr = incr; Max = max}

        let createNew n = createDto n [||] "" "" ""

        let apply f (d: Dto) = f d

        let setVals vals dto = { dto with Vals = vals }
        let setMin  min  (dto: Dto) = { dto with Min = min }
        let setIncr incr (dto: Dto) = { dto with Incr = incr }
        let setMax  max  (dto: Dto) = { dto with Max = max } 

        let (|Vals|Min|Incr|Max|NoProp|) p =  
            match p |> String.toLower with
            | "vals" -> Vals
            | "min"  -> Min
            | "incr" -> Incr
            | "max"  -> Max
            | _      -> NoProp

        let setProp p v var =
            match p with 
            | Vals -> var |> setVals (v |> String.splitAt ',')
            | Min  -> var |> setMin v
            | Incr -> var |> setIncr v
            | Max  -> var |> setMax v
            | NoProp -> var

        let toString { Name = name; Vals = vals; Min = min; Incr = incr; Max = max } = 
            let printRange min incr max =
                match min, incr, max with
                | Some min, None,      None     -> sprintf "[%s..]" min
                | Some min, Some incr, None     -> sprintf "[%s..%s..]" min incr
                | Some min, Some incr, Some max -> sprintf "[%s..%s..%s]" min incr max
                | Some min, None,      Some max -> sprintf "[%s..%s]" min max
                | None,     Some incr, None     -> sprintf "[..%s..]" incr
                | None,     Some incr, Some max -> sprintf "[..%s..%s]" incr max
                | None,     None,      Some max -> sprintf "[..%s]" max
                | None,     None,      None     -> "[]"

            let printVals vals =
                "[" + (vals |> Array.fold (fun s v -> if s = "" then v else s + ", " + v) "") + "]"

            let vals = 
                if vals |> Array.isEmpty then
                    let min = if min = "" then None else Some min
                    let incr = if incr = "" then None else Some incr
                    let max = if max = "" then None else Some max
                    printRange min incr max
                else vals |> printVals

            sprintf "%s%s" name vals
    

        ///  Create a variable from `Variable.Dto.Dto`.
        let fromDto fName fValue fValueRange fVariable (dto: Dto) =
            let parse s =
                if   s |> String.IsNullOrWhiteSpace then None
                else s |> fValue |> Some

            let name = dto.Name |> fName
            let min  = dto.Min  |> parse
            let incr = dto.Incr |> parse
            let max  = dto.Max  |> parse
        
            let vr = fValueRange (dto.Vals |> Set.ofSeq) min incr max

            fVariable name vr

        let fromDtoExc =
            let fName = Name.createExc
        
            let fValue = BigRational.Parse >> ValueRange.Value.createExc

            let fValueRange vals min incr max =
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

            let fValueRange vals min incr max = 
                match min, incr, max with
                | Some min', Some incr', Some max' ->   
                    try
                        let vals' =
                            vals 
                            |> Set.map fValue
                            |> Set.map Option.get
                        ValueRange.createOpt vals' min' incr' max'
                    with 
                    | _ -> None
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

            let min  = v.ValueRange |> ValueRange.getMin  |> someValueToBigR
            let incr = v.ValueRange |> ValueRange.getIncr |> someValueToBigR
            let max  = v.ValueRange |> ValueRange.getMax  |> someValueToBigR

            let vals = 
                v.ValueRange 
                |> ValueRange.getValueSet 
                |> Set.map ValueRange.Value.get
                |> Set.map (fun n -> n.ToString()) 
                |> Set.toArray

            { dto with Vals = vals; Min = min; Incr = incr; Max = max }

