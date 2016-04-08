namespace Informedica.GenSolver.Lib

open System

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

        exception NullOrWhiteSpaceException

        // #endregion

        // #region ---- TYPES -----

        /// Represents a non empty/null string identifying a `variable`
        type Name = Name of string

        // #endregion

        // #region ---- CREATE -----

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

            exception NonZeroOrPositiveValueException of BigRational

            // #endregion

            // #region ---- TYPES ----

            /// Represents a non zero positive rational number.
            type Value = Value of BigRational

            // #endregion

            // #region ---- UTILS -----

            /// Apply a function `f` to value `x`.
            let apply f (Value x): 'a = f x

            let raiseExc = fun v -> raise (v |> NonZeroOrPositiveValueException)

            // #endregion

            // #region ----- CREATORS ----

            /// Creates a `Value` and calls 
            /// `succ` when success and `fail` when
            /// failure.
            let create succ fail n =
                if n <= 0N then n |> fail
                else n |> Value |> succ

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

        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Range =
            
            // #region EXCEPTIONS

            exception MinLargerThanMaxException of Value.Value * Value.Value

            // #endregion

            // #region TYPES

            type Range = 
                { 
                    Min: Value.Value option
                    Incr: Value.Value option
                    Max: Value.Value option
                }

            /// Create a range using `min` as 
            /// the minimal value option, 
            /// `incr` as the increment value option
            /// and `max` as the maximum value option.
            /// If creation succeeds send it to `fs`,
            /// if it fails send it to `ff`, i.e. when
            /// `min` is larger than `max` and 
            /// if a value set can be made send it to 
            /// fv.

            // #endregion

            // #region UTILS

            let apply f (r: Range) = f r

            let get = apply id

            // #endregion

            // #region CREATORS 

            /// Create a range with eihter a 
            /// minimum 'min`, an increment `'incr'
            /// or a maximimum `max`.
            /// If creation succeeds pass it to
            /// `fs`. If it fails pass it to `ff` 
            /// and if the range is finite, i.e.
            /// can be converted to a finite set 
            /// of values pass it to `fv`.
            let create fs ff fv min incr max =

                let minincr n d  =
                    // ToDo Temp hack
                    let fs = id
                    let ff = fun _ -> failwith "Cannot create"

                    let n, d = n |> Value.get, d |> Value.get
                    let m = (n / d) |> BigRational.ToInt32 |> BigRational.FromInt
                    if m * d < n then (m + 1N) * d else m * d
                    |> Value.create fs ff


                match min, incr, max with
                | None, None, None -> failwith "Cannot create range without either min incr or max"
                | Some _, None, None
                | None, Some _, None
                | None, None, Some _ ->
                    { Min = min; Incr = incr; Max = max } |> fs
                | Some min', Some incr', None ->
                    let min' = minincr min' incr'
                    { Min = min' |> Some; Incr = incr; Max = None } |> fs
                | Some min', None, Some max' ->
                    if min' > max' then (min', max') |> ff
                    else { Min = min; Incr = incr; Max = max } |> fs
                | None, Some incr', Some max' ->
                    if incr' > max' then (incr', max') |> ff
                    else (incr', incr', max') |> fv
                | Some min', Some incr', Some max' ->
                    if min' > max' then (min', max')     |> ff
                    elif incr' > max' then (incr', max') |> ff
                    else (min', incr', max') |> fv

                    
            // #endregion

            // #region GETTERS

            let getMin r = 
                match (r |> get).Min with 
                | None -> r.Incr
                | _    -> r.Min

            let getIncr r = (r |> get).Incr

            let getMax r = (r |> get).Max

            // #endregion

            // #region SETTERS

            let setMin fs ff v r =
                match (r |> getMin) with
                | Some m when m > v -> ff m v
                | _ -> { r with Min = Some v } |> fs 

            let setIncr fs ff v r =
                let isNotMultipleOf i = Value.isMultiple i >> not

                match (r |> getIncr) with
                | Some i when v |> isNotMultipleOf i -> ff i v
                | _ -> { r with Incr = Some v } |> fs

            let setMax fs ff v r =
                match (r |> getMax) with
                | Some m when m < v -> ff m v
                | _ -> { r with Max = Some v } |> fs 

            // #endregion

            // #region PREDICATES

            let contains v { Min = min; Incr = incr; Max = max } =
                match min, incr, max with
                | Some min', None,       None      -> v >= min'
                | None,      Some incr', None      -> v |> Value.isMultiple incr'
                | None,      None,       Some max' -> v <= max'
                | Some min', Some incr', None      -> v >= min' && v |> Value.isMultiple incr'
                | Some min', None,       Some max' -> v >= min' && v <= max'
                | None,      Some _,     Some _
                | Some _,    Some _,     Some _
                | None,      None,       None      -> failwith "Range is not valid"

            // #endregion

        // #region ---- TYPES ----

        /// `ValueRange` represents a discrete set of 
        /// non-zero positive rational numbers,
        /// the set is either finite
        /// and then it is a set or
        /// it is infinite and then it 
        /// is a range.
        type ValueRange =
            | Unrestricted
        /// A `Range` is an infinite set of
        /// rational numbers, when a set has
        /// both a minimum, maximum and an 
        /// increment then it is not a range
        /// anymore but a finite set of values
            | Restricted of Range.Range
            | ValueSet of Value.Value Set

        // #endregion
        
        // #region ---- UTILS -----

        /// Aply the give functions to `Values`
        /// where fv is used for `Value Set` and
        /// fr is used for `Range`
        let apply ur fr fv vs : 'a =
            match vs with
            | Unrestricted -> ur
            | Restricted x -> x |> fr
            | ValueSet x -> x |> fv

        /// Filter a set of values according
        /// to increment, min and max constraints
        let filter min incr max vr = 
            let fTrue = fun _ -> true
            let fMin  = function | None -> fTrue | Some min ->  (<=) min
            let fIncr = function | None -> fTrue | Some incr -> Value.isMultiple incr
            let fMax  = function | None -> fTrue | Some max ->  (>=) max

            let fv = Set.filter (fun v -> v |> fMin min &&
                                          v |> fIncr incr &&
                                          v |> fMax max)
                     >> ValueSet

            let ur = Unrestricted
            let fr = Restricted

            apply ur fr fv vr

        let getSetMin s = if s |> Set.isEmpty then None else s.MinimumElement |> Some

        let getSetMax s = if s |> Set.isEmpty then None else s.MaximumElement |> Some

        // #endregion

        // #region ----- CREATORS ----

        let unrestricted = Unrestricted

        let createValueSet fs ff vals min incr max =
            let get = Option.get

            let test v1 v2 = v1 |> Option.isSome && v2 |> Option.isSome && v1 > v2

            if test min max    then (min |> get,  max |> get) |> ff
            elif test incr max then (incr |> get, max |> get) |> ff
            else
                vals
                |> ValueSet 
                |> filter min incr max
                |> fs

        let createRange fs ff min incr max =
            let createVal = Value.create (id >> Some) (fun _ -> None)

            let fv = fun (min, incr, max) -> 
                [(min |> Value.get)..(incr |> Value.get)..(max |> Value.get)]
                |> List.map createVal
                |> List.filter Option.isSome
                |> List.map Option.get
                |> Set.ofList 
                |> ValueSet
                |> fs

            match min, incr, max with
            | None, None, None -> unrestricted |> fs
            | _ -> Range.create (Restricted >> fs) ff fv min incr max

        let create fs ff (vals: Value.Value Set) min incr max =
            match vals with
            | _ when vals.IsEmpty ->
                createRange fs ff min incr max
            | _ -> createValueSet fs ff vals min incr max

        // #endregion

        // #region ---- GETTERS ----

        /// Get the values set, returns 
        /// empty set when values is a 
        /// range.
        let getValueSet = apply Set.empty (fun  _ -> Set.empty) id 

        /// Get the minimum from
        /// values if there is one
        /// *Note when no minimum is specified 
        /// but there is an increment, then
        /// the increment is the minimum*
        let getMin =
            let fv (vs: Value.Value Set) = vs |> getSetMin
            let fr = Range.getMin

            apply None fr fv

        /// Get the increment from
        /// values if there is one</br>
        /// *Note: a set of values has no increment* 
        let getIncr =
            let fv = fun _ -> None
            let fr = Range.getIncr

            apply None fr fv

        /// Get the maximum from
        /// values if there is one
        let getMax =
            let fv (vs: Value.Value Set) = vs |> getSetMax
            let fr = Range.getMax

            apply None fr fv

        /// Get all `Value`'s.
        let getAll vr =  (vr |> getValueSet), vr |> getMin, vr |> getIncr, vr |> getMax

        // #endregion

        // #region ---- SETTERS ----

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
            | Restricted(r) -> r |> Range.contains v
            | ValueSet(vs)  -> vs |> Set.contains v

        // #endregion
        
        // #region ---- CALCULATION -----

        let count = getValueSet >> Set.count

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

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Dto =

        open Informedica.GenSolver.Utils
        
        [<CLIMutable>]
        type Dto = { Name: string; Vals: string[]; Min: string; Incr: string; Max: string }

        let create n vals min max incr =  { Name = n; Vals = vals; Min = min; Incr = incr; Max = max}

        let createNew n = create n [||] "" "" ""

        let apply f (d: Dto) = f d

        let setVals vals dto = { dto with Vals = vals }
        let setMin  min  dto = { dto with Min = min }
        let setMax  max  dto = { dto with Max = max }
        let setIncr incr dto = { dto with Incr = incr }

        let (|Vals|Min|Max|Incr|NoProp|) p =  
            match p |> String.toLower with
            | "vals" -> Vals
            | "min"  -> Min
            | "max"  -> Max
            | "incr" -> Incr
            | _      -> NoProp

        let setProp p v var =
            match p with 
            | Vals -> var |> setVals (v |> String.splitAt ',')
            | Min  -> var |> setMin v
            | Max  -> var |> setMax v
            | Incr -> var |> setIncr v
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
                    let max = if max = "" then None else Some max
                    let incr = if incr = "" then None else Some incr
                    printRange min max incr
                else vals |> printVals

            sprintf "%s%s" name vals
    
    // #region ---- TYPES ----

    /// Represents a variable in an
    /// `Equation`. The variable is 
    /// identified by `Name` and has
    /// a set of possible `Values`.
    type Variable =
        {
            Name: Name.Name
            Values: ValueRange.ValueRange
            Min: ValueRange.Value.Value option
            Max: ValueRange.Value.Value option
        }

    // #endregion

    // #region ---- CREATE -----

    /// Create a variable
    let create n vs = { Name = n; Values = vs; Min = None; Max = None }

    type DtoStrat<'Name, 'Value, 'ValueSet, 'ValueRange, 'Variable> = 
        {
            CreateName: string -> 'Name
            CreateValue: string -> 'Value
            CreateValueSet: string[] -> 'ValueSet
            CreateValueRange: 'ValueSet -> 'Value -> 'Value -> 'Value -> 'ValueRange
            CreateVariable: 'Name -> 'ValueRange -> 'Variable
       }

    ///  Create a variable from `Variable.Dto.Dto`.
    let fromDto (strat: DtoStrat<_, _, _, _, _>) (dto: Dto.Dto) =
        let createVr min incr max vs = strat.CreateValueRange vs min incr max
        
        let createValue = strat.CreateValue

        let name = dto.Name |> strat.CreateName
        let min  = dto.Min  |> createValue
        let incr = dto.Incr |> createValue
        let max  = dto.Max  |> createValue
        
        dto.Vals 
        |> strat.CreateValueSet
        |> createVr min incr max
        |> strat.CreateVariable name


    let fromDtoOpt =
        let createName = Name.create 

        let createValue v =
            let ff = fun _ -> None
            let fs = Some
            try
                v 
                |> BigRational.Parse 
                |> Some
            with
            | _ -> None

            |> Option.bind (ValueRange.Value.create fs ff)

        let createValueSet vs =
            vs
            |> Array.map createValue
            |> Array.filter Option.isSome
            |> Array.map Option.get
            |> Set.ofSeq

        let createValueRange = 
            let fs = Some
            let ff = fun _ -> None
            ValueRange.create fs ff

        let createVar n = Option.bind (create n >> Some)

        let strat = 
            {   
                CreateName = createName
                CreateValue  = createValue
                CreateValueSet = createValueSet
                CreateValueRange = createValueRange
                CreateVariable = createVar
            }

        fromDto strat
        

    let toDto (v: Variable) =
        let someValueToBigR = function
            | Some v' -> let (ValueRange.Value.Value v) = v' in v.ToString()
            | None    -> ""

        let dto = Dto.createNew (let (Name.Name n) = v.Name in n)

        let min  = v.Values |> ValueRange.getMin  |> someValueToBigR
        let incr = v.Values |> ValueRange.getIncr |> someValueToBigR
        let max  = v.Values |> ValueRange.getMax  |> someValueToBigR

        let vals = 
            v.Values 
            |> ValueRange.getValueSet 
            |> Set.map ValueRange.Value.get
            |> Set.map (fun n -> n.ToString()) 
            |> Set.toArray

        { dto with Vals = vals; Min = min; Incr = incr; Max = max }

    // #endregion

