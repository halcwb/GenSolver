namespace Informedica.GenSolver.Lib

open System
open System.Collections.Generic
open MathNet.Numerics
open Informedica.GenSolver.Utils


module HashSet =

    let count (hs: HashSet<_>) = hs.Count

    let contains v (hs: HashSet<_>) = hs.Contains(v)

    let exists pf (hs : HashSet<_>) = hs |> Seq.exists pf

    let minElement (hs : HashSet<_>) = hs |> Seq.min

    let maxElement (hs : HashSet<_>) = hs |> Seq.max

    let fromSeq (xs : seq<_>) =
        let set = new HashSet<_>(xs)
        set

    let toList (hs : HashSet<_>) = hs |> Seq.toList

    let ofList = fromSeq

    let empty : HashSet<_> = Seq.empty |> fromSeq

    let isEmpty (hs: HashSet<_>) = hs.Count = 0

    let filter pf (hs: HashSet<_>) = 
        hs 
        |> Seq.filter pf
        |> fromSeq

    let forall pf (hs: HashSet<_>) = hs |> Seq.forall pf
        
    let intersect (hs1 : HashSet<_>) (hs2: HashSet<_>) = 
        hs1.IntersectWith(hs2)
        hs1

    let deepCopy (hs: HashSet<_>) =
        let copy = new HashSet<_>()
        copy.UnionWith(hs)

        copy

    let remove v (hs: HashSet<_>) = hs.Remove(v)

    module Tests =
        
        let testdeepcopy () = 
            let s1 = [1..5] |> fromSeq
            let s2 = s1 |> deepCopy

            s1 |> remove 1 |> ignore
            s2 |> contains 1 |> printfn "should be true: %A"
            s1 |> contains 1 |> printfn "should be false: %A"


/// Contains functions and types to represent
/// a `Variable` in an `Equation`:
///
/// * `Name`
/// * `Range`
/// * `ValueRange`
module Variable =


    /// Funcions and type to handle `Name` that represents the name of a `Variable`.
    module Name =

        open Informedica.GenUtils.Lib.BCL

        // #region ---- TYPES -----

        /// Represents a non empty/null string identifying a `Variable`.
        /// `Name` can be no longer than 30 characters.
        type Name = Name of string

        // #endregion

        // #region ---- EXCEPTIONS ----

        /// Error messages for type `Name`.
        type Message = 
            | NullOrWhiteSpaceException
            | LongerThan1000 of int

        /// Exception type for `Name`.
        exception NameException of Message

        /// Raise a `NameException` with `Message` **m**.
        let raiseExc msg = msg |> NameException |> raise

        // #endregion

        // #region ---- CREATE -----

        /// Create with continuation with **succ** function 
        /// when success and **fail** function when failure.
        /// Creates a `Name` from a`string`.
        let create succ fail n =
            if n |> String.IsNullOrWhiteSpace then NullOrWhiteSpaceException |> fail
            else 
                match n |> String.trim with
                | n' when n' |> String.length <= 1000 -> n' |> Name |> succ
                | n' -> n' |> String.length |> LongerThan1000 |> fail

        /// Returns a `Name` option if creation
        /// succeeds else `None`.
        let createOpt = create Some Option.none

        /// Create a `Name` that, raises
        /// an `NameException` when it fails.
        let createExc = create id raiseExc

        /// Return the `string` value of a `Name`.
        let toString (Name s) = s

        // #endregion

    /// Functions and types to handle `ValueRange`.
    ///
    /// * `Minimum`
    /// * `Maximum`
    /// * `Range`
    /// * `ValueRange`
    module ValueRange =

        open Informedica.GenUtils.Lib.BCL

        // #region ---- TYPES ----
           
        /// The minimal value in 
        /// a `Range`. Can be inclusive
        /// or exclusive.
        type Minimum =
            | MinIncl of BigRational
            | MinExcl of BigRational

        /// The maximum value in 
        /// a `Range`. Can be inclusive
        /// or exclusive.
        type Maximum =
            | MaxIncl of BigRational
            | MaxExcl of BigRational

        /// The increment in a `Range`. 
        /// Increment has to be a non zero
        /// or negative value
        type Increment = Increment of HashSet<BigRational>

        /// `ValueRange` represents a discrete set of 
        /// rational numbers.
        /// A `ValueRange` is either unrestricted,
        /// a finite set of `BigRational` or a `Range`.
        type ValueRange =
            | Unrestricted
            | ValueSet of HashSet<BigRational>
            | Range of Range

        /// A `Range` is restricted by either a 
        /// `Minimum`, a `Maximum`, a `Minimum` 
        /// and a increment, an increment and
        /// a `Maximum` or a `Minimum` and a
        /// `Maximum`. There is no `Range` with
        /// both an min, incr, and max, as that
        /// is, in fact, a `ValueSet`.
        and Range =
            | Min of Minimum
            | Max of Maximum
            | MinIncr of Minimum * Increment
            | IncrMax of Increment * Maximum
            | MinMax  of Minimum * Maximum

        // #endregion

        // #region --- EXCEPTIONS ---

        /// Failure messages.
        type Message =
            | MinLargerThanMax of Minimum * Maximum
            | ZeroOrNegativeIncrement of BigRational list
            | CannotCalculateValuesetAndIncrement
            | NotAValidOperator
            | EmptyValueSet
             
        /// `ValueRange` exception type
        exception ValueRangeException of Message

        /// Raise a `ValueRangeException` with `Message` **m**.
        let raiseExc m = m |> ValueRangeException |> raise

        // #endregion
        
        // #region ---- UTILS -----

        /// Apply **f** to the bigrational
        /// value of `Minimum`
        let applyMin f = function 
            | MinIncl(m) -> m |> f |> MinIncl
            | MinExcl(m) -> m |> f |> MinExcl 
            
        /// Apply **f** to the bigrational
        /// value of `Increment`
        let applyIncr f = function 
            | Increment(i) -> i |> f |> Increment
            
        /// Apply **f** to the bigrational
        /// value of `Maximum`
        let applyMax f = function 
            | MaxIncl(m) -> m |> f |> MaxIncl
            | MaxExcl(m) -> m |> f |> MaxExcl 
            
        /// Aply the give functions to `Values`
        /// where **unr** is used for an `Unrestricted` 
        /// `ValueRange`, **fv** is used for `ValueSet` and
        /// **fr** is used for `Range`
        let apply unr fValueSet fRange = function
            | Unrestricted        -> unr
            | ValueSet vs         -> vs |> fValueSet
            | Range r             -> r  |> fRange

        /// Aply the give functions to `Range`
        /// where **fMin** is used for a range with only
        /// a `Minimum`, **fMax** is used for range `Maximum`,
        /// **fMinIncr** is used for `MinIncr` , **fIncrMax**
        /// for `IncrMax` and **fMinMax** for `MinMax`. 
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
            apply 0 (fun s -> s |> HashSet.count) zero

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
        /// and doesn't contain any value.
        let isEmpty vr = (vr |> count = 0) && (vr |> isValueSet)

        /// Checks whether a `BigRational` is between an optional **min**
        /// an optional **max** and is a multiple of **incr**. 
        /// When min, incr and max are `None` then this will 
        /// return `true` for any value.
        let isBetweenAndMultOf min incr max v =
            let fTrue = fun _ -> true

            let fMin  = function | None -> fTrue  | Some(MinIncl m) -> (<=) m | Some(MinExcl m) -> (<) m
            let fMax  = function | None -> fTrue  | Some(MaxIncl m) -> (>=) m | Some(MaxExcl m) -> (>) m

            let fIncr i v = 
                match i with
                | None              -> true  
                | Some(Increment i) -> 
                    i 
                    |> HashSet.exists (fun i -> 
                        v 
                        |> BigRational.isMultiple i
                    )

            v |> fIncr incr &&
            v |> fMin min &&
            v |> fMax max

        /// Checks whether `Minimum` **m2** > **m1**
        let minLTmin m1 m2 = 
            match m2, m1 with
            | MinIncl m2', MinIncl m1' 
            | MinExcl m2', MinExcl m1' 
            | MinIncl m2', MinExcl m1' -> m2' > m1' 
            | MinExcl m2', MinIncl m1' -> m2' >= m1'

        /// Checks whether `Minimum` **m2** <= **m1**
        let minSTEmin m1 m2 = m2 |> minLTmin m1 |> not

        /// Checks whether `Maximum` **m2** > **m1**
        let maxLTmax m1 m2 = 
            match m2, m1 with
            | MaxIncl m2', MaxIncl m1' 
            | MaxExcl m2', MaxExcl m1' 
            | MaxExcl m2', MaxIncl m1' -> m2' > m1'
            | MaxIncl m2', MaxExcl m1' -> m2' >= m1' 

        /// Checks whether `Maximum` **m2** <= **m1**
        let maxSTEmax m1 m2 = m2 |> maxLTmax m1 |> not

        /// Checks whether `Minimum` **min** > `Maximum` **max**
        let minLTmax max min =
            let minmax =
                match min, max with
                | MinIncl min', MaxIncl max' -> min' > max'
                | MinExcl min', MaxIncl max' 
                | MinExcl min', MaxExcl max' 
                | MinIncl min', MaxExcl max' -> min' >= max' 
            if minmax then 
                printfn "MinLargerThanMax: %A > %A" min max
                true
            else false

        /// Checks whether `Minimum` **min** <= `Maximum` **max**
        let minSTEmax max min = min |> minLTmax max |> not

        /// Checks whether `Minimum` **min** = `Maximum` **max**
        let minEQmax max min = 
            match min, max with
            | MinIncl min', MaxIncl max' -> min' = max'
            | _ -> false

        /// Filter a set of `BigRational` according
        /// to **min**, **incr** and **max** constraints
        let filter min incr max = HashSet.filter (isBetweenAndMultOf min incr max)

        /// Get the minimal value in a `BigRational` set. Returns `None` if an empty set.
        let getSetMin s = if s |> HashSet.isEmpty then None else s |> HashSet.minElement |> MinIncl |> Some

        /// Get the maximum value in a `BigRational` set. Returns `None` if an empty set.
        let getSetMax s = if s |> HashSet.isEmpty then None else s |> HashSet.maxElement |> MaxIncl |> Some

        /// Convert a `Minimum` to a `BigRational`.
        let minToValue = function | MinIncl v | MinExcl v -> v
        
        /// Convert a `Maximum` to a `BigRational`.
        let maxToValue = function | MaxIncl v | MaxExcl v -> v    

        /// Convert an `Increment` to a `BigRational`
        let incrToValue (Increment i) = i

        /// Checks whether `Minimum` is exclusive.
        let isMinExcl = function | MinIncl _ -> false | MinExcl _ -> true

        /// Checks whether `Minimum` is inclusive.
        let isMinIncl = isMinExcl >> not

        /// Checks whether `Maximum` is exclusive.
        let isMaxExcl = function | MaxIncl _ -> false | MaxExcl _ -> true

        /// Checks whether `Maximum` is inclusive.
        let isMaxIncl = isMaxExcl >> not
        
        // Calculate `Minimum` as a multiple of `Increment` **incr**
        let minMultipleOf incr min =
            let n = match min with | MinIncl m | MinExcl m -> m 
            let d = incr |> incrToValue |> HashSet.minElement
            let n' = n |> BigRational.toMultipleOf d
            if min |> isMinExcl && n' <= n then n' + d else n'

        // Calculate `Maximum` **max** as a multiple of **incr**
        let maxMultipleOf incr max =
            let n = match max with | MaxIncl m | MaxExcl m -> m 
            let d = incr |> incrToValue |> HashSet.maxElement
            let n' = n |> BigRational.toMultipleOf d
            if max |> isMaxExcl && n' >= n then n' - d else n'

        /// Create a set of `BigRational` using **min**, **incr** and a **max**.
        let minIncrMaxToValueSet min incr max =
            let min' = min |> minMultipleOf incr
            let max' = match max with | MaxIncl m | MaxExcl m -> m 
            let incr' = incr |> incrToValue

            let vs = 
                [
                    for i in incr' do
                        [ min'..i..max' ]
                ]
                |> List.collect id
                |> Set.ofList

            // Remove the maximimum value if it equals to maximum when maximum is exclusive
            if vs |> Set.isEmpty |> not && 
               max |> isMaxExcl &&
               max' <= vs.MaximumElement then vs.Remove(vs.MaximumElement)
            else 
                vs
            |> HashSet.fromSeq
            |> ValueSet
            
        /// Create a string (to print) representation of
        /// a `ValueRange`. 
        let print unr vals min minincl incr max maxincl = 

            let printVals vals =
                let vals = vals |> List.sort |> List.map BigRational.toString
                "[" + (vals |> List.fold (fun s v -> if s = "" then v else s + ", " + v) "") + "]"

            let printRange min incr max =
                if unr then "<..>"
                else
                    let left  = if minincl then "[" else "<"
                    let right = if maxincl then "]" else ">"

                    match min, incr, max with
                    | Some min, _, None     when incr |> List.isEmpty -> sprintf "%s%A..>" left min
                    | Some min, _, Some max when incr |> List.isEmpty -> sprintf "%s%A..%A%s" left min max right
                    | None,     _, Some max when incr |> List.isEmpty -> sprintf "<..%A%s" max right
                    | Some min, incr, None     -> sprintf "%s%A..%A..>" left min (incr |> printVals) 
                    | None,     incr, Some max -> sprintf "<..%A..%A%s" incr max right
                    | _ -> "[]"

            let vals = 
                if vals |> List.isEmpty |> not then vals |> printVals
                else
                    printRange min incr max

            sprintf "%s" vals

        /// Convert a `ValueRange` to a `string`.
        let toString vr =
            let fVs vs = 
                print false (vs |> HashSet.toList) None false [] None false
            
            let fRange =
                let print min minincl incr max maxincl = print false [] min minincl incr max maxincl

                let fMin min =
                    let min, minincl = 
                        match min with
                        | MinIncl v -> v |> Some, true
                        | MinExcl v -> v |> Some, false  
                    print min minincl [] None false

                let fMax max =
                    let max, maxincl = 
                        match max with
                        | MaxIncl v -> v |> Some, true
                        | MaxExcl v -> v |> Some ,false  

                    print None false [] max maxincl

                let fMinIncr (min, incr)  = 
                    let min, minincl = 
                        match min with
                        | MinIncl v -> v |> Some, true
                        | MinExcl v -> v |> Some ,false  

                    let incr = incr |> incrToValue |> HashSet.toList
                
                    print min minincl incr None false

                let fIncrMax (incr, max)  = 
                    let max, maxincl = 
                        match max with
                        | MaxIncl v -> v |> Some, true
                        | MaxExcl v -> v |> Some ,false  

                    let incr = incr |> incrToValue |> HashSet.toList
                
                    print None false incr max maxincl

                let fMinMax (min, max) =
                    let min, minincl = 
                        match min with
                        | MinIncl v -> v |> Some, true
                        | MinExcl v -> v |> Some ,false  

                    let max, maxincl = 
                        match max with
                        | MaxIncl v -> v |> Some, true
                        | MaxExcl v -> v |> Some ,false  

                    print min minincl [] max maxincl

                applyRange fMin fMax fMinIncr fIncrMax fMinMax

            let unr = print true [] None false [] None false
            
            vr |> apply unr fVs fRange 

        // #endregion

        // #region ----- CREATORS ----
            
        /// An `Unrestricted` `ValueRange`.
        let unrestricted = Unrestricted

        /// An empty `ValueSet`.
        let empty = Seq.empty |> HashSet.fromSeq |> ValueSet

        /// Create a `ValueSet`.
        let createValueSet = ValueSet

        /// Create a `Minimum` that is 
        /// either inclusive or exclusive.
        let createMin isIncl m = if isIncl then m |> MinIncl else m |> MinExcl

        /// Create a `Maximum` that is 
        /// either inclusive or exclusive.
        let createMax isIncl m = if isIncl then m |> MaxIncl else m |> MaxExcl

        /// Create an `Increment` that 
        /// is a non zero or negative value
        let createIncr succ fail i = 
            if i |> HashSet.exists ((>) 0N) || i |> HashSet.isEmpty then i |> fail 
            else 
                i
                |> HashSet.toList
                |> List.sort
                |> List.fold (fun acc v ->
                    if acc |> List.isEmpty then [v]
                    else
                        match acc |> List.tryFind (BigRational.isMultiple v) with
                        | Some _ -> acc
                        | None   -> [v] |> List.append acc 

                ) []
                |> HashSet.ofList
                |> Increment 
                |> succ

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
            
        /// Create an `IncrMax` `ValueRange`.
        let incrMaxValueRange incr max =
            let max' = max |> maxMultipleOf incr |> MaxIncl
            (incr, max') |> IncrMax |> Range
            
        /// Create a `MinMax` `ValueRange`. If **min** > **max**
        /// pass a `Message` to **fail** else pass result to **succ**.
        let minMaxValueRange succ fail min max = 
            if min |> minLTmax max then (min, max) |> MinLargerThanMax |> fail
            elif min |> minEQmax max then 
                [min |> minToValue]
                |> HashSet.ofList
                |> ValueSet 
                |> succ
            else (min, max) |> MinMax |> Range |> succ

        /// Create a `ValueRange` using a `ValueSet` **vs**
        /// an optional `Minimum` **min**, **incr** and `Maximum` **max**.
        /// If the **vs** is an empty set then create a `Range`. 
        /// If both **min**, **incr** and **max** are `None`, and
        /// **unr** created an `Unrestricted` `ValueRange` else
        /// create an `empty` `ValueRange`. Pass the result 
        /// to **succ**, if failing a `Message` to **fail**.
        let create succ fail vs min incr max =
            if vs |> HashSet.isEmpty then 
                match min, incr, max with
                | None,      None,       None      -> unrestricted |> succ
                | Some min', None,       None      -> min' |> Min |> Range |> succ
                | None,      None,       Some max' -> max' |> Max |> Range |> succ 
                | Some min', None,       Some max' -> minMaxValueRange succ fail min' max'
                | Some min', Some incr', None      -> minIncrValueRange min' incr' |> succ
                | None,      Some incr', Some max' -> incrMaxValueRange incr' max' |> succ
                | None,      Some incr', None      -> 
                    minIncrValueRange (incr' |> incrToValue |> HashSet.minElement |> MinIncl) incr' |> succ
                | Some min', Some incr', Some max' -> minIncrMaxToValueSet min' incr' max' |> succ

            else
                vs
                |> filter min incr max
                |> HashSet.ofList
                |> ValueSet 
                |> succ
                 
        /// Create `ValueRange` and raises 
        /// an exception if it fails.
        let createExc vs min incr max = create id raiseExc vs min incr max 

        /// Return an optional `ValueRange`.
        let createOpt vs min incr max = create Some Option.none vs min incr max 

        // #endregion

        // #region ---- GETTERS ----

        /// Get a set of `BigRational` from a `ValueRange`, 
        /// returns an empty set when `ValueRange` is not 
        /// a `ValueSet` or the set of `BigRational` is actually empty.
        let getValueSet = apply HashSet.empty id (fun _ -> HashSet.empty) 

        /// Get a `Minimum` option in a `Range`
        let getRangeMin = applyRange Some Option.none (fst >> Some) Option.none (fst >> Some) 

        /// Get a `Maximum` option in a `Range`
        let getRangeMax = applyRange  Option.none Some Option.none (snd >> Some) (snd >> Some)

        /// Get an optional `Minimum` in a `ValueRange`
        let getMin = apply None getSetMin getRangeMin

        // Get an optional `Increment` in a `ValueRange`
        let getIncr = 
            let fRange = applyRange Option.none Option.none (snd >> Some) Option.none Option.none
            apply None Option.none fRange

        /// Get an optional `Maximum` in a `ValueRange`
        let getMax = apply None getSetMax getRangeMax

        // #endregion

        // #region --- CONTAINS ---

        /// Check whether a `ValueRange` **vr** contains
        /// a `BigRational` **v**.
        let contains v vr = 
            match vr with
            | ValueSet vs -> vs |> HashSet.contains v
            | _ ->
                let min = vr |> getMin
                let max = vr |> getMax

                let incr = vr |> getIncr
                v |> isBetweenAndMultOf min incr max

        // #endregion

        // #region ---- SETTERS ----

        /// Apply a `Minimum` **min** to a `ValueRange` **vr**.
        /// If minimum cannot be set the original is returned.
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
                let fIncrMax (incr, max)  = create succ fail HashSet.empty (Some min) (Some incr) (Some max)
                let fMinMax (min', max)   = min' |> checkMin (fun min -> minMaxValueRange succ fail min max) 

                applyRange fMin fMax fMinIncr fIncrMax fMinMax

            vr |> apply (Min min |> Range) fValueSet fRange

        /// Apply a `Maximum` **max** to a `ValueRange` **vr**.
        /// If maximum cannot be set the original is returned.
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
                let fMinIncr (min, incr)  = create succ fail HashSet.empty (Some min) (Some incr) (Some max)

                applyRange fMin fMax fMinIncr fIncrMax fMinMax

            vr |> apply (Max max |> Range) fValueSet fRange

        /// Apply a **incr** to a `ValueRange` **vr**.
        /// If increment cannot be set the original is returned.
        let setIncr incr vr =
            let succ = id
            let fail _ = vr
            let cr = create succ fail HashSet.empty

            // Check whether the new incr is more restrictive than the old incr
            let checkIncr f incr' = 
                if incr 
                   |> incrToValue 
                   |> HashSet.forall (fun i ->
                        incr' 
                        |> incrToValue
                        |> HashSet.exists (fun i' -> i |> BigRational.isMultiple i')
                    ) then 
                    incr |> f
                
                else vr

            let unr = minIncrValueRange (createMin true (incr |> incrToValue |> HashSet.minElement)) incr

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

        /// Appy a set of `BigRational` to a `ValueRange` **vr**.
        /// the result is a filtered or the intersect of
        /// the set of `BigRational` and **vr**.
        let setValues vs vr =
            let succ = id
            let fail _ = vr
            
            if vr |> isEmpty then vr
            else
                let vs1, min, incr, max = vr |> getValueSet, vr |> getMin, vr |> getIncr, vr |> getMax
                    
                let vs2 = 
                    if vs1 |> HashSet.isEmpty then
                        vs 
                        |> filter min incr max
                    else
                        vs 
                        |> filter min incr max
                        |> HashSet.intersect vs1
            
                if vs2 |> HashSet.isEmpty then vs2 |> ValueSet
                else
                    create succ fail vs2 min incr max

        // #endregion


        let deepCopy vr =
            let vs = 
                vr 
                |> getValueSet
                |> HashSet.deepCopy

            let i =
                vr
                |> getIncr
                |> Option.bind (incrToValue >> HashSet.deepCopy >> Increment >> Some)

            let min = vr |> getMin
            let max = vr |> getMax 

            createExc vs min i max

        
        // #region ---- CALCULATION -----

        /// Functions to calculate the `Minimum` 
        /// and `Maximum` in a `ValueRange`
        module MinMaxCalcultor =
            
            /// Calculate **x1** and **x2** with operator **op**
            /// and use **incl1** and **inc2** to determine whether
            /// the result is inclusive. Use constructor **c** to 
            /// create the optional result.
            let calc c op (x1, incl1) (x2, incl2) = 
                match x1, x2 with
                | Some (v1), Some (v2) -> 
                    if op |> BigRational.opIsDiv && v2 = 0N then None
                    else v1 |> op <| v2 |> c (incl1 && incl2) |> Some
                | _ -> None

            /// Calculate an optional `Minimum`
            let calcMin = calc createMin

            /// Calculate an optional `Maximum`
            let calcMax = calc createMax

            /// Match a min, max tuple **min**, **max**
            /// to:
            /// 
            /// * `PP`: both positive
            /// * `NN`: both negative
            /// * `NP`: one negative, the other positive
            let (|PP|NN|NP|) (min, max) = 
                match min, max with
                | Some(min), _ when min >= 0N -> PP
                | _, Some(max) when max < 0N  -> NN
                | Some(min), Some(max) when min < 0N && max >= 0N ->  NP
                | _ -> NP

            /// Calculate `Minimum` option and 
            /// `Maximum` option for addition of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let addition min1 max1 min2 max2 =
                let min = calcMin (+) min1 min2
                let max = calcMax (+) max1 max2
                min, max

            /// Calculate `Minimum` option and 
            /// `Maximum` option for subtraction of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let subtraction min1 max1 min2 max2 =
                let min = calcMin (-) min1 max2
                let max = calcMax (-) max1 min2
                min, max

            /// Calculate `Minimum` option and 
            /// `Maximum` option for multiplication of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let multiplication min1 max1 min2 max2 =
                match ((min1 |> fst), (max1 |> fst)), ((min2 |> fst), (max2 |> fst)) with
                | PP, PP ->  // min = min1 * min2, max = max1 * max2
                    calcMin (*) min1 min2, calcMax (*) max1 max2 
                | PP, NN -> // min = max1 * min2, max = min1 * max2
                    calcMin (*) max1 min2, calcMax (*) min1 max2 
                | PP, NP -> // min = max1 * min2, max = max1 * max2
                    calcMin (*) max1 min2, calcMax (*) min1 max2                 
                | NN, PP -> // min = min1 * max2, max = max1 * min2
                    calcMin (*) min1 max2, calcMax (*) max1 min2 
                | NN, NN -> // min = max1 * max2, max = min1 * min2
                    calcMin (*) max1 max2, calcMax (*) min1 min2                 
                | NN, NP -> // min = min1 * max2, max = min1 * min2
                    calcMin (*) min1 max2, calcMax (*) min1 min2 
                | NP, PP -> // min = min1 * max2, max = max1 * max2
                    calcMin (*) min1 max2, calcMax (*) max1 max2 
                | NP, NN -> // min = max1 * min2, max = min1 * min2
                    calcMin (*) max1 min2, calcMax (*) min1 min2 
                | NP, NP -> // min = min1 * max2, max = max1 * max2
                    calcMin (*) min1 max2, calcMax (*) max1 max2 

            /// Calculate `Minimum` option and 
            /// `Maximum` option for division of
            /// (**min1**, **max1**) and (**min2**, **max2)
            let division min1 max1 min2 max2 =
                match (min1 |> fst, max1 |> fst), (min2 |> fst, max2 |> fst) with
                | PP, PP -> // min = min1 / max2, max =	max1 / min2
                    calcMin (/) min1 max2, calcMax (/) max1 min2
                | PP, NN -> // min = max1 / max2	, max = min1 / min2
                    calcMin (/) max1 max2, calcMax (/) min1 min2
                | NN, PP -> // min = min1 / min2, max = max1 / max2
                    calcMin (/) min1 min2, calcMax (/) max1 max2
                | NN, NN -> // min = max1 / min2	, max = min1 / max2
                    calcMin (/) max1 min2, calcMax (/) min1 max2
                | NP, PP -> // min = min1 / min2, max = max1 / min2
                    calcMin (/) min1 min2, calcMax (/) max1 min2
                | NP, NN -> // min = max1 / max2, max = min1 / max2
                    calcMin (/) max1 max2, calcMax (/) min1 max2

                | NN, NP 
                | PP, NP              
                | NP, NP -> None, None

            /// Match the right minmax calcultion
            /// according to the operand
            let calcMinMax = function 
                | BigRational.Mult  -> multiplication
                | BigRational.Div   -> division
                | BigRational.Add   -> addition
                | BigRational.Subtr -> subtraction
                | BigRational.NoMatch ->
                    NotAValidOperator
                    |> raiseExc


        /// Safely calculate **v1** and **v2** using operator **op**,
        /// returns None if operator is division and **v2** is 0.
        let calcOpt op c v1 v2 = 
            match op with
            | BigRational.Mult  
            | BigRational.Subtr   
            | BigRational.Add  -> v1 |> op <| v2 |> c |> Some
            // prevent division by zero
            | BigRational.Div  -> 
                if v2 <> BigRational.zero then 
                    (v1 |> op <| v2) |> c |> Some 
                else None
            | BigRational.NoMatch ->
                NotAValidOperator
                |> raiseExc
                

                       
        /// Calculate an increment with
        /// **incr1** of x1 and **incr2** of x2
        /// in an equation: y = x1 **op** x2
        let calcIncr op incr1 incr2 = 
            match incr1, incr2 with
            | Some (Increment i1), Some (Increment i2) ->
                match op with
                // y.incr = x1.incr * x2.incr
                | BigRational.Mult -> 
                    [
                        for x in i1 do
                            for y in i2 do
                                x * y
                    ]
                    |> HashSet.ofList
                    |> Increment
                    |> Some

                // when y = x1 + x2 then y.incr = gcd of x1.incr and x2.incr
                | BigRational.Add | BigRational.Subtr -> 
                    [
                        for x in i1 do
                            for y in i2 do
                                BigRational.gcd x y
                    ]
                    |> HashSet.ofList
                    |> Increment 
                    |> Some

                |  _ -> None

            | _ -> None


        /// Applies an infix operator **op**
        /// to `ValueRange` **x1** and **x2**. 
        /// Calculates `Minimum`, increment or `Maximum`
        /// if either **x1** or **x2** is not a `ValueSet`.
        /// Doesn't perform any calculation when both
        /// **x1** and **x2** are `Unrestricted`.
        let calc op (x1, x2) =
            let calcOpt = calcOpt op

            match x1, x2 with
            | Unrestricted, Unrestricted -> unrestricted
            | ValueSet s1, ValueSet s2 ->
                // When one of the sets does not contain any value then the result of 
                // of the calculation cannot contain any value either
                if s1 |> HashSet.isEmpty || s2 |> HashSet.isEmpty then 
                    EmptyValueSet |> raiseExc
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
                    |> ValueSet   

            // A set with an increment results in a new set of increment
            | ValueSet s, Range(MinIncr(_, i))
            | Range(MinIncr(_, i)), ValueSet s 

            | ValueSet s, Range(IncrMax(i, _))
            | Range(IncrMax(i, _)), ValueSet s 

            | ValueSet s, Range(MinIncr(_, i))
            | Range(IncrMax(i, _)), ValueSet s 

            | ValueSet s, Range(MinIncr(_, i))
            | Range(IncrMax(i, _)), ValueSet s ->

                let min1, max1 = x1 |> getMin, x1 |> getMax
                let min2, max2 = x2 |> getMin, x2 |> getMax

                let min, max = 
                    let getMin m = 
                        let incl = 
                            match m with
                            | Some v -> v |> isMinIncl
                            | None   -> false
                        m |> Option.bind (minToValue >> Some), incl

                    let getMax m = 
                        let incl = 
                            match m with
                            | Some v -> v |> isMaxIncl
                            | None   -> false
                        m |> Option.bind (maxToValue >> Some), incl

                    MinMaxCalcultor.calcMinMax op (min1 |> getMin) (max1 |> getMax) (min2 |> getMin) (max2 |> getMax)

                let incr1 = i |> Some
                let incr2 = 
                    s
                    |> createIncr (Some) (fun _ -> None)

                let incr = calcIncr op incr1 incr2

                match min, incr, max with
                | None, None, None -> unrestricted
                | _ -> create id (fun _ -> empty) HashSet.empty min incr max

                
            // In any other case calculate min, incr and max
            | _ ->
                let min1, incr1, max1 = x1 |> getMin, x1 |> getIncr, x1 |> getMax
                let min2, incr2, max2 = x2 |> getMin, x2 |> getIncr, x2 |> getMax
                
                let min, max = 
                    let getMin m = 
                        let incl = 
                            match m with
                            | Some v -> v |> isMinIncl
                            | None   -> false
                        m |> Option.bind (minToValue >> Some), incl

                    let getMax m = 
                        let incl = 
                            match m with
                            | Some v -> v |> isMaxIncl
                            | None   -> false
                        m |> Option.bind (maxToValue >> Some), incl

                    MinMaxCalcultor.calcMinMax op (min1 |> getMin) (max1 |> getMax) (min2 |> getMin) (max2 |> getMax)

                let incr = calcIncr op incr1 incr2

                match min, incr, max with
                | None, None, None -> unrestricted
                | _ -> create id (fun _ -> empty) HashSet.empty min incr max


        // Extend type with basic arrhythmic operations.
        type ValueRange with

            static member (*) (vr1, vr2) = calc (*) (vr1, vr2)

            static member (/) (vr1, vr2) = calc (/) (vr1, vr2)

            static member (+) (vr1, vr2) = calc (+) (vr1, vr2)

            static member (-) (vr1, vr2) = calc (-) (vr1, vr2)

            /// Apply the expression **expr** to a `ValueRange` **y**.
            /// The result can only be an equal or more restricted
            /// version of **y**
            static member (!=) (y, expr) = 
                let set get set vr = 
                    match expr |> get with 
                    | Some m -> vr |> set m 
                    | None -> vr 
                match expr with 
                | Unrestricted -> y
                | ValueSet vs  -> 
                    if vs |> HashSet.isEmpty then 
                        EmptyValueSet
                        |> raiseExc
                    else y |> setValues vs
                | _ ->
                    y 
                    |> set getMin  setMin 
                    |> set getIncr setIncr
                    |> set getMax  setMax

        // #endregion

    open Informedica.GenUtils.Lib.BCL

    type Name = Name.Name
    type ValueRange = ValueRange.ValueRange

    // #region ---- TYPES ----

    /// Represents a variable in an
    /// `Equation`. The variable is 
    /// identified by `Name` and has
    /// a `Values` that are either
    /// `Unrestricted` or restricted by
    /// a `ValueSet` or a `Range`.
    type Variable =
        {
            Name: Name
            mutable Values: ValueRange
        }

    // #endregion

    // #region ---- CREATORS -----

    /// Create a `Variable` and passes
    /// the result to **succ**
    let create succ n vs = { Name = n; Values = vs } |> succ

    /// Create a `Variable` and directly
    /// return the result.
    let createSucc = create id

    /// Helper create function to
    /// store the result of a `Variable` 
    /// calculation before applying to 
    /// the actual result `Variable`.
    let createRes = createSucc ("Result" |> Name.createExc)


    let deepCopy (var : Variable) =
        { var with Values = var.Values |> ValueRange.deepCopy }

    // #endregion

    // #region ---- UTILS -----

    /// Apply **f** to `Variable` **var**.
    let apply f (var: Variable) = var |> f

    /// Helper function for type inference
    let get = apply id

    // #endregion

    // #region ---- GETTERS ----

    /// Get the `Name` of a `Variable`.
    let getName v = (v |> get).Name

    /// Get the `ValueRange of a `Variable`.
    let getValueRange v = (v |> get).Values

    let contains v vr =
        vr
        |> getValueRange
        |> ValueRange.contains v


    // #endregion

    // #region ---- SETTERS -----

    /// Change `Name` to **n**.
    let setName n v = { v with Name = n }

    /// Apply a `ValueRange` **vr** to
    /// `Variable` **v**.
    let setValueRange v vr = 
        try
            let vr' = (v |> get).Values != vr
            v.Values <- vr'
            v
        with
        | e -> 
            v.Name
            |> Name.toString
            |> sprintf "exception: %A\ncannot set value range %s" (e) 
            |> failwith

    /// Set the values to a `ValueRange` 
    /// that prevents zero or negative values.
    let setNonZeroOrNegative v = 
        let vr = (v |> get).Values |> ValueRange.setMin (BigRational.zero |> ValueRange.createMin false)
        v.Values <- vr
        v
    
    // #endregion

    // #region ---- PROPERTIES ----

    /// Get the number of distinct values
    let count v = v |> getValueRange |> ValueRange.count

    /// Checks whether **v1** and **v2** have the 
    /// same `Name`
    let eqName v1 v2 = v1 |> getName = (v2).Name

    /// Checks whether a `Variable` **v** is solved,
    /// i.e. there is but one possible value or
    /// there are no possible values left.
    let isSolved v = 
        (v |> count = 1) &&
        (v |> getValueRange |> ValueRange.isValueSet)

    /// Checks whether a `Variable` is *solvable*
    /// i.e. can be further restricted to one value
    /// (or no values at all)
    let isSolvable = isSolved >> not

    /// Checks whether there are no restrictions to
    /// possible values a `Variable` can contain
    let isUnrestricted = getValueRange >> ValueRange.isUnrestricted

    /// Apply the operator **op** to **v1** and **v2**
    /// return an intermediate *result* `Variable`.
    let calc op (v1, v2) =

        (v1 |> getValueRange) |> op <| (v2 |> getValueRange) |> createRes

    /// Extend type with basic arrhythmic operations.
    type Variable with

        static member (*) (v1, v2) = calc (*) (v1, v2)

        static member (/) (v1, v2) = calc (/) (v1, v2)

        static member (+) (v1, v2) = calc (+) (v1, v2)

        static member (-) (v1, v2) = calc (-) (v1, v2)

        /// Apply a `Variable` **expr** that is a result 
        /// of an expression to `Variable` **y**. Note that 
        /// that the resulting y is the same or a more 
        /// restricted version.
        static member (!=) (y, expr) = 
            if y |> isSolvable then 
                expr 
                |> getValueRange 
                |> setValueRange y
            else y


    /// Handle the creation of a `Variable` from a `Dto` and
    /// vice versa.
    module Dto =

        open Informedica.GenUtils.Lib.BCL
    
        /// The `Dto` representation of a `Variable`
        type Dto = 
            { 
                Name: string
                Unr: bool
                Vals: BigRational list
                Min: BigRational option
                MinIncl: bool
                Incr: BigRational list
                Max: BigRational option
                MaxIncl: bool 
            }

        /// Error messages
        type Message = 
            | ParseFailure of string
            | NameMessage of Name.Message
            | ValueRangeMessage of ValueRange.Message

        /// Dto exception type 
        exception DtoException of Message

        /// Raises a `DtoException` with `Message` **m**.
        let raiseExc m = m |> DtoException |> raise

        /// Create a `Dto`
        let createDto n unr vals min minincl incr max maxincl =  { Name = n; Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Incr = incr; Max = max; MaxIncl = maxincl }

        /// Create an *empty* *new* `Dto` with only a name **n**
        let createNew n = createDto n true [] None false [] None false

        /// Apply `f` to an `Dto` `d`
        let apply f (d: Dto) = f d

        /// Apply an array of `vals` to an **dto** 
        /// making sure the `Unr` is set to `false`.
        let setVals vals dto = { dto with Unr = false; Vals = vals }

        /// Set a `min` to an **dto** that is either inclusive `incl` true or exclusive `false`
        let setMin  min incl dto = { dto with Unr = false; Min = min; MinIncl = incl }

        /// Set a `max` to an **dto** that is either inclusive `incl` true or exclusive `false`
        let setMax  max incl dto = { dto with Unr = false; Max = max; MaxIncl = incl } 

        /// Set an `incr` to a **dto**
        let setIncr incr dto = { dto with Unr = false; Incr = incr }

        /// Match a string **p** to a field of `Dto`
        let (|Vals|MinIncl|MinExcl|Incr|MaxIncl|MaxExcl|NoProp|) p =  
            match p |> String.toLower with
            | "vals"     -> Vals
            | "minincl"  -> MinIncl
            | "minexcl"  -> MinExcl
            | "incr"     -> Incr
            | "maxincl"  -> MaxIncl
            | "maxexcl"  -> MaxExcl
            | _          -> NoProp

        /// Set a `Dto` member **p** with a value `v` to a `Dto` **dto**.
        /// If no field can be matched the **dto** is returned unchanged. 
        let setProp p vs dto =
            let getVal vs = 
                match vs with
                | [v] -> v |> Some
                | _   -> None

            match p with 
            | Vals     -> dto |> setVals vs
            | MinIncl  -> dto |> setMin  (vs |> getVal) true
            | MinExcl  -> dto |> setMin  (vs |> getVal) false
            | Incr     -> dto |> setIncr vs  
            | MaxIncl  -> dto |> setMax  (vs |> getVal) true
            | MaxExcl  -> dto |> setMax  (vs |> getVal) false
            | NoProp   -> dto

        /// Return a `string` representation of a `Dto`
        let toString { Name = name; Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Incr = incr; Max = max; MaxIncl = maxincl } = 
            let vals = ValueRange.print unr vals min minincl incr max maxincl 
            sprintf "%s%s" name vals
    
        /// Parse a sequence of `string` to
        /// `BigRational`, return the result 
        /// to the `succ` continuation or 
        /// pass a `Message` to the `fail` function
        let toValueSet succ fail vals =
            try
                vals 
                |> HashSet.fromSeq
                |> succ
            with 
            | _ -> vals.ToString() |> ParseFailure |> fail

        /// Create a `Variable` from a `Dto` and 
        /// raise a `DtoException` if this fails.
        let fromDtoExc (dto: Dto) =
            let succ = id
            let fail = raiseExc
    
            let n = dto.Name |> Name.create succ (fun m -> m |> NameMessage |> fail)
    
            let vs = dto.Vals |> toValueSet succ fail

            let min = dto.Min |> Option.bind (fun v -> v |> ValueRange.createMin dto.MinIncl |> Some)
            let max = dto.Max |> Option.bind (fun v -> v |> ValueRange.createMax dto.MaxIncl |> Some)
            let incr = 
                let faili i = 
                    i 
                    |> HashSet.toList
                    |> ValueRange.ZeroOrNegativeIncrement 
                    |> ValueRangeMessage 
                    |> fail

                dto.Incr 
                |> function 
                | [] -> None
                | _ ->
                    dto.Incr
                    |> HashSet.fromSeq
                    |> ValueRange.createIncr succ faili
                    |> Some

            let vr = ValueRange.create succ (fun m -> m |> ValueRangeMessage |> fail) vs min incr max

            create succ n vr

        /// Create a `Variable` option from a `Dto` and 
        /// return `None` when this fails.
        let fromDtoOpt (dto: Dto) =
            let succ = Some
            let fail = Option.none

            let n = dto.Name |> Name.create succ (fun m -> m |> NameMessage |> fail)
    
            let vs = 
                match dto.Vals |> toValueSet succ fail with
                | Some vs' -> vs' 
                | None -> HashSet.empty

            let min = dto.Min |> Option.bind (fun v -> v |> ValueRange.createMin dto.MinIncl |> Some)
            let max = dto.Max |> Option.bind (fun v -> v |> ValueRange.createMax dto.MaxIncl |> Some)

            let incr = 
                dto.Incr 
                |> HashSet.fromSeq
                |> ValueRange.createIncr succ fail

            let vr = ValueRange.create succ (fun m -> m |> ValueRangeMessage |> fail) vs min incr max

            match n, vr with
            | Some n', Some vr' -> create succ n' vr'
            | _ -> None

        /// Create a `Dto` from a `Variable`.
        let toDto (v: Variable) =
            let optToString = BigRational.optToString

            let dto = createNew (let (Name.Name n) = v.Name in n)

            let unr = v.Values |> ValueRange.isUnrestricted

            let minincl = 
                match v.Values |> ValueRange.getMin with
                | Some m -> m |> ValueRange.isMinExcl |> not | None -> false
        
            let maxincl = 
                match v.Values |> ValueRange.getMax with
                | Some m -> m |> ValueRange.isMaxExcl |> not | None -> false

            let min  = 
                v.Values 
                |> ValueRange.getMin 
                |> Option.bind (ValueRange.minToValue >> Some) 

            let max  = 
                v.Values 
                |> ValueRange.getMax 
                |> Option.bind (ValueRange.maxToValue >> Some) 

            let incr = 
                v.Values
                |> ValueRange.getIncr
                |> function 
                | Some i -> i |> ValueRange.incrToValue |> HashSet.toList
                | None   -> []

            let vals = 
                v.Values 
                |> ValueRange.getValueSet 
                |> HashSet.toList

            { dto with 
                Unr = unr
                Vals = vals
                Min = min
                MinIncl = minincl
                Incr = incr
                Max = max
                MaxIncl = maxincl }
