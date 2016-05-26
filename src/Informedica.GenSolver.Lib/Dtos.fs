namespace Informedica.GenSolver.Dtos

open System

open Informedica.GenSolver.Utils

/// Handle the creation of a `Variable` from a `Dto` and
/// vice versa.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Variable =
    
    open Informedica.GenSolver.Utils

    module BR = BigRational
    module VAR = Informedica.GenSolver.Lib.Variable
    module N = VAR.Name
    module VR = VAR.ValueRange
        
    /// The `Dto` representation of a `Variable`
    [<CLIMutable>]
    type Dto = 
        { 
            Name: string
            Unr: bool
            Vals: BigRational list
            Min: BigRational option
            MinIncl: bool
            Incr: BigRational option
            Max: BigRational option
            MaxIncl: bool 
        }

    /// Error messages
    type Message = 
        | ParseFailure of string
        | NameMessage of N.Message
        | ValueRangeMessage of VR.Message

    /// Dto exception type 
    exception DtoException of Message

    /// Raises a `DtoException` with `Message` `m`.
    let raiseExc m = m |> DtoException |> raise

    /// Create a `Dto`
    let createDto n unr vals min minincl incr max maxincl =  { Name = n; Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Incr = incr; Max = max; MaxIncl = maxincl }

    /// Create an *empty* *new* `Dto` with only a name `n`
    let createNew n = createDto n true [] None false None None false

    /// Apply `f` to an `Dto` `d`
    let apply f (d: Dto) = f d

    /// Apply an array of `vals` to an `dto` 
    /// making sure the `Unr` is set to `false`.
    let setVals vals dto = { dto with Unr = false; Vals = vals }

    /// Set a `min` to an `dto` that is either inclusive `incl` true or exclusive `false`
    let setMin  min incl dto = { dto with Unr = false; Min = min; MinIncl = incl }

    /// Set a `max` to an `dto` that is either inclusive `incl` true or exclusive `false`
    let setMax  max incl dto = { dto with Unr = false; Max = max; MaxIncl = incl } 

    /// Set an `incr` to a `dto`
    let setIncr incr dto = { dto with Unr = false; Incr = incr }

    /// Match a string `p` to a field of `Dto`
    let (|Vals|MinIncl|MinExcl|Incr|MaxIncl|MaxExcl|NoProp|) p =  
        match p |> String.toLower with
        | "vals"     -> Vals
        | "minincl"  -> MinIncl
        | "minexcl"  -> MinExcl
        | "incr"     -> Incr
        | "maxincl"  -> MaxIncl
        | "maxexcl"  -> MaxExcl
        | _          -> NoProp

    /// Set a `Dto` member `p` with a value `v` to a `Dto` `dto`.
    /// If no field can be matched the `dto` is returned unchanged. 
    let setProp p vs dto =
        let getVal vs = 
            match vs with
            | [v] -> v |> Some
            | _   -> None

        match p with 
        | Vals     -> dto |> setVals vs
        | MinIncl  -> dto |> setMin  (vs |> getVal) true
        | MinExcl  -> dto |> setMin  (vs |> getVal) false
        | Incr     -> dto |> setIncr (vs |> getVal)  
        | MaxIncl  -> dto |> setMax  (vs |> getVal) true
        | MaxExcl  -> dto |> setMax  (vs |> getVal) false
        | NoProp   -> dto

    /// Return a `string` representation of a `Dto`
    let toString { Name = name; Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Incr = incr; Max = max; MaxIncl = maxincl } = 
        let vals = VR.print unr vals min minincl incr max maxincl 
        sprintf "%s%s" name vals
        
    /// Parse a sequence of `string` to
    /// `BigRational`, return the result 
    /// to the `succ` continuation or 
    /// pass a `Message` to the `fail` function
    let toValueSet succ fail vals =
        try
            vals 
            |> Set.ofSeq
            |> succ
        with 
        | _ -> vals.ToString() |> ParseFailure |> fail

    /// Create a `Variable` from a `Dto` and 
    /// raise a `DtoException` if this fails.
    let fromDtoExc (dto: Dto) =
        let succ = id
        let fail = raiseExc
        
        let n = dto.Name |> N.create succ (fun m -> m |> NameMessage |> fail)
        
        let vs = dto.Vals |> toValueSet succ fail

        let min = dto.Min |> Option.bind (fun v -> v |> VR.createMin dto.MinIncl |> Some)
        let max = dto.Max |> Option.bind (fun v -> v |> VR.createMax dto.MaxIncl |> Some)
        let incr = 
            let faili i = i |> VR.ZeroOrNegativeIncrement |> ValueRangeMessage |> fail
            dto.Incr |> Option.bind (fun v -> v |> VR.createIncr succ faili |> Some)

        let vr = VR.create succ (fun m -> m |> ValueRangeMessage |> fail) dto.Unr vs min incr max

        VAR.create succ n vr

    /// Create a `Variable` option from a `Dto` and 
    /// return `None` when this fails.
    let fromDtoOpt (dto: Dto) =
        let succ = Some
        let fail = Option.none

        let n = dto.Name |> N.create succ (fun m -> m |> NameMessage |> fail)
        
        let vs = 
            match dto.Vals |> toValueSet succ fail with
            | Some vs' -> vs' 
            | None -> Set.empty

        let min = dto.Min |> Option.bind (fun v -> v |> VR.createMin dto.MinIncl |> Some)
        let max = dto.Max |> Option.bind (fun v -> v |> VR.createMax dto.MaxIncl |> Some)

        let incr = 
            dto.Incr 
            |> Option.bind (fun v -> v |> VR.createIncr succ fail)

        let vr = VR.create succ (fun m -> m |> ValueRangeMessage |> fail) dto.Unr vs min incr max

        match n, vr with
        | Some n', Some vr' -> VAR.create succ n' vr'
        | _ -> None

    /// Create a `Dto` from a `Variable`.
    let toDto (v: VAR.Variable) =
        let optToString = BR.optToString

        let dto = createNew (let (N.Name n) = v.Name in n)

        let unr = v.Values |> VR.isUnrestricted

        let minincl = 
            match v.Values |> VR.getMin with
            | Some m -> m |> VR.isMinExcl |> not | None -> false
            
        let maxincl = 
            match v.Values |> VR.getMax with
            | Some m -> m |> VR.isMaxExcl |> not | None -> false

        let min  = 
            v.Values 
            |> VR.getMin 
            |> Option.bind (VR.minToValue >> Some) 

        let max  = 
            v.Values 
            |> VR.getMax 
            |> Option.bind (VR.maxToValue >> Some) 

        let incr = 
            v.Values
            |> VR.getIncr
            |> Option.bind (VR.incrToValue >> Some)

        let vals = 
            v.Values 
            |> VR.getValueSet 
            |> Set.toList

        { dto with Unr = unr; Vals = vals; Min = min; MinIncl = minincl; Incr = incr; Max = max; MaxIncl = maxincl }


/// Functions for `Equation` dto type
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Equation =

    open Informedica.GenSolver.Utils

    module EQ = Informedica.GenSolver.Lib.Equation
    
    /// `Dto` for an `Equation`
    [<CLIMutable>]
    type Dto = { Vars: Variable.Dto[]; IsProdEq: bool }

    /// `Message` type for failures
    type Message =
        | NoVarsInEquation 
        | EquationMessage of EQ.Message
        | VariableMessage of Variable.Message

    /// `DtoException type 
    exception DtoException of Message

    /// Raise a `DtoException` with `Message` `m`
    let raiseExc m = m |> DtoException |> raise

    /// Create a `Dto` with `vars` (variable dto array)
    /// that is either a `ProductEquation` or a `SumEquation`
    let create isProd vars  = { Vars = vars; IsProdEq = isProd }

    /// Create a `ProductEquation` `Dto`
    let createProd = create true

    /// Create a `SumEquation` `Dto`
    let createSum  = create false

    /// Apply a function `f` to a `Dto` `dto`
    let apply f (dto: Dto) = dto |> f 

    /// Helper function to facilitate type 
    let get = apply id

//    /// If equation `eq` contains a variable 
//    /// with name `n` then the property `p` of
//    /// that variable is updated with value `v`. 
//    let setVar n p v eq = 
//        let var = 
//            match (eq |> get).Vars |> Array.tryFind (fun v -> n = v.Name) with
//            | Some var' -> var' |> Variable.setProp p v |> Some
//            | None -> None
//        { eq with 
//            Vars = 
//                match var with
//                | Some var' -> 
//                    eq.Vars 
//                    |> Array.replace (fun v -> v.Name = n) var'
//                | None -> eq.Vars }

    /// Return the `string` representation of a `Dto`
    let toString e = 
        let op = if (e |> get).IsProdEq then "*" else "+"
        let varToString = Variable.toString

        match e.Vars |> Array.toList with
        | [] -> ""
        | _::[] -> ""
        | y::xs -> 
            let s = 
                sprintf "%s = " (y |> varToString) + 
                (xs |> List.fold (fun s v -> s + (v |> varToString) + " " + op + " ") "")
            s.Substring(0, s.Length - 2)

    /// Helper function to create a `Variable` from a `dto`
    let fromVarDtoExc dto = 
        try 
            dto |> Variable.fromDtoExc 
        with 
        | Variable.DtoException(m) -> m |> VariableMessage |> raiseExc

    /// Create a `Dto` and raise an exception if it fails
    let fromDtoExc dto =
        let succ = id
        let fail = raiseExc

        match (dto |> get).Vars |> Array.toList with
        | [] -> NoVarsInEquation |> fail
        | y::xs ->
            let y = y |> fromVarDtoExc
            let e = (y, xs |> List.map fromVarDtoExc)
            if dto.IsProdEq then e |> EQ.createProductEq succ (fun m -> m |> EquationMessage |> fail)
            else e |> EQ.createSumEq succ (fun m -> m |> EquationMessage |> fail)

    /// Create a `Dto` from an `Equation` `e`
    let toDto e =
        let c isProd y xs =
            { Vars = y::xs |> List.map Variable.toDto |> List.toArray; IsProdEq = isProd }
        let fp = c true 
        let fs = c false 
        e |> EQ.apply fp fs
