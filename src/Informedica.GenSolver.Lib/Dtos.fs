namespace Informedica.GenSolver.Dtos

open System

open Informedica.GenSolver.Utils

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Variable =
    
    open Informedica.GenSolver.Utils

    module VAR = Informedica.GenSolver.Lib.Variable
    module N = VAR.Name
    module VR = VAR.ValueRange
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
        | ParseFailure of string
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

    let parseOpt succ fail s = 
        if s |> String.IsNullOrWhiteSpace then None |> succ
        else 
            match s |> BigRational.tryParse with
            | Some v -> v |> Some |> succ
            | None   -> s |> ParseFailure |> fail
        
    let toValue succ fail = V.create succ (fun m -> m |> ValueMessage |> fail)

    let toValueSet succ fail toValue vals =
        try
            vals 
            |> Seq.map BigRational.parse
            |> Seq.map toValue
            |> Set.ofSeq
            |> succ
        with 
        | _ -> vals.ToString() |> ParseFailure |> fail

    let fromDtoExc (dto: Dto) =
        let succ = id
        let fail = raiseExc
        
        let n = dto.Name |> N.create succ (fun m -> m |> NameMessage |> fail)
        
        let toValue = toValue succ fail

        let vs = dto.Vals |> toValueSet succ fail toValue

        let minMax c i s = 
            let cr = Option.bind (toValue >> (c i) >> Some)
            s |> parseOpt cr fail

        let min = dto.Min |> minMax VR.createMin dto.MinIncl

        let max = dto.Max |> minMax VR.createMax dto.MaxIncl

        let incr = dto.Incr |> minMax (fun _ v -> v) false

        let vr = VR.create succ (fun m -> m |> ValueRangeMessage |> fail) dto.Unr vs min incr max

        VAR.create succ n vr

    let fromDtoOpt (dto: Dto) =
        let succ = Some
        let fail = Option.none

        let n = dto.Name |> N.create succ (fun m -> m |> NameMessage |> fail)
        
        let toValue = toValue succ fail

        let vs = 
            match dto.Vals |> toValueSet succ fail toValue with
            | Some vs' -> vs' |> Set.filter Option.isSome |> Set.map Option.get
            | None -> Set.empty

        let minMax c i s = 
            let cr = Option.bind (toValue >> (Option.bind(fun v -> v |> c i |> Some)))
            s |> parseOpt cr fail

        let min = dto.Min |> minMax VR.createMin dto.MinIncl

        let max = dto.Max |> minMax VR.createMax dto.MaxIncl

        let incr = dto.Incr |> minMax (fun _ v -> v) false

        let vr = VR.create succ (fun m -> m |> ValueRangeMessage |> fail) dto.Unr vs min incr max

        match n, vr with
        | Some n', Some vr' -> VAR.create succ n' vr'
        | _ -> None


    let toDto (v: VAR.Variable) =
        let optToString = V.optToString

        let dto = createNew (let (N.Name n) = v.Name in n)

        let unr = v.ValueRange |> VR.isUnrestricted

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

module Equation =

    open Informedica.GenSolver.Utils

    module E = Informedica.GenSolver.Lib.Equation
        
    [<CLIMutable>]
    type Dto = { Vars: Variable.Dto[]; IsProdEq: bool }

    type Message =
        | NoVarsInEquation 
        | EquationMessage of E.Message
        | VariableMessage of Variable.Message

    exception DtoException of Message

    let raiseExc m = m |> DtoException |> raise

    let create isProd vars  = { Vars = vars; IsProdEq = isProd }

    let createProd = create true
    let createSum  = create false

    let apply f (dto: Dto) = f dto
    let get = apply id

    /// If equation `eq` contains a variable 
    /// with name `n` then the property `p` of
    /// that variable is updated with value `v`. 
    let setVar n p v eq = 
        let var = 
            match (eq |> get).Vars |> Array.tryFind (fun v -> n = v.Name) with
            | Some var' -> var' |> Variable.setProp p v |> Some
            | None -> None
        { eq with 
            Vars = 
                match var with
                | Some var' -> 
                    eq.Vars 
                    |> Array.replace (fun v -> v.Name = n) var'
                | None -> eq.Vars }

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

    let fromVarDtoExc dto = try dto |> Variable.fromDtoExc with | Variable.DtoException(m) -> m |> VariableMessage |> raiseExc

    let fromDtoExc dto =
        let succ = id
        let fail = raiseExc

        match (dto |> get).Vars |> Array.toList with
        | [] -> NoVarsInEquation |> fail
        | y::xs ->
            let y = y |> fromVarDtoExc
            let e = (y, xs |> List.map fromVarDtoExc)
            if dto.IsProdEq then e |> E.createProductEq succ (fun m -> m |> EquationMessage |> fail)
            else e |> E.createSumEq succ (fun m -> m |> EquationMessage |> fail)

    let toDto e =
        let c isProd y xs =
            { Vars = y::xs |> List.map Variable.toDto |> List.toArray; IsProdEq = isProd }
        let fp = c true 
        let fs = c false 

        e |> E.apply fp fs
