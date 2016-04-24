namespace Informedica.GenSolver.Lib.Dtos

open System

open Informedica.GenSolver.Utils

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Variable =
    
    open Informedica.GenSolver.Utils
    open Informedica.GenSolver.Lib

    module VAR = Variable
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

        let fVariable n vr = VAR.create succ n vr

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
            | Some n', Some vr' -> VAR.create succ n' vr'
            | _ -> None

        fromDto fail fName fValueRange fVariable

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
    open Informedica.GenSolver.Lib

    module E = Equation
    module VAR = Variable
        
    [<CLIMutable>]
    type Dto = { Vars: Variable.Dto[]; IsProdEq: bool }

    type Message =
        | NoVarsInEquation 
        | DuplicateVariables of Equation.Message

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

//    let fromDto succ fail dto =
//        match (dto |> get).Vars with
//        | [] -> 

module Solver =

    /// Initialize the solver returning a set of equations
    let init eqs = 
        let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
        let createProdEqs = List.map Equation.createProd
        let createSumEqs  = List.map Equation.createSum

        let parse eqs op = 
            eqs 
            |> List.map (String.splitAt '=')
            |> List.map (Array.collect (String.splitAt op))
            |> List.map (Array.map String.trim)
            |> List.map (Array.map Variable.createNew)
            
        (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)


    /// Print a set of equations to the stdout.
    let printEqs eqs = 
        for e in eqs do printfn "%s" (e |> Equation.toString)

