#load "load-project.fsx"

open Swensen.Unquote



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Variable =

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
        

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Equation =

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Dto =

        open Informedica.GenSolver.Utils
        
        [<CLIMutable>]
        type Dto = { Vars: Variable.Dto.Dto[]; IsProdEq: bool }

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
                | Some var' -> var' |> Variable.Dto.setProp p v |> Some
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
            let varToString = Variable.Dto.toString

            match e.Vars |> Array.toList with
            | [] -> ""
            | _::[] -> ""
            | y::xs -> 
                let s = 
                    sprintf "%s = " (y |> varToString) + 
                    (xs |> List.fold (fun s v -> s + (v |> varToString) + " " + op + " ") "")
                s.Substring(0, s.Length - 2)



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Solver =    

    open Informedica.GenSolver.Utils

    let init eqs = 
        let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
        let createProdEqs = List.map Equation.Dto.createProd
        let createSumEqs  = List.map Equation.Dto.createSum

        let parse eqs op = 
            eqs 
            |> List.map (String.splitAt '=')
            |> List.map (Array.collect (String.splitAt op))
            |> List.map (Array.map String.trim)
            |> List.map (Array.map Variable.Dto.createNew)
            
        (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)

    let solve eqs n p v = 
        eqs
        |> List.map (Equation.Dto.setVar n p v), false



let printEqs eqs = 
    for e in eqs do printfn "%s" (e |> Equation.Dto.toString)

let eqs = 
    Solver.init 
        [
            "total = freq * quant"
            "quant = time * rate"
            "total = comp.quant1 + comp.quant2"
        ]
eqs |> printEqs

Solver.solve eqs "total" "vals" "1,2,4" |> fst |> printEqs
Solver.solve eqs "total" "min" "1" |> fst |> printEqs
Solver.solve eqs "total" "max" "1" |> fst |> printEqs
Solver.solve eqs "total" "incr" "1" |> fst |> printEqs
Solver.solve eqs "total" "x" "1" |> fst |> printEqs
