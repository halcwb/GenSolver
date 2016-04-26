#load "load-project.fsx"

#time

open System
open Swensen.Unquote

open Informedica.GenSolver.Utils
open Informedica.GenSolver.Lib

module VAR = Informedica.GenSolver.Dtos.Variable
module E = Informedica.GenSolver.Dtos.Equation
module Solver = Informedica.GenSolver.Lib.Solver

/// Initialize the solver returning a set of equations
let init eqs = 
    let notempty = String.IsNullOrWhiteSpace >> not
    let prodEqs, sumEqs = eqs |> List.partition (String.contains "*")
    let createProdEqs = List.map (E.createProd >> E.fromDtoExc)
    let createSumEqs  = List.map (E.createSum  >> E.fromDtoExc)

    let parse eqs op = 
        eqs 
        |> List.map (String.splitAt '=')
        |> List.map (Array.collect (String.splitAt op))
        |> List.map (Array.map String.trim)
        |> List.map (Array.filter notempty)
        |> List.map (Array.map VAR.createNew)
            
    (parse prodEqs '*' |> createProdEqs) @ (parse sumEqs '+' |> createSumEqs)


/// Print a set of equations to the stdout.
let printEqs eqs = 
    for e in eqs |> List.map E.toDto do 
        printfn "%s" (e |> E.toString)
    printfn "-----"
    eqs    

let solve n p v eqs =
    printfn "Setting variable %s %s with %s" n p v
    eqs 
    |> List.map E.toDto
    |> List.map (E.setVar n p v)
    |> List.map E.fromDtoExc
    |> Solver.solve
    |> printEqs

let nonZeroNegative eqs =
    eqs 
    |> List.map Equation.nonZeroOrNegative
    

let oneCompDrug = 
    init [
        "sub.comp.qty   = sub.comp.conc * comp.total"
        "sub.drug.qty   = sub.comp.conc * comp.qty"
        "sub.drug.qty   = sub.drug.conc * drug.total"
        "sub.dose.qty   = sub.drug.conc * pres.qty"
        "sub.dose.total = sub.dose.qty  * pres.freq" 
        "drug.total     = comp.qty +"
    ] |> nonZeroNegative

oneCompDrug 
|> solve "sub.comp.conc" "vals" "60, 120, 240, 500, 1000" 
|> solve "comp.total" "vals" "1"
|> solve "comp.qty" "vals" "1"
|> solve "sub.dose.total" "maxincl" "2000"
|> solve "sub.dose.total" "minincl" "500"
|> solve "pres.freq" "vals" "2,3,4"
|> solve "pres.qty" "maxincl" "1"
|> solve "pres.qty" "incr" "1/2"
|> ignore

let cardioversion = 
    init [
        "joules = weight * joules.perkg"
    ]

cardioversion 
|> solve "joules" "vals" "1,2,3,5,7,10,20,30,50,70,100,150,200,300,360"
|> solve "weight" "minincl" "3"
|> solve "weight" "maxincl" "150"
|> solve "joules.perkg" "maxincl" "4"
|> solve "weight" "vals" "4"
|> solve "joules" "vals" "10"
|> ignore

let gentconc =
    init [
        "gent.sub.comp.qty = gent.sub.comp.conc * gent.comp.total"
        "gent.sub.drug.qty = gent.sub.comp.conc * gent.comp.qty"
        "gent.sub.drug.qty = gent.sub.drug.conc * drug.total"
        "gent.comp.qty     = gent.ampuls        * gent.comp.total"
        "drug.total        = gent.comp.qty      + sal.comp.qty"
        "gent.sub.drug.qty = gent.dose.kg       * weight"
    ] |> nonZeroNegative

gentconc 
|> solve "gent.sub.comp.qty" "vals" "20,80,400"
|> solve "gent.sub.comp.conc" "vals" "10,40"
|> solve "gent.comp.total" "vals" "2,10"
|> solve "gent.comp.qty" "incr" "1"
|> solve "gent.ampuls" "minincl" "1/2"
|> solve "gent.ampuls" "maxincl" "2"
|> solve "drug.total" "vals" "5,10,20,50,100"
|> solve "gent.sub.drug.conc" "maxincl" "2"
|> solve "gent.dose.kg" "maxincl" "7"
|> solve "gent.dose.kg" "minincl" "5"
|> ignore

let fahrtocels =
    init [
        "fahr = x + const32"
        "cels = x * const5/9"
    ]

fahrtocels
|> solve "const32" "vals" "32"
|> solve "const5/9" "vals" "5/9"
|> solve "fahr" "vals" "0"
|> ignore

let map =
    init [
        "map = x1 + x2"
        "x1 = c1/3 * sbp"
        "x2 = c2/3 * dbp"
    ] |> nonZeroNegative

map
|> solve "c1/3" "vals" "1/3"
|> solve "c2/3" "vals" "2/3"
// Ready to use
|> solve "map" "vals" "80"
|> solve "dbp" "vals" "50"
|> ignore
