#load "load-project.fsx"

#time

open System
open Swensen.Unquote

open Informedica.GenSolver.Utils

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
    eqs 
    |> List.map E.toDto
    |> List.map (E.setVar n p v)
    |> List.map E.fromDtoExc
    |> Solver.solve
    |> printEqs


//    |> add [ compQty;       compConc;      drugTotal   ] ProductEquation
//    |> add [ compDose;      compConc;      prescrQty   ] ProductEquation
//    |> add [ compDoseTotal; compConc;      prescrTotal ] ProductEquation
//    |> add [ compDoseRate;  compConc;      prescrRate  ] ProductEquation
//    |> add [ compDose;      compDoseRate;  time        ] ProductEquation
//    |> add [ compDoseTotal; compDose;      freq        ] ProductEquation


//    |> add [ substDrugQty; substDrugConc; drugTotal   ] ProductEquation
//    |> add [ substDrugQty; substCompConc; compQty     ] ProductEquation
//    |> add [ substCompQty; substCompConc; compTotal   ] ProductEquation
//    |> add [ doseTotal;    freq;          doseQty     ] ProductEquation
//    |> add [ doseQty;      time;          doseRate    ] ProductEquation
//    |> add [ doseRate;     substDrugConc; prescrRate  ] ProductEquation
//    |> add [ doseQty;      substDrugConc; prescrQty   ] ProductEquation
//    |> add [ doseTotal;    substDrugConc; prescrTotal ] ProductEquation

let oneCompDrug = 
    init [
        "sub.comp.qty   = sub.comp.conc * comp.total"
        "sub.drug.qty   = sub.comp.conc * comp.qty"
        "sub.drug.qty   = sub.drug.conc * drug.total"
        "sub.dose.qty   = sub.drug.conc * pres.qty"
        "sub.dose.total = sub.dose.qty  * pres.freq" 
        "drug.total     = comp.qty +"
    ]

let res =
    oneCompDrug 
    |> solve "sub.comp.conc" "vals" "60, 120, 240, 500, 1000" 
    |> solve "comp.total" "vals" "1"
    |> solve "comp.qty" "vals" "1"
    |> solve "sub.dose.total" "maxincl" "2000"
    |> solve "sub.dose.total" "minincl" "500"
    |> solve "pres.freq" "vals" "2,3,4"
    |> solve "pres.qty" "maxincl" "1"
    |> solve "pres.qty" "incr" "1/2"

// [res.[1]] |> Solver.solve 
//    eqs |> Solver.solve

//Solver.solve eqs "total" "vals" "1,2,4" |> fst |> printEqs
//Solver.solve eqs "total" "min" "1" |> fst |> printEqs
//Solver.solve eqs "total" "max" "1" |> fst |> printEqs
//Solver.solve eqs "total" "incr" "1" |> fst |> printEqs
//Solver.solve eqs "total" "x" "1" |> fst |> printEqs

