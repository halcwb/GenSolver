#load "load-project.fsx"

#time

open System
open Swensen.Unquote

open Informedica.GenSolver.Api 

let eqs = " = "
let tms = " * "

let time   = "time"
let freq   = "freq"
let total  = "prescr.total"
let qty    = "prescr.qty"
let rate   = "prescr.rate"

let drug_total = "drug.total"
let drug_qty   = "drug.qty"

let compSub c comp_total s = 
    let sub_comp_qty  = s + "." + c + ".comp.qty"  // Quantity of substance in component
    let sub_comp_conc = s + "." + c + ".comp.conc" // Concentration of substance in component

    let sub_drug_qty  = s + ".drug.qty"  // Quantity of substance in drug
    let sub_drug_conc = s + ".drug.conc" // Concentration of substance in drug

    let sub_dose_qty   = s + ".dose.qty"   // Quantity dose of substance
    let sub_dose_total = s + ".dose.total" // Total dose of substance
    let sub_dose_rate  = s + ".dose.rate"  // Rate dose of substance

    [
        sub_comp_qty   + eqs + sub_comp_conc + tms + comp_total
        sub_drug_qty   + eqs + sub_drug_conc + tms + drug_total
        sub_dose_total + eqs + sub_dose_qty  + tms + freq 
        sub_dose_qty   + eqs + sub_dose_rate + tms + time
        sub_dose_qty   + eqs + sub_drug_conc + tms + qty
        sub_dose_total + eqs + sub_drug_conc + tms + total
        sub_dose_rate  + eqs + sub_drug_conc + tms + rate
    ]

let comp cs =
    let c, sl = cs
    let comp_qty   = c + ".comp.qty"   // Quantity of component
    let comp_total = c + ".comp.total" // Total of component

    let comp_drug_qty  = c + ".drug.qty"  // Quantity of component in drug
    let comp_drug_conc = c + ".drug.conc" // Concentration of component in drug

    let comp_dose_qty   = c + ".dose.qty"   // Quantity dose of component
    let comp_dose_total = c + ".dose.total" // Total dose of component
    let comp_dose_rate  = c + ".dose.rate"  // Rate dose of component

    [
        drug_total      + eqs + comp_qty       + tms + comp_total
        comp_drug_qty   + eqs + comp_drug_conc + tms + drug_total
        comp_dose_total + eqs + comp_dose_qty  + tms + freq 
        comp_dose_qty   + eqs + comp_dose_rate + tms + time
        comp_dose_qty   + eqs + comp_drug_conc + tms + qty
        comp_dose_total + eqs + comp_drug_conc + tms + total
        comp_dose_rate  + eqs + comp_drug_conc + tms + rate
    ] |> List.append (sl |> List.collect (compSub c comp_total))

let drug cs =   
    [ 
        total + eqs + qty        + tms + freq
        qty   + eqs + rate       + tms + time
        total + eqs + drug_total + tms + drug_qty
    ] |> List.append (cs |> List.collect comp)

    |> init
    |> nonZeroNegative

let paracetamol = [("paracetamol", ["paracetamol.supp"])] |> drug


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
|> solve "gent.ampuls" "minincl" "1/2"
|> solve "gent.ampuls" "maxincl" "2"
|> solve "gent.sub.drug.conc" "maxincl" "2"
|> solve "gent.dose.kg" "maxincl" "7"
|> solve "gent.dose.kg" "minincl" "5"
|> solve "gent.sub.comp.qty" "vals" "20,80,400"
|> solve "gent.sub.comp.conc" "vals" "10,40"
|> solve "gent.comp.total" "vals" "2,10"
|> solve "drug.total" "vals" "5,10,20,50,100"
//|> solve "gent.comp.qty" "incr" "1/10"
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
|> solve "map" "vals" "80"
|> solve "dbp" "vals" "50"
|> ignore

        