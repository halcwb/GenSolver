(*** hide ***)
#I "../../src/Informedica.GenSolver.Lib/Scripts"
#load "load-project.fsx"

#time

open Informedica.GenSolver.Utils

module API = Informedica.GenSolver.Api

let procss s = "> " + s + " </br> "|> String.replace "*" "\*"

let printEqs = API.printEqs procss
let solve    = API.solve procss
let init     = API.init
let nonZeroNegative = API.nonZeroNegative

(**
# Medication Calculation Model

A more complex calcultation model involves calculation of medication
*)


(**

A prescription of a drug can have a frequency, quantity and total or/and a rate, quantity and time 
in which the drug is administered. A drug can contain one or more components and each component can 
contain multiple substances. 

This gives rise to the following model: 
*)

let eqs = " = "
let tms = " * "
let add = " + "

let time   = "time"         // Time
let freq   = "freq"         // Frequency
let total  = "prescr.total" // Total prescribed
let qty    = "prescr.qty"   // Quantity prescribed
let rate   = "prescr.rate"  // Rate prescribed

let drug_total = "drug.total" // Total of drug
let drug_qty   = "drug.qty"   // Quantity of drug

let wght = "weight" // Weight adjustment
let bsa  = "bsa"    // Body surface area adjustment

let compSub c comp_total comp_drug_qty s = 
    let sub_comp_qty  = s + "." + c + ".comp.qty"  // Quantity of substance in component
    let sub_comp_conc = s + "." + c + ".comp.conc" // Concentration of substance in component

    let sub_drug_qty  = s + ".drug.qty"  // Quantity of substance in drug
    let sub_drug_conc = s + ".drug.conc" // Concentration of substance in drug

    let sub_dose_qty   = s + ".dose.qty"   // Quantity dose of substance
    let sub_dose_total = s + ".dose.total" // Total dose of substance
    let sub_dose_rate  = s + ".dose.rate"  // Rate dose of substance

    let sub_dose_qty_wght   = s + ".dose.qty.wght"   // Weight adjusted quantity dose of substance
    let sub_dose_total_wght = s + ".dose.total.wght" // Weight adjusted total dose of substance
    let sub_dose_rate_wght  = s + ".dose.rate.wght"  // Weight adjusted rate dose of substance

    let sub_dose_qty_bsa   = s + ".dose.qty.bsa"   // Body surface area adjusted quantity dose of substance
    let sub_dose_total_bsa = s + ".dose.total.bsa" // Body surface area adjusted total dose of substance
    let sub_dose_rate_bsa  = s + ".dose.rate.bsa"  // Body surface area adjusted rate dose of substance


    [
        sub_comp_qty   + eqs + sub_comp_conc + tms + comp_total
        sub_drug_qty   + eqs + sub_drug_conc + tms + drug_total
        sub_drug_qty   + eqs + sub_comp_conc + tms + comp_drug_qty
        sub_dose_total + eqs + sub_dose_qty  + tms + freq 
        sub_dose_qty   + eqs + sub_dose_rate + tms + time
        sub_dose_qty   + eqs + sub_drug_conc + tms + qty
        sub_dose_total + eqs + sub_drug_conc + tms + total
        sub_dose_rate  + eqs + sub_drug_conc + tms + rate

        sub_dose_qty   + eqs + sub_dose_qty_wght   + tms + wght
        sub_dose_total + eqs + sub_dose_total_wght + tms + wght
        sub_dose_rate  + eqs + sub_dose_rate_wght  + tms + wght

        sub_dose_qty   + eqs + sub_dose_qty_bsa   + tms + bsa
        sub_dose_total + eqs + sub_dose_total_bsa + tms + bsa
        sub_dose_rate  + eqs + sub_dose_rate_bsa  + tms + bsa
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

    let comp_dose_qty_wght   = c + ".dose.qty.wght"   // Weight adjusted quantity dose of component
    let comp_dose_total_wght = c + ".dose.total.wght" // Weight adjusted total dose of component
    let comp_dose_rate_wght  = c + ".dose.rate.wght"  // Weight adjusted rate dose of component

    let comp_dose_qty_bsa   = c + ".dose.qty.bsa"   // Body surface area adjusted quantity dose of component
    let comp_dose_total_bsa = c + ".dose.total.bsa" // Body surface area adjusted total dose of component
    let comp_dose_rate_bsa  = c + ".dose.rate.bsa"  // Body surface area adjusted rate dose of component

    [
        drug_total      + eqs + comp_qty       + tms + comp_total
        comp_drug_qty   + eqs + comp_drug_conc + tms + drug_total
        comp_dose_total + eqs + comp_dose_qty  + tms + freq 
        comp_dose_qty   + eqs + comp_dose_rate + tms + time
        comp_dose_qty   + eqs + comp_drug_conc + tms + qty
        comp_dose_total + eqs + comp_drug_conc + tms + total
        comp_dose_rate  + eqs + comp_drug_conc + tms + rate

        comp_dose_qty   + eqs + comp_dose_qty_wght   + tms + wght
        comp_dose_total + eqs + comp_dose_total_wght + tms + wght
        comp_dose_rate  + eqs + comp_dose_rate_wght  + tms + wght

        comp_dose_qty   + eqs + comp_dose_qty_bsa   + tms + bsa
        comp_dose_total + eqs + comp_dose_total_bsa + tms + bsa
        comp_dose_rate  + eqs + comp_dose_rate_bsa  + tms + bsa

    ] |> List.append (sl |> List.collect (compSub c comp_total comp_drug_qty))

let drug cs =   
    [ 
        total + eqs + qty        + tms + freq
        qty   + eqs + rate       + tms + time
        total + eqs + drug_total + tms + drug_qty
    ] 
    |> List.append (cs |> List.collect comp)
    |> List.append [ 
        drug_total + eqs + (cs |> List.fold (fun acc (c, _) -> 
        if acc = "" then c + ".drug.qty" else acc + add + c + ".drug.qty") "")
        ]

    |> init              // Initialize the calculation model
    |> nonZeroNegative   // Set all variables to only contain non zero positive values

(** 
For a 'simple' one component, one substance drug like a paracetamol supp, the following 
equantions are generated:
*)

let paracetamol = [("supp", ["paracetamol"])] |> drug

paracetamol 
|> printEqs
|> ignore

(** 

> paracetamol.supp.comp.qty<0..> = paracetamol.supp.comp.conc<0..> \* supp.comp.total<0..>  </br> 
> paracetamol.drug.qty<0..> = paracetamol.drug.conc<0..> \* drug.total<0..>  </br> 
> paracetamol.drug.qty<0..> = paracetamol.supp.comp.conc<0..> \* supp.drug.qty<0..>  </br> 
> paracetamol.dose.total<0..> = paracetamol.dose.qty<0..> \* freq<0..>  </br> 
> paracetamol.dose.qty<0..> = paracetamol.dose.rate<0..> \* time<0..>  </br> 
> paracetamol.dose.qty<0..> = paracetamol.drug.conc<0..> \* prescr.qty<0..>  </br> 
> paracetamol.dose.total<0..> = paracetamol.drug.conc<0..> \* prescr.total<0..>  </br> 
> paracetamol.dose.rate<0..> = paracetamol.drug.conc<0..> \* prescr.rate<0..>  </br> 
> paracetamol.dose.qty<0..> = paracetamol.dose.qty.wght<0..> \* weight<0..>  </br> 
> paracetamol.dose.total<0..> = paracetamol.dose.total.wght<0..> \* weight<0..>  </br> 
> paracetamol.dose.rate<0..> = paracetamol.dose.rate.wght<0..> \* weight<0..>  </br> 
> paracetamol.dose.qty<0..> = paracetamol.dose.qty.bsa<0..> \* bsa<0..>  </br> 
> paracetamol.dose.total<0..> = paracetamol.dose.total.bsa<0..> \* bsa<0..>  </br> 
> paracetamol.dose.rate<0..> = paracetamol.dose.rate.bsa<0..> \* bsa<0..>  </br> 
> drug.total<0..> = supp.comp.qty<0..> \* supp.comp.total<0..>  </br> 
> supp.drug.qty<0..> = supp.drug.conc<0..> \* drug.total<0..>  </br> 
> supp.dose.total<0..> = supp.dose.qty<0..> \* freq<0..>  </br> 
> supp.dose.qty<0..> = supp.dose.rate<0..> \* time<0..>  </br> 
> supp.dose.qty<0..> = supp.drug.conc<0..> \* prescr.qty<0..>  </br> 
> supp.dose.total<0..> = supp.drug.conc<0..> \* prescr.total<0..>  </br> 
> supp.dose.rate<0..> = supp.drug.conc<0..> \* prescr.rate<0..>  </br> 
> supp.dose.qty<0..> = supp.dose.qty.wght<0..> \* weight<0..>  </br> 
> supp.dose.total<0..> = supp.dose.total.wght<0..> \* weight<0..>  </br> 
> supp.dose.rate<0..> = supp.dose.rate.wght<0..> \* weight<0..>  </br> 
> supp.dose.qty<0..> = supp.dose.qty.bsa<0..> \* bsa<0..>  </br> 
> supp.dose.total<0..> = supp.dose.total.bsa<0..> \* bsa<0..>  </br> 
> supp.dose.rate<0..> = supp.dose.rate.bsa<0..> \* bsa<0..>  </br> 
> prescr.total<0..> = prescr.qty<0..> \* freq<0..>  </br> 
> prescr.qty<0..> = prescr.rate<0..> \* time<0..>  </br> 
> prescr.total<0..> = drug.total<0..> \* drug.qty<0..>  </br> 
> drug.total<0..> = supp.drug.qty<0..>  </br> 

*)

(** 
## Using the model

Next we can start using the model.
*)

(** 
A paracetamol supp can have a set of quanities, like: 50, 60, 120, 240, 500 and 1000 mg. 
And a supp has a total of 1. As the drug contains only the supp, the drug total equals 
the compoment total and is 1.
*)

paracetamol
|> solve "paracetamol.supp.comp.qty" "vals" "50, 60, 120,240, 500, 1000"
|> solve "supp.comp.total" "vals" "1"
|> solve "supp.drug.qty" "vals" "1"


(** 
Lets assume that patients normally tolerate one supp at a time. So, the prescription quantity can 
be set to 1. Also, the frequency of administration will be around, 1,2,3,4 or 6 times daily.
*)

|> solve "prescr.qty" "vals" "1"
|> solve "freq" "vals" "1,2,3,4,6"

(** 
The maximum paracetamol dose is 4000 mg/day. For children the dose is weight adjusted and should 
not exceed 90 mg/kg/day
*)

|> solve "paracetamol.dose.total" "maxincl" "4000"
|> solve "paracetamol.dose.total.wght" "maxincl" "90"

(** 
Suppose we want to prescribe paracetamol with this setup to a child of 6 kg. With a frequency of 
3 or 4 times/day.
*)

|> solve "weight" "vals" "6"
|> solve "freq" "vals" "3,4"
|> printEqs
|> ignore

(** 

> paracetamol.supp.comp.qty[50, 60, 120] = paracetamol.supp.comp.conc[50, 60, 120] \* supp.comp.total[1]  </br> 
> paracetamol.drug.qty[50, 60, 120] = paracetamol.drug.conc[50, 60, 120] \* drug.total[1]  </br> 
> paracetamol.drug.qty[50, 60, 120] = paracetamol.supp.comp.conc[50, 60, 120] \* supp.drug.qty[1]  </br> 
> paracetamol.dose.total[150, 180, 200, 240, 360, 480] = paracetamol.dose.qty[50, 60, 120] \* freq[3, 4]  </br> 
> paracetamol.dose.qty[50, 60, 120] = paracetamol.dose.rate<0..> \* time<0..>  </br> 
> paracetamol.dose.qty[50, 60, 120] = paracetamol.drug.conc[50, 60, 120] \* prescr.qty[1]  </br> 
> paracetamol.dose.total[150, 180, 200, 240, 360, 480] = paracetamol.drug.conc[50, 60, 120] \* prescr.total[3, 4]  </br> 
> paracetamol.dose.rate<0..> = paracetamol.drug.conc[50, 60, 120] \* prescr.rate<0..>  </br> 
> paracetamol.dose.qty[50, 60, 120] = paracetamol.dose.qty.wght[25/3, 10, 20] \* weight[6]  </br> 
> paracetamol.dose.total[150, 180, 200, 240, 360, 480] = paracetamol.dose.total.wght[25, 30, 100/3, 40, 60, 80] \* weight[6]  </br> 
> paracetamol.dose.rate<0..> = paracetamol.dose.rate.wght<0..> \* weight[6]  </br> 
> paracetamol.dose.qty[50, 60, 120] = paracetamol.dose.qty.bsa<0..> \* bsa<0..>  </br> 
> paracetamol.dose.total[150, 180, 200, 240, 360, 480] = paracetamol.dose.total.bsa<0..> \* bsa<0..>  </br> 
> paracetamol.dose.rate<0..> = paracetamol.dose.rate.bsa<0..> \* bsa<0..>  </br> 
> drug.total[1] = supp.comp.qty[1] \* supp.comp.total[1]  </br> 
> supp.drug.qty[1] = supp.drug.conc[1] \* drug.total[1]  </br> 
> supp.dose.total[3, 4] = supp.dose.qty[1] \* freq[3, 4]  </br> 
> supp.dose.qty[1] = supp.dose.rate<0..> \* time<0..>  </br> 
> supp.dose.qty[1] = supp.drug.conc[1] \* prescr.qty[1]  </br> 
> supp.dose.total[3, 4] = supp.drug.conc[1] \* prescr.total[3, 4]  </br> 
> supp.dose.rate<0..> = supp.drug.conc[1] \* prescr.rate<0..>  </br> 
> supp.dose.qty[1] = supp.dose.qty.wght[1/6] \* weight[6]  </br> 
> supp.dose.total[3, 4] = supp.dose.total.wght[1/2, 2/3] \* weight[6]  </br> 
> supp.dose.rate<0..> = supp.dose.rate.wght<0..> \* weight[6]  </br> 
> supp.dose.qty[1] = supp.dose.qty.bsa<0..> \* bsa<0..>  </br> 
> supp.dose.total[3, 4] = supp.dose.total.bsa<0..> \* bsa<0..>  </br> 
> supp.dose.rate<0..> = supp.dose.rate.bsa<0..> \* bsa<0..>  </br> 
> prescr.total[3, 4] = prescr.qty[1] \* freq[3, 4]  </br> 
> prescr.qty[1] = prescr.rate<0..> \* time<0..>  </br> 
> prescr.total[3, 4] = drug.total[1] \* drug.qty[3, 4]  </br> 
> drug.total[1] = supp.drug.qty[1]  </br> 

*)

(** 
From the output it can be deduced that:

* The supp's that can be used are the 50, 60 and 120 mg supps
* The maximum total dose is 480 mg/day
* The maximum weight adjusted total dose is 80 mg/kg/day

*)