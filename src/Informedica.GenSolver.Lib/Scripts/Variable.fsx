#load "load-references.fsx"
#load "load-project.fsx"

module V = Informedica.GenSolver.Lib.Variable.ValueRange.Value
module VR = Informedica.GenSolver.Lib.Variable.ValueRange

let test min1 min1excl incr1 max1 max1excl min2 min2excl incr2 max2 max2excl =
    let toValue c x = 
        match x |> BigRational.FromInt |> V.createOption with
        | Some v -> v |> c |> Some
        | None -> None

    let min1'  = min1 |> toValue (VR.createMin min1excl)
    let max1'  = max1 |> toValue (VR.createMax max1excl) 
    let incr1' = incr1 |> toValue id

    let min2'  = min2 |> toValue (VR.createMin min2excl)
    let max2'  = max2 |> toValue (VR.createMax max2excl) 
    let incr2' = incr2 |> toValue id

    let vr1 = 
        match VR.createOpt Set.empty min1' incr1' max1' with
        | Some vr -> vr
        | None -> VR.empty
    let vr2 = 
        match VR.createOpt Set.empty min2' incr2' max2' with
        | Some vr -> vr
        | None -> VR.empty

    printfn "--------------"
    printfn "%s * %s = %s" (vr1 |> VR.toString) (vr2 |> VR.toString) ((vr1 * vr2) |> VR.toString)
    printfn "%s / %s = %s" (vr1 |> VR.toString) (vr2 |> VR.toString) ((vr1 / vr2) |> VR.toString)
    printfn "%s + %s = %s" (vr1 |> VR.toString) (vr2 |> VR.toString) ((vr1 + vr2) |> VR.toString)
    printfn "%s - %s = %s" (vr1 |> VR.toString) (vr2 |> VR.toString) ((vr1 - vr2) |> VR.toString)
    printfn "--------------"
    true

FsCheck.Check.Quick test

VR.createExc ([1N] |> List.map V.createExc |> Set.ofList) None None None |> VR.getMin
