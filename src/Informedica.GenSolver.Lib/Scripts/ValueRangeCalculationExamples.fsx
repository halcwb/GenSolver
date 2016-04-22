#load "load-references.fsx"
#load "load-project.fsx"

module V = Informedica.GenSolver.Lib.Variable.ValueRange.Value
module VR = Informedica.GenSolver.Lib.Variable.ValueRange

let results = new ResizeArray<_>()

let genList unr1 unr2 unr3 vs1 vs2 vs3 min1 min1excl incr1 max1 max1excl min2 min2excl incr2 max2 max2excl min3 min3excl incr3 max3 max3excl =

    let toValue c x = 
        match x |> BigRational.FromInt |> V.createOption with
        | Some v -> v |> c |> Some
        | None -> None

    let toVs vs =
        vs
        |> List.map (toValue id)
        |> List.filter Option.isSome
        |> List.map Option.get
        |> Set.ofList

    let vs1' = vs1 |> toVs
    let vs2' = vs2 |> toVs
    let vs3' = vs3 |> toVs

    let min1'  = min1 |> toValue (VR.createMin min1excl)
    let max1'  = max1 |> toValue (VR.createMax max1excl) 
    let incr1' = incr1 |> toValue id

    let min2'  = min2 |> toValue (VR.createMin min2excl)
    let max2'  = max2 |> toValue (VR.createMax max2excl) 
    let incr2' = incr2 |> toValue id

    let min3'  = min3 |> toValue (VR.createMin min3excl)
    let max3'  = max3 |> toValue (VR.createMax max3excl) 
    let incr3' = incr3 |> toValue id

    let vr1 = 
        if unr1 then VR.unrestricted
        else
            match VR.createOpt vs1' min1' incr1' max1' with
            | Some vr -> vr
            | None -> VR.empty

    let vr2 = 
        if unr2 then VR.unrestricted
        else
            match VR.createOpt vs2' min2' incr2' max2' with
            | Some vr -> vr
            | None -> VR.empty

    let vr3 = 
        if unr3 then VR.unrestricted
        else
            match VR.createOpt vs3' min3' incr3' max3' with
            | Some vr -> vr
            | None -> VR.empty

    let vr1s, vr2s, vr3s = (vr1 |> VR.toString), (vr2 |> VR.toString), (vr3 |> VR.toString)
    
    let calcToStr ops op =
        let expr = vr1 |> op <| vr2 
        let y = vr3 != expr |> VR.toString
        let res = expr |> VR.toString
        sprintf "%s %s %s = %s -> %s\n" vr1s ops vr2s res y 

    let s =
        sprintf "y = %s\n" vr3s +
        sprintf "----------------------------------------------------------\n" +
        calcToStr "*" (*) +
        calcToStr "/" (/) +
        calcToStr "+" (+) +
        calcToStr "-" (-)

    results.Add(s)
    true

let run () =
    FsCheck.Check.Quick genList
    let results' =
        new System.Collections.Generic.HashSet<_>(results, HashIdentity.Structural)
        |> List.ofSeq

    let text = """
[] means an empty set cannot contain any value
<0..> means an unrestricted set that can contain any value > 0
<1..> means a restricted set with only values > 3
[3..> means a restricted set with only values >= 3
[4..2..> means a restriced set only values >= 4 and each value a multiple of 2
<0..3> means a restricted set with only values > 0 and < 3 
<0..3] means a restricted set with only values > 0 and <= 3 
[1,3,5] means a restriced set with only those values

The result of the expression will be applied to y (at the top of each test)
denoted by the ->
    """
    printfn "%s" text
    printfn "Number of unique test cases: %i\n" (results' |> List.length)
    let i = ref 0
    for r in results' do
        i := !i + 1
        printfn "___________ Test %i ______________" (!i)
        printfn ""
        printfn "%s" r

