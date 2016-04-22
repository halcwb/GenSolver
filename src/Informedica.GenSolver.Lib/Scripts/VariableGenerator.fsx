#load "load-references.fsx"
#load "load-project.fsx"

open FsCheck

module VAR = Informedica.GenSolver.Lib.Variable
module N = VAR.Name
module VR = VAR.ValueRange
module V = VR.Value


/// Create the necessary test generators
module Generators =

    let intIntToBigR (n, d) = 
            let n = if n = 0UL then 1UL else n
            let d = if d = 0UL then 1UL else d
            let n' = n.ToString() |> BigRational.Parse
            let d' = d.ToString() |> BigRational.Parse
            n'/d'

    let bigRGenerator =
        gen {
            let! n = Arb.generate<uint64>
            let! d = Arb.generate<uint64>
            return intIntToBigR(n, d)
        }

    let valueGenerator = 
        gen {
            let! x = Arb.generate<BigRational>
            return V.createExc x
        }

    let valueRange vs v1 minexcl incr v2 maxexcl =
        let v1, v2 = if v1 > v2 then v2, v1 else v1, v2

        let  min = v1 |> Option.bind ((VR.createMin minexcl) >> Some) 
        let  max = v2 |> Option.bind ((VR.createMax maxexcl) >> Some) 
             
        match min, incr, max with
        | Some _, None, None
        | None, None, Some _
        | Some _, Some _, None
        | Some _, None, Some _ ->
            match VR.createOpt Set.empty min incr max with
            | Some vr -> vr
            | None -> VR.unrestricted
        | _ ->            
            match VR.createOpt vs min incr max with
            | Some vr -> 
                if vr |> VR.isEmpty then 
                    match VR.createOpt Set.empty min incr max with
                    | Some vr -> if vr |> VR.isEmpty then VR.unrestricted else vr 
                    | None -> VR.unrestricted
                else vr
            | None    -> VR.unrestricted
        

    let valueRangeGenerator =
        gen {
            let! vs = Arb.generate<V.Value Set>
            let! v1 = Arb.generate<V.Value Option>
            let! v2 = Arb.generate<V.Value Option>
            let! minexcl = Arb.generate<bool>
            let! maxexcl = Arb.generate<bool>
            let! incr = Arb.generate<V.Value Option>

            return valueRange vs v1 minexcl incr v2 maxexcl
        }

    type MyGenerators () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator }

        static member Value () =
            { new Arbitrary<V.Value>() with
                override x.Generator = valueGenerator }

        static member ValueRange () =
            { new Arbitrary<VR.ValueRange>() with
                override x.Generator = valueRangeGenerator }


module TestCases =

    Arb.register<Generators.MyGenerators>() |> ignore

    let cases = new ResizeArray<_>()

    let toString cs =
        let rec parse cs s =
            match cs with
            | [] -> s
            | c::tail -> 
                s + ", " + (c |> VR.toString) |> parse tail

        "" |> parse (cs |> List.ofSeq)

    let generate () =
        let test (v1: VR.ValueRange) v2 v3 = [v1;v2;v3] |> cases.Add; true

        Check.One({ Config.Quick with MaxTest = 1000 }, test)

        let results' =
            new System.Collections.Generic.HashSet<_>(cases, HashIdentity.Structural)
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
        """
        printfn "%s" text
        printfn "Number of unique test cases: %i\n" (results' |> List.length)
        let i = ref 0
        for r in results' do
            i := !i + 1
            printfn "___________ Test %i ______________" (!i)
            printfn ""
            printfn "%s" (r |> toString)

    let run pred =
        let failed = ref false
        let tests =
            cases 
            |> Seq.map(fun c -> c |> pred, c)

        for test in tests do
            if test |> fst |> not then
                printfn "Failed test with: %s" (test |> snd |> toString)
                failed := true

TestCases.run (fun rl ->
    let rs = (rl.[0] != rl.[1] * rl.[2]) 
    if rs|> VR.isEmpty then
        printfn "Result: %s" (rs |> VR.toString)
        false
    else true
)
