namespace Informedica.GenSolver.Tests

open Informedica.GenSolver.Lib
open Swensen.Unquote
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

/// Create the necessary test generators
module Generators =

    let bigRGen (n, d) = 
            let d = if d = 0 then 1 else d
            let n' = abs(n) |> BigRational.FromInt
            let d' = abs(d) |> BigRational.FromInt
            n'/d'

    let bigRGenerator =
        gen {
            let! n = Arb.generate<int>
            let! d = Arb.generate<int>
            return bigRGen(n, d)
        }

    type MyGenerators () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator }


[<SetUpFixture>]
type Config () =
    
    /// Make sure the generators are
    /// registered before running any
    /// test code.
    [<SetUp>]
    member x.Setup () = 

        Arb.register<Generators.MyGenerators>() |> ignore
        
module Testing =

    module Variable =

        module Value =

            open Variable

            type valueExc = ValueRange.Value.ValueException

            let raiseValueExc v = v |> ValueRange.Value.ValueException |>  raise

            let fs = id
            let ff = fun v -> v |> raiseValueExc

            let create = ValueRange.Value.create fs ff
            let calc = ValueRange.Value.calc fs ff
            let get = ValueRange.Value.get
            let zero = 0N

            [<TestFixture>]
            type ``The create function`` () =

                [<Property>]
                member x.``Will not create negative values`` () =
                    let nonZeroOrNegative x =
                        try
                            (x 
                            |> BigRational.FromInt
                            |> create
                            |> get) > zero 
                        with 
                        | _ -> true
                    
                    nonZeroOrNegative
                    
            [<TestFixture>]
            type ``Given a zero or negative number`` () =

                [<Test>]
                member x.``the value is passed to the failure function`` () =
                    raises<valueExc> <@ 0N  |> create @>
                    raises<valueExc> <@ -1N |> create @>

            [<TestFixture>]
            type ``Given a non zero positive value`` () =
                [<Test>]
                member x.``A value can be created`` () =
                    test<@ create 1N |> get = 1N @>
        
            [<TestFixture>]
            type ``Given an infix operand`` () =
            
                [<Test>]
                member x.``The operand gives the same result as applied to BigRationals`` () =
                    let v1 = 1N |> create
                    let v2 = 1N |> create
                    test <@ calc (+) v1 v2 |> get = 2N @>
                    test <@ (v1 + v2) |> get = 2N @>

            [<TestFixture>]
            type ``Is overloaded with`` () =
                let create = BigRational.FromInt >> create
                let get    = get >> BigRational.ToInt32

                let check op1 op2 x1 x2 =
                    if x1 > 0 && x2 > 0 then
                        let x1' = x1 |> create
                        let x2' = x2 |> create
                        (x1' |> op1 <| x2') |> get = (x1 |> op2 <| x2)
                    else true
                

                [<Property>]
                member x.``Basic arrhythmic functions`` () =
                    let checkMult = check (*) (*)

                    let checkDiv = check (/) (/)

                    let checkAdd = check (+) (+)

                    let checkSubtr x1 x2 =
                        if x1 > x2 then check (-) (-) x1 x2
                        else true

                    fun (m1, m2, d1, d2, a1, a2, s1, s2) ->
                        checkMult m1 m2 &&
                        checkDiv d1 d2 &&
                        checkAdd a1 a2 &&
                        checkSubtr s1 s2
        
            [<TestFixture>]
            type ``Given a negative subtraction result`` () =
                
                [<Test>]
                member x.``A NonZeroOrPositive error is thrown`` () =
                    let v1 = create 1N
                    let v2 = create 2N
                    raises<valueExc> <@ v1 - v2 @> 

        module ValueRange =

            open  Variable

            let fs = id
            let ff = fun msg -> msg |> ValueRange.raiseExc

            let createVal = ValueRange.Value.create id (fun _ -> failwith "Cannot create")
            let createValOpt = ValueRange.Value.create Some (fun _ -> None)
            let create min max vs = Variable.ValueRange.create fs ff vs min max

            let getMin  = ValueRange.getMin >> Option.bind (ValueRange.minToValue >> Some)
            let getMax  = ValueRange.getMax >> Option.bind (ValueRange.maxToValue >> Some)

            let contains v vr = 
                let min = vr |> ValueRange.getMin
                let max = vr |> ValueRange.getMax
                v |> ValueRange.contains min max

            let containsProp pred vr v =
                if v > 0N && v |> pred then 
                    printfn "Testing: %A" v
                    vr  |> contains (v |> createVal)
                else true
        
            [<TestFixture>]
            type ``Given list = empty min = None max = None`` () =
                let min = None
                let max = None

                let empty = create min max Set.empty

                [<Test>]
                member x.``Creating values returns an unrestricted valueset`` () =
                    test <@ empty = ValueRange.empty @>
        
                [<Test>]
                member x.``Counting values returns zero`` () =
                    test <@ empty |> ValueRange.count = 0 @>

                [<Property>]
                member x.``The set contains any Value`` () =
                    containsProp (fun _ -> true) empty

            [<TestFixture>]
            type ``Given list with one value min = None max = None`` () =
                let min = None
                let max = None
                // List with one value
                let v = 1N |> createVal
                let vs = 
                    Set.empty
                    |> Set.add v
                let vr = vs |> create min max

                [<Test>]
                member x.``Counting values returns one`` () =
                    test <@ vr |> ValueRange.count = 1 @>
        
                [<Test>]
                member x.``The result contains that value`` () =
                    test <@ vr |> contains v @>


            [<TestFixture>]
            type ``Given empty list min = Some 1N max = None`` () =
                let min = 1N |> createValOpt |> Option.bind (ValueRange.createMinIncl >> Some)
                let max = None
                // List with one value
                let vs = Set.empty
                let vr = create min max vs

                [<Test>]
                member x.``Values contain no values`` () =
                    test <@ vr |> Variable.ValueRange.count = 0 @>

                [<Test>]
                member x.``Minimum is one`` () =
                    test <@ vr |> ValueRange.getMin = min @>

                [<Property>]
                member x.``The result can contain any value greater or equal to one`` () =
                    containsProp ((<=) 1N) vr

            [<TestFixture>]
            type ``Given empty list min = None max = Some 1`` () =
                let min = None
                let max = 1N |> createValOpt |> Option.bind (ValueRange.createMaxIncl >> Some)
                // List with one value
                let vs = Set.empty
                let vr = create min max vs

                [<Test>]
                member x.``Values contain no values`` () =
                    test <@ vr |> Variable.ValueRange.count = 0 @>

                [<Test>]
                member x.``Maximum is one`` () =
                    test <@ vr |> ValueRange.getMax = max @>

                [<Property>]
                member x.``The result can contain any value less or equal to one`` () =
                    containsProp ((>=) 1N) vr


            [<TestFixture>]
            type ``Given empty list min = Some 2N max = Some 4N`` () =
                let incr = None
                let min = 1N |> createValOpt |> Option.bind (ValueRange.createMinIncl >> Some)
                let max = 4N |> createValOpt |> Option.bind (ValueRange.createMaxIncl >> Some)
                // List with one value
                let vals = Set.empty

                [<Test>]
                member x.``Values contain no values`` () =
                    test <@ create min max vals |> Variable.ValueRange.count = 0 @>

                [<Test>]
                member x.``Increment is one and minimum is two`` () =
                    test <@ create min max vals |> ValueRange.getMin = min @>
                    test <@ create min max vals |> ValueRange.getMax = max @>


            [<TestFixture>]
            type ``Given a list of Value`` () =

                let ff = fun _ -> failwith "Cannot create"

                let createVals ns =
                    ns
                    |> List.map (ValueRange.Value.create id ff)
                    |> Set.ofList
    
                [<Property>]
                member x.``The resulting ValueSet contains an equal amount`` () =
                    let equalCount c =
                        if c >= 1 then
                            [1..c] 
                            |> List.map BigRational.FromInt
                            |> createVals
                            |> Set.count = c
                        else true

                    equalCount

                [<Test>]
                member x.``Can filter by min and max`` () =
                    // Check values filter
                    let vsincr = [1N..1N..10N] |> createVals
                    let min = createVal 4N |> Some |> Option.bind (ValueRange.createMinIncl >> Some)
                    let max = createVal 8N |> Some |> Option.bind (ValueRange.createMaxIncl >> Some)
                    test <@ ValueRange.filter None None vsincr = vsincr @>
                    test <@ ValueRange.filter None None vsincr = ([1N..1N..10N] |> createVals) @>
                    test <@ ValueRange.filter min  None vsincr = ([4N..1N..10N] |> createVals) @>
                    test <@ ValueRange.filter min  max  vsincr = ([4N..1N..8N] |> createVals) @>
                    
            [<TestFixture>]
            type ``Given addition multiplication or division of two value sets`` () =
                    
                let ff = fun _ -> failwith "Cannot create"

                let createVals ns =
                    ns
                    |> List.map BigRational.FromInt
                    |> List.map (ValueRange.Value.create id ff)
                    |> Set.ofList
    

                [<Test>]
                member x.``The resultset will be a distinct set of calculated values`` () =

                    let create = createVals >> Variable.ValueRange.ValueSet

                    let checkAdd l1 l2 =
                        // Only values > 0
                        let l1 = l1 |> List.filter ((<) 0)
                        let l2 = l2 |> List.filter ((<) 0)

                        let add =
                            [ for x1 in l1 do
                                for x2 in l2 do
                                    yield x1 + x2 ]
                            |> List.toSeq
                            // List will only contain distinct values
                            |> Seq.distinct
                            |> Seq.toList
                        let l1' = l1 |> create
                        let l2' = l2 |> create
                        (l1' + l2') |> Variable.ValueRange.count = add.Length

                    let checkMult l1 l2 =
                        // Only values > 0
                        let l1 = l1 |> List.filter ((<) 0)
                        let l2 = l2 |> List.filter ((<) 0)

                        let mult =
                            [ for x1 in l1 do
                                for x2 in l2 do
                                    yield x1 * x2 ]
                            |> List.toSeq
                            // List will only contain distinct values
                            |> Seq.distinct
                            |> Seq.toList
                        let l1' = l1 |> create
                        let l2' = l2 |> create
                        (l1' * l2') |> Variable.ValueRange.count = mult.Length

                    let checkDiv l1 l2 =
                        // Only values > 0
                        let l1 = l1 |> List.filter ((<) 0) |> List.map BigRational.FromInt
                        let l2 = l2 |> List.filter ((<) 0) |> List.map BigRational.FromInt

                        let create =
                            List.map (ValueRange.Value.create id (fun _ -> failwith "Could not create"))
                            >> Set.ofList
                            >> ValueRange.ValueSet

                        let div =
                            [ for x1 in l1 do
                                for x2 in l2 do
                                    yield x1 / x2 ]
                            |> List.toSeq
                            // List will only contain distinct values
                            |> Seq.distinct
                            |> Seq.toList
                        let l1' = l1 |> create
                        let l2' = l2 |> create

                        (l1' / l2') |> Variable.ValueRange.count = div.Length

                    Check.Quick checkAdd
                    Check.Quick checkMult
                    Check.Quick checkDiv

            [<TestFixture>]
            type ``Given subtraction of two value sets`` () =
                    
                let ff = fun _ -> failwith "Cannot create"

                let createVals ns =
                    let create vs = ValueRange.create id ff vs None None

                    ns
                    |> List.map BigRational.FromInt
                    |> List.map (ValueRange.Value.create id ff)
                    |> Set.ofList
                    |> create

                [<Test>]
                member x.``The resultset will be a distinct set of only positive values`` () =
                    let checkSubtr l1 l2 =
                        // Only values > 0
                        let l1 = l1 |> List.filter ((<) 0)
                        let l2 = l2 |> List.filter ((<) 0)

                        let create = createVals
                        let subtr =
                            [ for x1 in l1 do
                                for x2 in l2 do
                                    yield x1 + x2 ]
                            |> List.toSeq
                            // List will only contain distinct values
                            |> Seq.distinct
                            |> Seq.toList
                        let l1' = l1 |> create
                        let l2' = l2 |> create
                        (l1' + l2') |> ValueRange.count = subtr.Length

                    Check.Quick checkSubtr

        [<TestFixture>]
        type ``There and back again`` () =
            let theraAndBackAgainProp n vs min incr max =
                
                let fromDto = Variable.Dto.fromDtoOpt
                let toDto   = Variable.Dto.toDto
                
                let toStr(n: BigRational) = n.ToString()

                if n |> System.String.IsNullOrWhiteSpace then true
                else
                    let dto = 
                        let dto = Variable.Dto.createNew "test"
                        let vals = vs |> Array.map toStr
                        let dto = dto |> Variable.Dto.setVals vals
                        let dto = dto |> Variable.Dto.setMin (min |> toStr)
                        let dto = dto |> Variable.Dto.setMax (max |> toStr)
                        dto
     
                    match dto |> Variable.Dto.fromDtoOpt with
                    | Some vr -> 
                        let dto'  = vr |> toDto |> fromDto |> Option.get |> toDto
                        let dto'' = dto' |> fromDto |> Option.get |> toDto
                        //printfn "new:\n%A\noriginal:\n%A" dto'' dto'
                        dto' = dto''
                    | None -> true
                
            [<Test>]
            member x.``Creating from dto has same result as creating from dto, back to dto and again from dto`` () =
                Check.Quick theraAndBackAgainProp 
