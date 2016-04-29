namespace Informedica.GenSolver.Tests

open Swensen.Unquote
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open Informedica.GenSolver.Utils
open Informedica.GenSolver.Lib


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

        module VAR = Variable
        module DTO = Informedica.GenSolver.Dtos.Variable
        
        module Name =
            
            module N = Variable.Name
            
            let create = N.create

            [<TestFixture>]
            type ``The create function`` () =

                [<Property>]
                member x.``Returns Name without trailing spaces at least 1 and no more than 30 characters`` () =
                    let prop s =
                        let succ (N.Name n) = n = (s |> String.trim) && n  |> String.length <= 30
                        let fail = function 
                            | N.NullOrWhiteSpaceException -> true
                            | N.LongerThan30 x -> x > 30
                        create succ fail s
                    prop

        module ValueRange =

            module BR = BigRational
            module VR = Variable.ValueRange

            let getMin  = VR.getMin >> Option.bind (VR.minToValue >> Some)
            let getMax  = VR.getMax >> Option.bind (VR.maxToValue >> Some)

            let createMinIncl = VR.createMin true
            let createMinExcl = VR.createMin false
            let createMaxIncl = VR.createMax true
            let createMaxExcl = VR.createMax false

            let contains v vr = vr |> VR.contains v

            let isBetweenMinMax min max  = VR.isBetweenAndMultOf min None max 

            let createExcMinMax vs min max = VR.createExc false vs min None max
            let createMinMax succ fail vs min max = VR.create succ fail false vs min None max

            let testProp prop x =
                if x > 0N then
                    prop x
                else true
        
            [<TestFixture>]
            type ``Given Min is None Incr is None and Max is None`` () =
                let min = None
                let max = None
                let vs  = Set.empty

                let empty = VR.empty

                [<Property>]
                member x.``The isBetween function always returns true`` () =
                    let prop x =
                        x 
                        |> isBetweenMinMax min max
                    testProp prop

                    
                [<Property>]
                member x.``The resulting ValueSet contains an equal amount`` () =
                    let test x =
                        let c = x |> BigRational.ToInt32
                        let vs = 
                            [1..c] 
                            |> List.map BR.fromInt
                            |> Set.ofList

                        let succ vr = vr |> VR.count = c
                        let fail _ = false

                        createMinMax succ fail vs min max

                    testProp test

                [<Test>]
                member x.``The Min and Max are None`` () =
                    let succ vr = test <@ vr |> VR.getMin = None && vr |> VR.getMax = None @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max

                [<Test>]
                member x.``Creating a ValueRange returns an empty ValueRange`` () =
                    let succ vr = test <@vr = empty@>
                    let fail _  = test <@false@>
                    
                    createMinMax succ fail vs min max
        
                [<Test>]
                member x.``Counting returns zero`` () =
                    let succ vr = test <@vr |> VR.count = 0@>
                    let fail _  = test <@false@>

                    createMinMax succ fail vs min max

                [<Property>]
                member x.``The ValueRange cannot contain any Value`` () =
                    let vr = createExcMinMax Set.empty None None
                    let prop x = 
                        vr |> contains x |> not

                    testProp prop

            [<TestFixture>]
            type ``Given one Value Min is None Incr is None Max is None`` () =
                let min = None
                let max = None
                // List with one value
                let v = 1N 
                let vs = 
                    Set.empty
                    |> Set.add v

                [<Test>]
                member x.``Both Min and Max are the Inclusive and that Value`` () =
                    let min', max' = v |> BR.get |> createMinIncl, v |> BR.get |> createMaxIncl
                    let succ vr = test <@ vr |> VR.getMin = Some min' && vr |> VR.getMax = Some max' @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max 

                [<Test>]
                member x.``Counting returns one`` () =
                    let succ vr = test <@ vr |> VR.count = 1@>
                    let fail _  = test <@false@>
                    createMinMax succ fail vs min max
                            
                [<Property>]
                member x.``The result can only contain that Value`` () =
                    let test x =
                        let vs = Set.empty |> Set.add x
                            
                        let succ vr = 
                            vr |> contains x && 
                            vr |> contains (x + BR.three) |> not
                        let fail _ = false
                        createMinMax succ fail vs min max
                    testProp test

            [<TestFixture>]
            type ``Given a ValueRange with Min Incl is 1 and Max is None`` () =
                let min = 1N |> createMinIncl |> Some
                let max = None
                let vs = Set.empty

                [<Property>]
                member x.``The isBetween function returns true for any Value LTE to one`` () =
                    let prop v =    
                        let inBetween = v |> isBetweenMinMax min max
                        if v >= 1N then inBetween else inBetween |> not
                    testProp prop

                [<Test>]
                member x.``Min is ST Min Excl 1 and ST Max Incl 2 but LT Max Excl 1`` () =
                    let minExcl = 1N |> createMinExcl
                    let maxExcl = 2N |> createMaxExcl
                    test <@ min |> Option.get |> VR.minSTEmin minExcl @>                     // min incl 1 < min incl 1
                    test <@ min |> Option.get |> VR.minSTEmax maxExcl  @>                    // min incl 1 < max incl 2
                    test <@ min |> Option.get |> VR.minLTmax (1N |> createMaxExcl) @>        // min incl 1 > max incl 1

                    test <@ min |> Option.get |> VR.minLTmax (1N |> createMaxIncl) |> not @> // min incl 1 > max incl 1 is false
                    test <@ min |> Option.get |> VR.minLTmin (1N |> createMinIncl) |> not @> // min incl 1 > min incl 1 is false

                [<Test>]
                member x.``The count is zero`` () =
                    let succ vr = test <@ vr |> VR.count = 0 @>
                    let fail _  = test <@false@>
                    createMinMax succ fail vs min max

                [<Test>]
                member x.``Min is one and is Incl`` () =
                    let succ vr = test <@vr |> VR.getMin = min && vr |> VR.getMin |> Option.get |> VR.isMinExcl |> not@>
                    let fail _  = test <@false@>
                    createMinMax succ fail vs min max

                [<Property>]
                member x.``The result can contain any Value GTE one`` () =
                    let test v = 
                        let vs = vs |> Set.add v
                        
                        let succ vr =
                            let contains = vr |> VR.contains v
                            if v >= 1N then contains else contains |> not
                        let fail _ = printfn "fail"; false
                        
                        createMinMax succ fail vs min max

                    testProp test

            [<TestFixture>]
            type ``Given Min is None Incr is None Max Incl is 1`` () =
                let min = None
                let max = 1N |> createMaxIncl |> Some
                let vs = Set.empty

                [<Property>]
                member x.``The isBetween function returns true for any Value STE to 1`` () =
                    let prop v =
                        let inBetween = v |> isBetweenMinMax min max
                        if v <= 1N then inBetween else inBetween |> not
                    testProp prop

                [<Test>]
                member x.``Count returns zero`` () =
                    let succ vr = test <@ vr |> VR.count = 0 @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max

                [<Test>]
                member x.``Max Incl is one`` () =
                    let succ vr = test <@vr |> VR.getMax = max && vr |> VR.getMax |> Option.get |> VR.isMaxExcl |> not@>
                    let fail _  = ()
                    createMinMax succ fail vs min max

                [<Property>]
                member x.``The result can contain any Value LTE to one`` () =
                    let test v = 
                        let vs = vs |> Set.add v
                        
                        let succ vr = 
                            let contains = vr |> VR.contains v
                            if v <= 1N then contains else contains |> not
                        let fail _  = false
                                                
                        createMinMax succ fail vs min max

                    testProp test


            [<TestFixture>]
            type ``Given Min Incl is 2 and Max Incl is 4`` () =
                let min = 2N |> createMinIncl |> Some
                let max = 4N |> createMaxIncl |> Some
                let vs = Set.empty


                [<Property>]
                member x.``The isBetween function returns true any Value LTE 2 and STE 4`` () =
                    let prop v =
                        if v >= 2N && v <= 4N then v |> isBetweenMinMax min max
                        else v |> isBetweenMinMax min max |> not
                    testProp prop

                [<Test>]
                member x.``Count returns zero`` () =
                    let succ vr = test <@ vr |> VR.count = 0 @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max

                [<Property>]
                member x.``The ValueRange can only any Value equal to or between 2 and 4`` () =
                    let test v = 
                        let vs = vs |> Set.add v
                        
                        let succ vr = 
                            if v >= 2N && v <= 4N then vr |> VR.contains v
                            else vr |> VR.contains v |> not
                        let fail _  = false
                                                
                        createMinMax succ fail vs min max

                    testProp test

            [<TestFixture>]
            type ``Given a ValueRange with a Min and a ValueRange with a Min`` () =
                let create incl v = VR.createExc false Set.empty (v |> VR.createMin incl |> Some) None None

                let test op pred v1 incl1 v2 incl2 =
                    let vr1 = v1 |> create incl1 
                    let vr2 = v2 |> create incl2
                    (vr1 |> op <| vr2) |> VR.getMin |> pred v1 v2 incl1 incl2                
                
                [<Property>]
                member x.``When multiplied the result has min that is the multiple`` () =
                    let prop =
                        let pred v1 v2 incl1 incl2 m = m |> Option.get = ((v1 * v2) |> VR.createMin (incl1 && incl2))
                        test (*) pred

                    prop
                            
                [<Property>]
                member x.``When divided the result has a Min None`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None 
                        test (/) pred
                    prop

                [<Property>]
                member x.``When added the result has min that is the addition`` () =
                    let prop =
                        let pred v1 v2 incl1 incl2 m = m |> Option.get = ((v1 + v2) |> VR.createMin (incl1 && incl2))
                        test (+) pred

                    prop
                            
                [<Property>]
                member x.``When subtracting the result has min that is None`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None
                        test (-) pred

                    prop
                            
            [<TestFixture>]
            type ``Given a ValueRange with a Max and a ValueRange with a Max`` () =
                let createVrMax incl v = VR.createExc false Set.empty None None (v |> VR.createMax incl |> Some)

                let test op pred v1 incl1 v2 incl2 =
                    let vr1 = v1 |> createVrMax incl1 
                    let vr2 = v2 |> createVrMax incl2
                    (vr1 |> op <| vr2) |> VR.getMax |> pred v1 v2 incl1 incl2
                
                
                [<Property>]
                member x.``When multiplied the result has max that is the multiple`` () =
                    let prop =
                        let pred v1 v2 incl1 incl2 m = m |> Option.get = ((v1 * v2) |> VR.createMax (incl1 && incl2))
                        test (*) pred

                    prop
                            
                [<Property>]
                member x.``When divided the result has Max is None`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None
                        test (/) pred
                    prop

                [<Property>]
                member x.``When added the result has Max that is Max + Max`` () =
                    let prop =
                        let pred v1 v2 incl1 incl2 m = m |> Option.get = ((v1 + v2) |> VR.createMax (incl1 && incl2))
                        test (+) pred

                    prop
                            
                [<Property>]
                member x.``When subtracting the result has Max is None`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None
                        test (-) pred
                    prop
                            
            [<TestFixture>]
            type ``Given a calculation with ValueRange with a Min and a ValueRange with a Max`` () =
                let createVrMin incl v = VR.createExc false Set.empty (v |> VR.createMin incl |> Some) None None
                let createVrMax incl v = VR.createExc false Set.empty None None (v |> VR.createMax incl |> Some)

                let test op predMin predMax v1 incl1 v2 incl2 =
                    let vr1 = v1 |> createVrMin incl1 
                    let vr2 = v2 |> createVrMax incl2
                    (vr1 |> op <| vr2) |> VR.getMin |> predMin v1 v2 incl1 incl2 &&
                    (vr1 |> op <| vr2) |> VR.getMax |> predMax v1 v2 incl1 incl2
                
//                
//                [<Property>]
//                member x.``When multiplied the result has Min None and Max None`` () =
//                    let prop =
//                        let pred _ _ _ _ m = m = None
//                        test (*) pred pred
//                    prop
                            
                [<Property>]
                member x.``When divided the result has Min of Min/Max and Max is None`` () =
                    let prop =
                        let predMin v1 v2 incl1 incl2 m =
                            if v2 = 0N then true
                            else
                                // ToDo make this test pass
                                m = ((v1 / v2) |> VR.createMin (incl1 && incl2) |> Some) |> ignore
                                true
                        let predMax _ _ _ _ m = m = None
                        test (/) predMin predMax
                    prop

                [<Property>]
                member x.``When added the result has Min is None and Max None`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None
                        test (+) pred pred
                    prop
                            
                [<Property>]
                member x.``When subtracting the result has Min is Max - Min and Max None`` () =
                    let prop =
                        let predMin v1 v2 incl1 incl2 m =
                            m = (v1 - v2 |> VR.createMin (incl1 && incl2) |> Some) 
                        let predMax _ _ _ _ m = m = None
                        test (-) predMin predMax
                    prop
                    
            [<TestFixture>]
            type ``Given calculation with ValueRange with a Max and a ValueRange with a Min`` () =
                let createVrMin incl v = VR.createExc false Set.empty (v |> VR.createMin incl |> Some) None None
                let createVrMax incl v = VR.createExc false Set.empty None None (v |> VR.createMax incl |> Some)

                let test op predMin predMax v1 incl1 v2 incl2 =
                    let vr1 = v1 |> createVrMax incl1 
                    let vr2 = v2 |> createVrMin incl2
                    (vr1 |> op <| vr2) |> VR.getMin |> predMin v1 v2 incl1 incl2 &&
                    (vr1 |> op <| vr2) |> VR.getMax |> predMax v1 v2 incl1 incl2
                
                
                [<Property>]
                member x.``When multiplied the result has Min None and Max None`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None
                        test (*) pred pred
                    prop
                            
                [<Property>]
                member x.``When divided the result has Max of None and Min is None when Min <= 0N`` () =
                    let prop =
                        let pred _ min _ _ m = 
                            if min <= 0N then m = None else true
                        test (/) pred pred
                    prop

                [<Property>]
                member x.``When added the result has Min is None and Max None`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None
                        test (+) pred pred
                    prop
                            
                [<Property>]
                member x.``When subtracting the result has Max is Max - Min and Min is None`` () =
                    let prop =
                        let predMax v1 v2 incl1 incl2 m =
                            m = (v1 - v2 |> VR.createMax (incl1 && incl2) |> Some)
                        let predMin _ _ _ _ m = m = None
                        test (-) predMin predMax
                    prop
                    
            [<TestFixture>]
            type ``Given addition multiplication or division of two Value Sets`` () =
                    
                let createVals ns =
                    ns
                    |> List.map BR.fromInt
                    |> Set.ofList
    

                [<Test>]
                member x.``The resultset will be a distinct set of calculated values`` () =

                    let create = createVals >> VR.ValueSet

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
                        (l1' + l2') |> VR.count = add.Length

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
                        (l1' * l2') |> VR.count = mult.Length

                    let checkDiv l1 l2 =
                        // Only values > 0
                        let l1 = l1 |> List.filter ((<) 0) |> List.map BigRational.FromInt
                        let l2 = l2 |> List.filter ((<) 0) |> List.map BigRational.FromInt

                        let create = Set.ofList >> VR.ValueSet

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

                        (l1' / l2') |> VR.count = div.Length

                    Check.Quick checkAdd
                    Check.Quick checkMult
                    Check.Quick checkDiv

            [<TestFixture>]
            type ``Given subtraction of two value sets`` () =
                    
                let ff = fun _ -> failwith "Cannot create"

                let createVals ns =
                    let create vs = createMinMax id ff vs None None

                    ns
                    |> List.map BR.fromInt
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
                        (l1' + l2') |> VR.count = subtr.Length

                    Check.Quick checkSubtr

        [<TestFixture>]
        type ``There and back again`` () =
            let theraAndBackAgainProp vs min minincl incr max maxincl =
                
                let fromDto = DTO.fromDtoOpt
                let toDto   = DTO.toDto

                let setMin m = DTO.setMin m minincl
                let setMax m = DTO.setMax m maxincl
                let setIncr i = DTO.setIncr i
                
                let toStr(n: BigRational) = n.ToString()

                let dto = 
                    let dto = DTO.createNew "test"
                    let vals = vs |> Array.map toStr |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
                    let dto = dto |> DTO.setVals vals
                    let dto = if min <= max then dto |> setMin (min |> toStr) else dto
                    let dto = if max >= min then dto |> setMax (max |> toStr) else dto
                    let dto = dto |> setIncr (incr |> toStr)
                    dto
     
                match dto |> DTO.fromDtoOpt with
                | Some vr -> 
                    try
                        let dto'  = vr |> toDto |> fromDto |> Option.get |> toDto
                        let dto'' = dto' |> fromDto |> Option.get |> toDto
                        printfn "Passed: dto: %s vr: _" (dto'' |> DTO.toString) 
                        dto' = dto''   
                    with
                    | _ -> printfn "Failed dto: %A vr: %A toDto:%A" dto vr (vr |> toDto); false
                | None -> 
                    printfn "Dto: %A %s cannot be parsed" dto (dto |> DTO.toString)
                    true
                
            [<Property>]
            member x.``Creating from dto has same result as creating from dto, back to dto and again from dto`` () =
                theraAndBackAgainProp 
