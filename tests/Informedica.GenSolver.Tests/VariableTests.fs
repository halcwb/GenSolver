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

        module VAR = Variable
        module DTO = VAR.Dto
        
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

        module Value =

            module V = Variable.ValueRange.Value

            type valueExc = V.ValueException

            let raiseValueExc v = v |> V.ValueException |>  raise

            let zero = 0N

            let printCalc op x1 x2 = printfn "Testing %A, %A = %A" x1 x2 (x1 |> op <| x2)

            [<TestFixture>]
            type ``The create function`` () =

                [<Property>]
                member x.``Will not create values equal or smaller than zero`` () =
                    let nonZeroOrNegative x =
                        let succ (V.Value v) = v > zero && v = x
                        let fail = function 
                            | V.ZeroOrNegativeValue v -> v <= zero 
                        
                        V.create succ fail x
                    
                    nonZeroOrNegative
                    
        
            [<TestFixture>]
            type ``Given an infix operand`` () =
                        
                [<Property>]
                member x.``The operand gives the same result as applied to BigRationals`` () =
                    let prop op x1 x2 =
                        if x1 > 0N && x2 > 0N then
                            printCalc op x1 x2
                            let v1 = x1 |> V.createExc
                            let v2 = x2 |> V.createExc

                            let succ (V.Value v) = v = (x1 |> op <| x2)
                            let fail _ = true
                            V.calc succ fail op v1 v2
                        else true

                    prop

        
            [<TestFixture>]
            type ``Given a negative subtraction result`` () =
                
                [<Property>]
                member x.``The result is going to the failure function`` () =
                    let prop x1 x2 =
                        if x1 > 0N && x2 > 0N then
                            let v1 = V.createExc x1
                            let v2 = V.createExc x2
                            
                            printCalc (-) x1 x2

                            if x1 <= x2 then
                                let succ _ = false
                                let fail = function
                                    | V.ZeroOrNegativeValue v -> v <= 0N
                            
                                V.calc succ fail (-) v1 v2
                            else 
                                let succ _ = true
                                let fail _ = false

                                V.calc succ fail (-) v1 v2
                        else true    

                    prop

        module ValueRange =

            module V = Variable.ValueRange.Value
            module VR = Variable.ValueRange

            let getMin  = VR.getMin >> Option.bind (VR.minToValue >> Some)
            let getMax  = VR.getMax >> Option.bind (VR.maxToValue >> Some)

            let createMinIncl = V.createExc >> (VR.createMin false)
            let createMinExcl = V.createExc >> (VR.createMin true)
            let createMaxIncl = V.createExc >> (VR.createMax false)
            let createMaxExcl = V.createExc >> (VR.createMax true)

            let contains v vr = vr |> VR.contains v

            let isBetweenMinMax min max  = VR.isBetween min None max 

            let createExcMinMax vs min max = VR.createExc vs min None max
            let createMinMax succ fail vs min max = VR.create succ fail vs min None max

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
                        |> V.createExc
                        |> isBetweenMinMax min max
                    testProp prop

                    
                [<Property>]
                member x.``The resulting ValueSet contains an equal amount`` () =
                    let test x =
                        let c = x |> BigRational.ToInt32
                        let vs = 
                            [1..c] 
                            |> List.map (BigRational.FromInt >> V.createExc)
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
                        let v = V.createExc x
                        vr |> contains v |> not

                    testProp prop

            [<TestFixture>]
            type ``Given one Value Min is None Incr is None Max is None`` () =
                let min = None
                let max = None
                // List with one value
                let v = 1N |> V.createExc
                let vs = 
                    Set.empty
                    |> Set.add v

                [<Test>]
                member x.``Both Min and Max are the Inclusive and that Value`` () =
                    let min', max' = v |> VR.createMin false, v |> VR.createMax false
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
                        let v = V.createExc x
                        let vs = Set.empty |> Set.add v
                            
                        let succ vr = 
                            vr |> contains v && 
                            vr |> contains (v + V.three) |> not
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
                    let prop x =    
                        let v = x |> V.createExc
                        if x >= 1N then v |> isBetweenMinMax min max
                        else v |> isBetweenMinMax min max |> not
                    testProp prop

                [<Test>]
                member x.``Min is ST Min Excl 1 and ST Max Incl 2 but LT Max Excl 1`` () =
                    let minExcl = 1N |> createMinExcl
                    let maxExcl = 2N |> createMaxExcl
                    test <@ min |> Option.get |> VR.minSTEmin minExcl @>                     // min incl 1 < min excl 1
                    test <@ min |> Option.get |> VR.minSTEmax maxExcl  @>                    // min incl 1 < max excl 2
                    test <@ min |> Option.get |> VR.minLTmax (1N |> createMaxExcl) @>        // min incl 1 > max excl 1

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
                    let test x = 
                        let v = V.createExc x
                        let vs = vs |> Set.add v
                        
                        let succ vr = 
                            if x >= 1N then 
                                printfn "%s contains %s" (vr |> VR.toString) (v |> V.toString)
                                vr |> VR.contains v
                            else 
                                printfn "%s does not contain %s" (vr |> VR.toString) (v |> V.toString)
                                vr |> VR.contains v |> not
                        let fail _ = printfn "fail"; false
                        
                        createMinMax succ fail vs min max

                    testProp test

            [<TestFixture>]
            type ``Given Min is None Incr is None Max Incl is 1`` () =
                let min = None
                let max = 1N |> V.createExc |> VR.createMax false |> Some
                let vs = Set.empty

                [<Property>]
                member x.``The isBetween function returns true for any Value LTE to 1`` () =
                    let prop x =
                        let v = x |> V.createExc
                        if x <= 1N then v |> isBetweenMinMax min max
                        else v |> isBetweenMinMax min max |> not
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
                    let test x = 
                        let v = V.createExc x
                        let vs = vs |> Set.add v
                        
                        let succ vr = 
                            if x <= 1N then vr |> VR.contains v
                            else vr |> VR.contains v |> not
                        let fail _  = false
                                                
                        createMinMax succ fail vs min max

                    testProp test


            [<TestFixture>]
            type ``Given Min Incl is 2 and Max Incl is 4`` () =
                let min = 2N |> V.createExc |> VR.createMin false |> Some
                let max = 4N |> V.createExc |> VR.createMax false |> Some
                let vs = Set.empty


                [<Property>]
                member x.``The isBetween function returns true any Value LTE 2 and STE 4`` () =
                    let prop x =
                        let v = x |> V.createExc
                        if x >= 2N && x <= 4N then v |> isBetweenMinMax min max
                        else v |> isBetweenMinMax min max |> not
                    testProp prop

                [<Test>]
                member x.``Count returns zero`` () =
                    let succ vr = test <@ vr |> VR.count = 0 @>
                    let fail _  = test <@ false @>
                    createMinMax succ fail vs min max

                [<Property>]
                member x.``The ValueRange can only any Value equal to or between 2 and 4`` () =
                    let test x = 
                        let v = V.createExc x
                        let vs = vs |> Set.add v
                        
                        let succ vr = 
                            if x >= 2N && x <= 4N then vr |> VR.contains v
                            else vr |> VR.contains v |> not
                        let fail _  = false
                                                
                        createMinMax succ fail vs min max

                    testProp test

            [<TestFixture>]
            type ``Given a ValueRange with a Min and a ValueRange with a Min`` () =
                let createVrMin excl v = VR.createExc Set.empty (v |> VR.createMin excl |> Some) None None

                let test op pred x1 excl1 x2 excl2 =
                    match x1 |> V.createOption, x2 |> V.createOption with
                    | Some v1, Some v2 -> 
                        let vr1 = v1 |> createVrMin excl1 
                        let vr2 = v2 |> createVrMin excl2
                        (vr1 |> op <| vr2) |> VR.getMin |> pred v1 v2 excl1 excl2
                    | _ -> true
                
                
                [<Property>]
                member x.``When multiplied the result has min that is the multiple`` () =
                    let prop =
                        let pred v1 v2 excl1 excl2 m = m |> Option.get = ((v1 * v2) |> VR.createMin (excl1 || excl2))
                        test (*) pred

                    prop
                            
                [<Property>]
                member x.``When divided the result has min none`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None
                        test (/) pred
                    prop

                [<Property>]
                member x.``When added the result has min that is the addition`` () =
                    let prop =
                        let pred v1 v2 excl1 excl2 m = m |> Option.get = ((v1 + v2) |> VR.createMin (excl1 || excl2))
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
                let createVrMax excl v = VR.createExc Set.empty None None (v |> VR.createMax excl |> Some)

                let test op pred x1 excl1 x2 excl2 =
                    match x1 |> V.createOption, x2 |> V.createOption with
                    | Some v1, Some v2 -> 
                        let vr1 = v1 |> createVrMax excl1 
                        let vr2 = v2 |> createVrMax excl2
                        (vr1 |> op <| vr2) |> VR.getMax |> pred v1 v2 excl1 excl2
                    | _ -> true
                
                
                [<Property>]
                member x.``When multiplied the result has max that is the multiple`` () =
                    let prop =
                        let pred v1 v2 excl1 excl2 m = m |> Option.get = ((v1 * v2) |> VR.createMax (excl1 || excl2))
                        test (*) pred

                    prop
                            
                [<Property>]
                member x.``When divided the result has max none`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None
                        test (/) pred
                    prop

                [<Property>]
                member x.``When added the result has max that is the addition`` () =
                    let prop =
                        let pred v1 v2 excl1 excl2 m = m |> Option.get = ((v1 + v2) |> VR.createMax (excl1 || excl2))
                        test (+) pred

                    prop
                            
                [<Property>]
                member x.``When subtracting the result has max is the first max`` () =
                    let prop =
                        let pred v1 _ _ _ m = m = (v1 |> VR.createMax true |> Some)
                        test (-) pred

                    prop
                            
            [<TestFixture>]
            type ``Given a ValueRange with a Min and a ValueRange with a Max`` () =
                let createVrMin excl v = VR.createExc Set.empty (v |> VR.createMin excl |> Some) None None
                let createVrMax excl v = VR.createExc Set.empty None None (v |> VR.createMax excl |> Some)

                let test op predMin predMax x1 excl1 x2 excl2 =
                    match x1 |> V.createOption, x2 |> V.createOption with
                    | Some v1, Some v2 -> 
                        let vr1 = v1 |> createVrMin excl1 
                        let vr2 = v2 |> createVrMax excl2
                        (vr1 |> op <| vr2) |> VR.getMin |> predMin v1 v2 excl1 excl2 &&
                        (vr1 |> op <| vr2) |> VR.getMax |> predMax v1 v2 excl1 excl2
                    | _ -> true
                
                
                [<Property>]
                member x.``When multiplied the result has Min None and Max None`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None
                        test (*) pred pred
                    prop
                            
                [<Property>]
                member x.``When divided the result has Min division of Min/Max and Max None`` () =
                    let prop =
                        let predMin v1 v2 excl1 excl2 m =
                            m = ((v1 / v2) |> VR.createMin (excl1 || excl2) |> Some)
                        let predMax _ _ _ _ m = m = None
                        test (/) predMin predMax
                    prop

                [<Property>]
                member x.``When added the result has Min is Min Excl and Max None`` () =
                    let prop =
                        let predMin v1 _ _ _ m =
                            m = (v1 |> VR.createMin true |> Some)
                        let predMax _ _ _ _ m = m = None
                        test (+) predMin predMax
                    prop
                            
                [<Property>]
                member x.``When subtracting the result has Min Min - Max and Max None`` () =
                    let prop =
                        let predMin v1 v2 excl1 excl2 m =
                            if v1 > v2 then
                                m = (v1 - v2 |> VR.createMin (excl1 || excl2) |> Some) 
                            else m = None 
                        let predMax _ _ _ _ m = m = None
                        test (-) predMin predMax
                    prop
                    
            [<TestFixture>]
            type ``Given a ValueRange with a Max and a ValueRange with a Min`` () =
                let createVrMin excl v = VR.createExc Set.empty (v |> VR.createMin excl |> Some) None None
                let createVrMax excl v = VR.createExc Set.empty None None (v |> VR.createMax excl |> Some)

                let test op predMin predMax x1 excl1 x2 excl2 =
                    match x1 |> V.createOption, x2 |> V.createOption with
                    | Some v1, Some v2 -> 
                        let vr1 = v1 |> createVrMax excl1 
                        let vr2 = v2 |> createVrMin excl2
                        (vr1 |> op <| vr2) |> VR.getMin |> predMin v1 v2 excl1 excl2 &&
                        (vr1 |> op <| vr2) |> VR.getMax |> predMax v1 v2 excl1 excl2
                    | _ -> true
                
                
                [<Property>]
                member x.``When multiplied the result has Min None and Max None`` () =
                    let prop =
                        let pred _ _ _ _ m = m = None
                        test (*) pred pred
                    prop
                            
                [<Property>]
                member x.``When divided the result has Max division of Max/Min and Min None`` () =
                    let prop =
                        let predMax v1 v2 excl1 excl2 m =
                            m = ((v1 / v2) |> VR.createMax (excl1 || excl2) |> Some)
                        let predMin _ _ _ _ m = m = None
                        test (/) predMin predMax
                    prop

                [<Property>]
                member x.``When added the result has Min is Min Excl and Max None`` () =
                    let prop =
                        let predMin _ v2 _ _ m =
                            m = (v2 |> VR.createMin true |> Some)
                        let predMax _ _ _ _ m = m = None
                        test (+) predMin predMax
                    prop
                            
                [<Property>]
                member x.``When subtracting the result has Max Max - Min and Min None`` () =
                    let prop =
                        let predMax v1 v2 excl1 excl2 m =
                            if v1 > v2 then
                                m = (v1 - v2 |> VR.createMax (excl1 || excl2) |> Some) 
                            else m = None 
                        let predMin _ _ _ _ m = m = None
                        test (-) predMin predMax
                    prop
                    
            [<TestFixture>]
            type ``Given addition multiplication or division of two Value Sets`` () =
                    
                let createVals ns =
                    ns
                    |> List.map (BigRational.FromInt >> (VR.Value.createExc))
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

                        let create =
                            List.map (VR.Value.create id (fun _ -> failwith "Could not create"))
                            >> Set.ofList
                            >> VR.ValueSet

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
                    |> List.map BigRational.FromInt
                    |> List.map (VR.Value.create id ff)
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
                
                let toStr(n: BigRational) =
                    if n <= 0N then "" else n.ToString()

                let dto = 
                    let dto = DTO.createNew "test"
                    let vals = vs |> Array.map toStr |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
                    let dto = dto |> DTO.setVals vals
                    let dto = dto |> setMin (min |> toStr)
                    let dto = dto |> setMax (max |> toStr)
                    let dto = dto |> setIncr (incr |> toStr)
                    dto
     
                match dto |> Variable.Dto.fromDtoOpt with
                | Some vr -> 
                    try
                        let dto'  = vr |> toDto |> fromDto |> Option.get |> toDto
                        let dto'' = dto' |> fromDto |> Option.get |> toDto
                        dto' = dto''   
                    with
                    | _ -> printfn "dto: %A vr: %A toDto:%A" dto vr (vr |> toDto); false
                | None -> 
                    printfn "Dto: %A %s cannot be parsed" dto (dto |> DTO.toString)
                    true
                
            [<Property>]
            member x.``Creating from dto has same result as creating from dto, back to dto and again from dto`` () =
                theraAndBackAgainProp 
