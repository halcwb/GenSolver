namespace Informedica.GenSolver.Tests

open Informedica.GenSolver.Lib
open Swensen.Unquote
open NUnit.Framework
open FsCheck

module Testing =

    // #region ---- QUICK CHECK GENERATORS ----
    let bigRGen (n, d) = 
        let n' = abs(n) |> BigRational.FromInt
        let d' = abs(d) |> BigRational.FromInt
        n'/d'

    let bigRGenerator =
        gen {
            let n, d = Random.m1, Random.m2
            return bigRGen(n, d)
        }

    type MyGenerators () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator }

    Arb.register<MyGenerators>() |> ignore

    // #endregion

    module Variable =

        module Value =

            open Variable

            let create = Value.create
            let calc = Value.calc

            [<TestFixture>]
            type ``The create function`` () =
                [<Test>]
                member x.``Will not create negative values`` () =
                    let nonZeroOrNegative x =
                        try
                            x 
                            |> BigRational.FromInt
                            |> Variable.Value.create > Variable.Value.zero 
                        with 
                        | Variable.Value.NonZeroOrPositiveValueException _ -> true

                    Check.Quick nonZeroOrNegative
                    
            [<TestFixture>]
            type ``Given a zero or negative number`` () =
                [<Test>]
                member x.``With 0 an exception is raised`` () =
                    raises<Variable.Value.NonZeroOrPositiveValueException> <@ 0N |> create <> (0N |> Value.Value) @>
                [<Test>]
                member x.``With -1N an exception is raised`` () =
                    raises<Variable.Value.NonZeroOrPositiveValueException> <@ 0N |> create <> (-1N |> Value.Value) @>

            [<TestFixture>]
            type ``Given a non zero positive value`` () =
                [<Test>]
                member x.``A value can be created`` () =
                    test<@ create 1N = (1N |> Value.Value) @>
        
            [<TestFixture>]
            type ``Given a get function`` () =
                [<Test>]
                member x.``The same value is returned as created`` () =
                    let canGetValue v =
                        if v > 0 then
                            v
                            |> BigRational.FromInt
                            |> create
                            |> Variable.Value.get
                            |> BigRational.ToInt32 = v
                        else true

                    Check.Quick canGetValue
            
            [<TestFixture>]
            type ``Given an infix operand`` () =
            
                [<Test>]
                member x.``The operand gives the same result as applied to BigRationals`` () =
                    let v1 = 1N |> create
                    let v2 = 1N |> create
                    test <@ calc (+) v1 v2 |> Variable.Value.get = 2N @>
                    test <@ (v1 + v2) |> Variable.Value.get = 2N @>

            [<TestFixture>]
            type ``Is overloaded with`` () =
                [<Test>]
                member x.``Basic arrhythmic functions`` () =
                    let checkMult x1 x2 =
                        let create = BigRational.FromInt >> create
                        let get = Variable.Value.get >> BigRational.ToInt32
                        if x1 > 0 && x2 > 0 then
                            let x1' = x1 |> create
                            let x2' = x2 |> create
                            (x1' + x2') |> get = (x1 + x2)
                        else true

                    let checkDiv x1 x2 =
                        let create = BigRational.FromInt >> create
                        let get = Variable.Value.get >> BigRational.ToInt32
                        if x1 > 0 && x2 > 0 then
                            let x1' = x1 |> create
                            let x2' = x2 |> create
                            (x1' + x2') |> get = (x1 + x2)
                        else true

                    let checkAdd x1 x2 =
                        let create = BigRational.FromInt >> create
                        let get = Variable.Value.get >> BigRational.ToInt32
                        if x1 > 0 && x2 > 0 then
                            let x1' = x1 |> create
                            let x2' = x2 |> create
                            (x1' + x2') |> get = (x1 + x2)
                        else true

                    let checkSubtr x1 x2 =
                        let create = BigRational.FromInt >> create
                        let get = Variable.Value.get >> BigRational.ToInt32
                        if x1 > 0 && x2 > 0 then
                            let x1' = x1 |> create
                            let x2' = x2 |> create
                            (x1' + x2') |> get = (x1 + x2)
                        else true

                    Check.Quick checkMult
                    Check.Quick checkDiv
                    Check.Quick checkAdd
                    Check.Quick checkSubtr
        
            [<TestFixture>]
            type ``Given a negative subtraction result`` () =
                
                [<Test>]
                member x.``A NonZeroOrPositive error is thrown`` () =
                    let v1 = create 1N
                    let v2 = create 2N
                    raises<Variable.Value.NonZeroOrPositiveValueException> <@ v1 - v2 @> 

        module Values =

            open  Variable

            let createSomeVal = Variable.Value.create >> Some
            let createVals = Variable.Values.create
            let createVal = Variable.Value.create

            let getIncr = Variable.Values.getIncr
            let getMin  = Variable.Values.getMin
            let getMax  = Variable.Values.getMax

            [<TestFixture>]
            type ``Given list = empty incr = None min = None max = None`` () =
                let incr = None
                let min = None
                let max = None

                let vals = Values.rangeAll 
        
                [<Test>]
                member x.``Creating values returns range All`` () =
                    test <@ createVals incr min max [] = vals @>
        
                [<Test>]
                member x.``Counting values returns zero`` () =
                    test <@ createVals incr min max [] |> Variable.Values.count = 0 @>

            [<TestFixture>]
            type ``Given list with one value incr = None min = None max = None`` () =
                let incr = None
                let min = None
                let max = None
                // List with one value
                let vals = [1N |> createVal] 

                [<Test>]
                member x.``Counting values returns one`` () =
                    test <@ createVals incr min max vals |> Variable.Values.count = 1 @>
        
                [<Test>]
                member x.``Creating values returns list with one value`` () =
                    test <@ createVals incr min max vals = (vals |> Variable.Values.seqToValueSet) @>

            [<TestFixture>]
            type ``Given empty list incr = Some 1N min = None max = None`` () =
                let incr = 1N |> createSomeVal
                let min = None
                let max = None
                // List with one value
                let vals = [] 

                [<Test>]
                member x.``Values contain no values`` () =
                    test <@ createVals incr min max vals |> Variable.Values.count = 0 @>

                [<Test>]
                member x.``Increment is one`` () =
                    test <@ createVals incr min max vals |> getIncr = incr @>

            [<TestFixture>]
            type ``Given empty list incr = None min = Some 1N max = None`` () =
                let incr = None
                let min = 1N |> createSomeVal
                let max = None
                // List with one value
                let vals = [] 

                [<Test>]
                member x.``Values contain no values`` () =
                    test <@ createVals incr min max vals |> Variable.Values.count = 0 @>

                [<Test>]
                member x.``Minimum is one`` () =
                    test <@ createVals incr min max vals |> getMin = min @>

            [<TestFixture>]
            type ``Given empty list incr = None min = None max = Some 1`` () =
                let incr = None
                let min = None
                let max = 1N |> createSomeVal
                // List with one value
                let vals = [] 

                [<Test>]
                member x.``Values contain no values`` () =
                    test <@ createVals incr min max vals |> Variable.Values.count = 0 @>

                [<Test>]
                member x.``Maximum is one`` () =
                    test <@ createVals incr min max vals |> getMax = max @>

            [<TestFixture>]
            type ``Given empty list incr = Some 1N min = Some 2N max = None`` () =
                let incr = 1N |> createSomeVal
                let min = 2N |> createSomeVal
                let max = None
                // List with one value
                let vals = [] 

                [<Test>]
                member x.``Values contain no values`` () =
                    test <@ createVals incr min max vals |> Variable.Values.count = 0 @>

                [<Test>]
                member x.``Increment is one and minimum is two`` () =
                    test <@ createVals incr min max vals |> getIncr = incr @>
                    test <@ createVals incr min max vals |> getMin = min @>

            [<TestFixture>]
            type ``Given empty list incr = None min = Some 2N max = Some 4N`` () =
                let incr = None
                let min = 2N |> createSomeVal
                let max = 4N |> createSomeVal
                // List with one value
                let vals = [] 

                [<Test>]
                member x.``Values contain no values`` () =
                    test <@ createVals incr min max vals |> Variable.Values.count = 0 @>

                [<Test>]
                member x.``Increment is one and minimum is two`` () =
                    test <@ createVals incr min max vals |> getMin = min @>
                    test <@ createVals incr min max vals |> getMax = max @>

            [<TestFixture>]
            type ``Given empty list incr = 2N min = None max = Some 4N`` () =
                let incr = 2N |> createSomeVal
                let min = None
                let max = 4N |> createSomeVal
                // List with one value
                let vals = [] 

                [<Test>]
                member x.``Values now contains two values`` () =
                    test <@ createVals incr min max vals |> Variable.Values.count = 2 @>

                [<Test>]
                member x.``Increment is none minimum is 2 and maximum is 4`` () =
                    test <@ createVals incr min max vals |> getIncr = None @>
                    test <@ createVals incr min max vals |> getMin = incr @>
                    test <@ createVals incr min max vals |> getMax = max @>

            [<TestFixture>]
            type ``Given a list of Value`` () =
    
                [<Test>]
                member x.``The resulting ValueSet contains an equal amount`` () =
                    let equalCount c =
                        if c >= 1 then
                            let vals = 
                                [1..c] 
                                |> List.map BigRational.FromInt
                                |> Variable.Values.createValues
                            vals |> Variable.Values.valueSetToList |> List.length = c
                        else true

                    Check.Quick equalCount

                [<Test>]
                member x.``Can filter by incr, min and max`` () =
                    // Check values filter
                    let create = Variable.Values.createValues
                    let vsincr = [1N..1N..10N] |> create
                    let incr = createVal 2N |> Some
                    let min = createVal 4N |> Some 
                    let max = createVal 8N |> Some
                    test <@ Variable.Values.filter None None None vsincr = vsincr @>
                    test <@ Variable.Values.filter incr None None vsincr = ([2N..2N..10N] |> create) @>
                    test <@ Variable.Values.filter incr min None vsincr = ([4N..2N..10N] |> create) @>
                    test <@ Variable.Values.filter incr min max vsincr  = ([4N..2N..8N] |> create) @>
                    
            [<TestFixture>]
            type ``Given addition multiplication or division of two value sets`` () =
                    
                [<Test>]
                member x.``The resultset will be a distinct set of added values`` () =
                    let checkAdd l1 l2 =
                        // Only values > 0
                        let l1 = l1 |> List.filter ((<) 0)
                        let l2 = l2 |> List.filter ((<) 0)

                        let create = (List.map BigRational.FromInt) >> Variable.Values.createValues
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
                        (l1' + l2') |> Variable.Values.valueSetToList |> List.length = add.Length

                    let checkMult l1 l2 =
                        // Only values > 0
                        let l1 = l1 |> List.filter ((<) 0)
                        let l2 = l2 |> List.filter ((<) 0)

                        let create = (List.map BigRational.FromInt) >> Variable.Values.createValues
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
                        (l1' * l2') |> Variable.Values.valueSetToList |> List.length = mult.Length

                    let checkDiv l1 l2 =
                        // Only values > 0
                        let l1 = l1 |> List.filter ((<) 0) |> List.map BigRational.FromInt
                        let l2 = l2 |> List.filter ((<) 0) |> List.map BigRational.FromInt

                        let create = Variable.Values.createValues
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
                        (l1' / l2') |> Variable.Values.valueSetToList |> List.length = div.Length

                    Check.Quick checkAdd
                    Check.Quick checkMult
                    Check.Quick checkDiv

            [<TestFixture>]
            type ``Given subtraction of two value sets`` () =
                    
                [<Test>]
                member x.``The resultset will be a distinct set of positive values`` () =
                    let checkSubtr l1 l2 =
                        // Only values > 0
                        let l1 = l1 |> List.filter ((<) 0)
                        let l2 = l2 |> List.filter ((<) 0)

                        let create = (List.map BigRational.FromInt) >> Variable.Values.createValues
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
                        (l1' + l2') |> Variable.Values.valueSetToList |> List.length = subtr.Length

                    Check.Quick checkSubtr


        [<TestFixture>]
        type ``There and back again`` () =
    

            let theraAndBackAgainProp n incr min max vs =
    
                if n |> System.String.IsNullOrWhiteSpace then true
                else

                    let dto = Variable.Dto.create n
                    dto.Increment <- incr
                    dto.Minimum   <- min
                    dto.Maximum   <- max
                    dto.Values    <- vs
     
                    (dto |> Variable.fromDto |> Variable.toDto |> Variable.fromDto) = (dto |> Variable.fromDto)
                
            [<Test>]
            member x.``Creating from dto has same result as creating from dto, back to dto and again from dto`` () =

                Check.Quick theraAndBackAgainProp 
