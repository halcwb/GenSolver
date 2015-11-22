namespace Informedica.GenSolver.Tests

open Informedica.GenSolver.Lib
open Swensen.Unquote
open NUnit.Framework

module Testing =

    module Variable =

        module Value =

            open Variable.Value
            open Variable.ValueSet
        
            [<TestFixture>]
            type ``Given a zero or negative number`` () =
                [<Test>]
                member x.``With 0 an exception is raised`` () =
                    raises<Variable.Value.NonZeroOrPositiveValueException> <@ 0N |> Variable.Value.create <> (0N |> Value.Value) @>
                [<Test>]
                member x.``With -1N an exception is raised`` () =
                    raises<Variable.Value.NonZeroOrPositiveValueException> <@ 0N |> Variable.Value.create <> (-1N |> Value.Value) @>
            [<TestFixture>]
            type ``Given a non zero positive value`` () =
                [<Test>]
                member x.``A value can be created`` () =
                    test<@ Variable.Value.create 1N = (1N |> Value.Value) @>
        
            [<TestFixture>]
            type ``Given an infix operand`` () =
            
                [<Test>]
                member x.``The operand gives the same result as applied to BigRationals`` () =
                    let v1 = 1N |> Variable.Value.create
                    let v2 = 1N |> Variable.Value.create
                    test <@ Variable.Value.calc (+) v1 v2 |> Variable.Value.getValue = 2N @>
                    test <@ (v1 + v2) |> Variable.Value.getValue = 2N @>


        module Values =

            open  Variable.ValueSet

            [<TestFixture>]
            type ``Given list = empty incr = None min = None max = None`` () =
                let incr = None
                let min = None
                let max = None

                let vals = Range.All |> ValueSet.Range
        
                [<Test>]
                member x.``Creating values returns range All`` () =
                    test <@ Variable.ValueSet.create incr min max [] = vals @>
        
                [<Test>]
                member x.``Counting values returns one`` () =
                    test <@ Variable.ValueSet.create incr min max [] |> Variable.ValueSet.count = 0 @>

            [<TestFixture>]
            type ``Given list with one value incr = None min = None max = None`` () =
                let incr = None
                let min = None
                let max = None

                let vals = [1N |> Variable.Value.create] 

                [<Test>]
                member x.``Counting values returns one`` () =
                    test <@ Variable.ValueSet.create incr min max vals |> Variable.ValueSet.count = 1 @>
        
                [<Test>]
                member x.``Creating values returns one value`` () =
                    test <@ Variable.ValueSet.create incr min max vals = (vals |> Variable.ValueSet.seqToValueSet) @>
