namespace Informedica.GenSolver.Tests

open Informedica.GenSolver.Lib
open Swensen.Unquote
open NUnit.Framework

module Testing =

    [<TestFixture>]
    type ``Given list = empty incr = None min = None max = None`` () =
        let incr = None
        let min = None
        let max = None

        let vals = range.All |> values.Range
        
        [<Test>]
        member x.``Creating values returns range All`` () =
            test <@ Variable.Values.create [] incr min max = vals @>
        
        [<Test>]
        member x.``Counting values returns one`` () =
            test <@ Variable.Values.create [] incr min max |> Variable.Values.count = 0 @>

    [<TestFixture>]
    type ``Given list with one value incr = None min = None max = None`` () =
        let incr = None
        let min = None
        let max = None

        let vals = [1N |> Variable.Value.create] 

        [<Test>]
        member x.``Counting values returns one`` () =
            test <@ Variable.Values.create vals incr min max |> Variable.Values.count = 1 @>
        
        [<Test>]
        member x.``Creating values returns one value`` () =
            test <@ Variable.Values.create vals incr min max = (vals |> values.Values) @>
