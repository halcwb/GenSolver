// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#I "Scripts/"
#load "load-references.fsx"
#load "Variable.fs"

// Define your library scripting code here
open Informedica.GenSolver.Lib
open Swensen.Unquote

module Testing =

    type ``Given list = empty incr = None min = None max = None`` () =
        let incr = None
        let min = None
        let max = None

        let vals = range.All |> values.Range
        
        member x.``Creating values returns range All`` () =
            test <@ Variable.Values.create [] incr min max = vals @>
        
        member x.``Counting values returns one`` () =
            test <@ Variable.Values.create [] incr min max |> Variable.Values.count = 0 @>

    type ``Given list with one value incr = None min = None max = None`` () =
        let incr = None
        let min = None
        let max = None

        let vals = [1N |> Variable.Value.create] 

        
        member x.``Counting values returns one`` () =
            test <@ Variable.Values.create vals incr min max |> Variable.Values.count = 0 @>
        
        member x.``Creating values returns one value`` () =
            test <@ Variable.Values.create vals incr min max = (vals |> values.Values) @>


let runTests () =
    let test1 = new Testing.``Given list = empty incr = None min = None max = None``()
    test1.``Counting values returns one``()
    test1.``Creating values returns range All``()

    let test2 = new Testing.``Given list with one value incr = None min = None max = None``()
    test2.``Counting values returns one``()
    test2.``Creating values returns one value``()
    