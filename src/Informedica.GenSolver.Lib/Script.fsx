// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#I "Scripts/"
#load "load-references.fsx"
#load "Variable.fs"

// Define your library scripting code here
open Informedica.GenSolver.Lib
open Swensen.Unquote


let v1 = 1N |> Variable.Value.create
let v2 = 1N |> Variable.Value.create
v1 + v2

module Testing =

    open Variable.Value
    open Variable.Values

    type ``Given a zero or negative number`` () =

        member x.``With 0 an exception is raised`` () =
            raises<Variable.Value.NonZeroOrPositiveValueException> <@ 0N |> Variable.Value.create <> (0N |> value.Value) @>

        member x.``With -1N an exception is raised`` () =
            raises<Variable.Value.NonZeroOrPositiveValueException> <@ 0N |> Variable.Value.create <> (-1N |> value.Value) @>

    type ``Given a non zero positive value`` () =
        
        member x.``A value can be created`` () =
            test<@ Variable.Value.create 1N = (1N |> value.Value) @>

    type ``Given an infix operand`` () =
        
        member x.``The operand gives the same result as applied to BigRationals`` () =
            let v1 = 1N |> Variable.Value.create
            let v2 = 1N |> Variable.Value.create
            test <@ Variable.Value.calc (+) v1 v2 |> Variable.Value.getValue = 2N @>
            test <@ (v1 + v2) |> Variable.Value.getValue = 2N @>


    type ``Given list = empty incr = None min = None max = None`` () =
        let incr = None
        let min = None
        let max = None

        let vals = range.All |> values.Range
        
        member x.``Creating values returns range All`` () =
            test <@ Variable.Values.create incr min max [] = vals @>
        
        member x.``Counting values returns one`` () =
            test <@ Variable.Values.create incr min max [] |> Variable.Values.count = 0 @>

    type ``Given list with one value incr = None min = None max = None`` () =
        let incr = None
        let min = None
        let max = None

        let vals = [1N |> Variable.Value.create] 

        
        member x.``Counting values returns one`` () =
            test <@ Variable.Values.create incr min max vals |> Variable.Values.count = 1 @>
        
        member x.``Creating values returns one value`` () =
            test <@ Variable.Values.create incr min max vals = (vals |> values.Values) @>


let runTests () =

    let test = new Testing.``Given a zero or negative number``()
    test.``With 0 an exception is raised``()
    test.``With -1N an exception is raised``()
    
    let test = new Testing.``Given a non zero positive value``()
    test.``A value can be created``()

    let test = new Testing.``Given an infix operand``()
    test.``The operand gives the same result as applied to BigRationals``()

    let test = new Testing.``Given list = empty incr = None min = None max = None``()
    test.``Counting values returns one``()
    test.``Creating values returns range All``()

    let test = new Testing.``Given list with one value incr = None min = None max = None``()
    test.``Counting values returns one``()
    test.``Creating values returns one value``()
    