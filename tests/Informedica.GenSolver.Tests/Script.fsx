﻿// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#I "../../src/Informedica.GenSolver.Lib/"
#I "Scripts/"
#load "load-references.fsx"
#load "Variable.fs"

// Define your library scripting code here
open Informedica.GenSolver.Lib

open Swensen.Unquote
open FsCheck
open NUnit.Framework


module Testing =

    module Variable =

        module Value =

            open Variable.Value
            open Variable.ValueSet

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


let runTests () =

    let test = new Testing.Variable.Value.``Given a zero or negative number``()
    test.``With 0 an exception is raised``()
    test.``With -1N an exception is raised``()
    
    let test = new Testing.Variable.Value.``Given a non zero positive value``()
    test.``A value can be created``()

    let test = new Testing.Variable.Value.``The create function``()
    test.``Will not create negative values``()

    let test = new Testing.Variable.Value.``Given an infix operand``()
    test.``The operand gives the same result as applied to BigRationals``()

    let test = new Testing.Variable.Values.``Given list = empty incr = None min = None max = None``()
    test.``Counting values returns one``()
    test.``Creating values returns range All``()

    let test = new Testing.Variable.Values.``Given list with one value incr = None min = None max = None``()
    test.``Counting values returns one``()
    test.``Creating values returns one value``()
    