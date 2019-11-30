namespace Informedica.GenSolver.Tests

module Main =
    open Expecto

    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssembly defaultConfig argv
