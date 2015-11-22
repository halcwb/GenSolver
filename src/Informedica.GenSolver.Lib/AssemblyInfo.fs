namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Informedica.GenSolver.Lib")>]
[<assembly: AssemblyProductAttribute("GenSolver")>]
[<assembly: AssemblyDescriptionAttribute("A solver that solves a set of product and sum equations in which variables are discrete sets of rational numbers that cannot be zero or negative")>]
[<assembly: AssemblyVersionAttribute("0.0.2")>]
[<assembly: AssemblyFileVersionAttribute("0.0.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.2"
