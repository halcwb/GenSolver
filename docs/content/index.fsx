(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../src/Informedica.GenSolver.Lib/Scripts"
#load "load-project-release.fsx"

#time

(**
GenSolver
======================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The GenSolver library can be <a href="https://nuget.org/packages/Informedica.GenSolver.Lib">installed from NuGet</a>:
      <pre>PM> Install-Package Informedica.GenSolver.Lib -Pre</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates using a function defined in this sample library.

*)

module UT  = Informedica.GenUtils.Lib.BCL.String
module Api = Informedica.GenSolver.Api

// Format print output to use in markdown
let solve = Api.solve (fun s -> "> " + s + "</br>" |> UT.replace "*" "\*" |> printfn "%s")

// Initialize calculation model
Api.init [ 
    "y = x1 * x2"
    "x1 = x3 + x4"
]
// Stepwise solve the model
|> solve "y"  "vals" [4N]
|> solve "x4" "vals" [6N]
|> solve "x2" "vals" [5N]
|> ignore

(** 
Prints:

> Setting variable y vals with 4</br>
> x1<..> = x3<..> + x4<..> </br>
> y[4] = x1<..> \* x2<..> </br>
> -----</br>
> Setting variable x4 vals with 6</br>
> x1<..> = x3<..> + x4[6] </br>
> y[4] = x1<..> \* x2<..> </br>
> -----</br>
> Setting variable x2 vals with 5</br>
Loop solveEq
Loop solveEq
> x1[4/5] = x3[-26/5] + x4[6] </br>
> y[4] = x1[4/5] \* x2[5] </br>
> -----</br>
> Real: 00:00:00.001, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0

*)


(**
Some more info

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/halcwb/GenSolver/tree/master/docs/content
  [gh]: https://github.com/halcwb/GenSolver
  [issues]: https://github.com/halcwb/GenSolver/issues
  [readme]: https://github.com/halcwb/GenSolver/blob/master/README.md
  [license]: https://github.com/halcwb/GenSolver/blob/master/LICENSE.txt
*)