(*** hide ***)
#I "../../src/Informedica.GenSolver.Lib/Scripts"
#load "load-project.fsx"

(**
# How to use this library
The simplest way to use or tryout this library is through the Api interface
*)

module API = Informedica.GenSolver.Api

(**
## Setting up a Calulation Model

A string based calculation model can be specified. This model can only contain sum equations and/or 
product equations. So take for example conversion from Fahrenheit to Celsius. The original
formula looks like this:

> cels = (fahr - 32) * 5 / 9 

This can be refactored to: 

> fahr = x + 32 and </br>
> cels = x + 5/9

However an equation can only contain variables, so the final version of the model looks like:

> fahr = x + const32 </br>
> cels = x + const5/9

These equations can be feed as strings to the init function that will convert those strings in 2 equations
with the variables `fahr`, `cels`, `x`, `const32` and `const5/9`.
*)

let fahrCelsConv_Setup =
    API.init [
        "fahr = x + const32"
        "cels = x * const5/9"
    ]

(**
We can look at the generated equations by using the `printEquations` function.
*)

fahrCelsConv_Setup
|> API.printEqs |> ignore

(**
This wil spit out the equations:

> cels<..> = x<..> * const5/9<..> </br>
> fahr<..> = x<..> + const32<..> 

*)


(**
Having obtained the model `fahrCelsConv_Setup`, the model can be solved by the `API.solve` function. 
This will print out the intermediate results and return an updated model. 
The following properties can be set for each variable in the equation:

* `unr`: Whether the value range for the variable is unrestricted
* `vals`: A list of possible values for the variable
* `minincl`: The lower boundary for the variable, inclusive that value
* `minexcl`: The lower boundary for the variable, exclusive that value
* `increment`: Each possible of a value for the variable should be a multiple of the increment for that variable
* `maxincl`: The upper boundary for the variable, inclusive that value
* `maxexcl`: The upper boundary for the variable, exclusive that value

So, the first step to further prepare the model is to set the two *constant* variables to a single possible value
*)

let fahrCelsConv =
    API.init [
        "fahr = x + const32"
        "cels = x * const5/9"
    ]
    |> API.solve "const32"  "vals" "32"
    |> API.solve "const5/9" "vals" "5/9"

(**
Now the calulation model is ready for use.
*)

(**
# Using the Calculation Model

For example convert 20 degrees Celsius to Fahrenheit:
*)

fahrCelsConv
|> API.solve "cels" "vals" "20"
|> ignore

(**
The result looks like:

> cels[20] = x[36] * const5/9[5/9] </br>
> fahr[68] = x[36] + const32[32] 

However the model is more capable than simple conversion. It can also determine the value range in Celsius 
for a value range in Fahrenheit:
*)

fahrCelsConv
|> API.solve "fahr" "minincl" "50"
|> API.solve "fahr" "maxincl" "140"
|> ignore

(**

> Setting variable fahr minincl with 50 </br>
> cels[10..> = x[18..> * const5/9[5/9] </br> 
> fahr[50..> = x[18..> + const32[32] </br> 
> 
> Setting variable fahr maxincl with 140 </br>
> cels[10..60] = x[18..108] * const5/9[5/9] </br>
> fahr[50..140] = x[18..108] + const32[32] 

And so the Celsius range is 10 to 60.
*)