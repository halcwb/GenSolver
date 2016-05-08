(*** hide ***)
#I "../../src/Informedica.GenSolver.Lib/Scripts"
#load "load-project.fsx"

(**
# How to use this library
The simplest way to use or tryout this library is through the Api interface
*)

open Informedica.GenSolver.Utils

module API = Informedica.GenSolver.Api

let procss s = "> " + s + " </br> "|> String.replace "*" "\*"

let printEqs = API.printEqs procss
let solve    = API.solve procss
let init     = API.init

(**
## Setting up a Calulation Model

A string based calculation model can be specified. This model can only contain sum equations and/or 
product equations. So take for example conversion from Fahrenheit to Celsius. The original
formula looks like this:

> cels = (fahr - 32) \* 5 / 9 

This can be refactored to: 

> fahr = x + 32 and </br>
> cels = x \* 5/9

However an equation can only contain variables, so the final version of the model looks like:

> fahr = x + const32 </br>
> cels = x \* const5/9

These equations can be feed as strings to the init function that will convert those strings in 2 equations
with the variables `fahr`, `cels`, `x`, `const32` and `const5/9`.
*)

let fahrCelsConv_Setup =
    init [
        "fahr = x + const32"
        "cels = x * const5/9"
    ]

(**
We can look at the generated equations by using the `printEquations` function.
*)

fahrCelsConv_Setup
|> printEqs |> ignore

(**
This wil spit out the equations:

> cels<..> = x<..> \* const5/9<..> </br>
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
    init [
        "fahr = x + const32"
        "cels = x * const5/9"
    ]
    |> solve "const32"  "vals" "32"
    |> solve "const5/9" "vals" "5/9"

(**
Now the calulation model is ready for use.
*)

(**
# Using the Calculation Model

For example convert 20 degrees Celsius to Fahrenheit:
*)

fahrCelsConv
|> solve "cels" "vals" "20"
|> ignore

(**
The result looks like:

> cels[20] = x[36] \* const5/9[5/9] </br>
> fahr[68] = x[36] + const32[32] 

However the model is more capable than simple conversion. It can also determine the value range in Celsius 
for a value range in Fahrenheit:
*)

fahrCelsConv
|> solve "fahr" "minincl" "50"
|> solve "fahr" "maxincl" "140"
|> ignore

(**

> Setting variable fahr minincl with 50 </br>
> cels[10..> = x[18..> \* const5/9[5/9] </br> 
> fahr[50..> = x[18..> + const32[32] </br> 
> 
> Setting variable fahr maxincl with 140 </br>
> cels[10..60] = x[18..108] \* const5/9[5/9] </br>
> fahr[50..140] = x[18..108] + const32[32] 

And so the Celsius range is 10 to 60.
*)

(**
# Discrete sets of values
*)

(** 
To calculate the amount of joules to perform a medical cardioversion the following formula can be used:

> joules = weight \* joules.perkg

*)

let cardioversion = 
    init [
        "joules = weight * joules.perkg"
    ]


(** 
However, a defribillator can only be set to a discrete set of joule values
*)

cardioversion 
|> solve "joules" "vals" "1,2,3,5,7,10,20,30,50,70,100,150,200,300,360"

(** 
For this set of values the amount of joule can be calculated for a weight range 
*)

|> solve "weight" "minincl" "3"
|> solve "weight" "maxincl" "150"

(** 
Typically the amount of joules necessary is about 4 joules/kg
*)

|> solve "joules.perkg" "maxincl" "4"

(** 
Then the amount of joules can be calculated rounded for the available discrete 
set of possible joules. For example for a body weight of 4 kg.
*)


|> solve "weight" "vals" "4"

(** 
It can be determined that with 10 joules the nearest possible dose of joules to 4 joules/kg 
can be reached. 
*)

|> solve "joules" "vals" "10"
|> ignore

(** 

> Setting variable joules vals with 1,2,3,5,7,10,20,30,50,70,100,150,200,300,360 </br>
> joules[1, 2, 3, 5, 7, 10, 20, 30, 50, 70, 100, 150, 200, 300, 360] = weight<..> \* joules.perkg<..> </br>
> </br>
> Setting variable weight minincl with 3
> joules[1, 2, 3, 5, 7, 10, 20, 30, 50, 70, 100, 150, 200, 300, 360] = weight[3..> \* joules.perkg<..120] </br>
> </br>
> Setting variable weight maxincl with 150
> joules[1, 2, 3, 5, 7, 10, 20, 30, 50, 70, 100, 150, 200, 300, 360] = weight[3..150] \* joules.perkg[1/150..120] </br>
> </br>
> Setting variable joules.perkg maxincl with 4 </br>
> joules[1, 2, 3, 5, 7, 10, 20, 30, 50, 70, 100, 150, 200, 300, 360] = weight[3..150] \* joules.perkg[1/150..4] </br>
> </br>
> Setting variable weight vals with 4 </br>
> joules[1, 2, 3, 5, 7, 10] = weight[4] \* joules.perkg[1/4, 1/2, 3/4, 5/4, 7/4, 5/2] </br>
> </br>
> Setting variable joules vals with 10 </br>
> joules[10] = weight[4] \* joules.perkg[5/2] </br>
> </br>
> val it : unit = ()

*)