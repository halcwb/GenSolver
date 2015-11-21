(*** hide ***)
#I "../../src/Informedica.GenSolver.Lib/Scripts"
#load "load-project.fsx"

(**
# How to use this library
First open up the name space
*)

open Informedica.GenSolver.Lib

(**
## How to create a value
Then create a value
*)

let value = Variable.Value.create 1N

(** 
Or a list of `BigRational` can be used to create values
*)
let vals = Variable.Values.create [1N..2N..10N] None None None