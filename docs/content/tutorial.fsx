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

let dto = 
    Variable.Dto.createNew "Test"
    |> Variable.Dto.setMin "2"
    |> Variable.Dto.setMax "10"

let value = dto |> Variable.Dto.fromDtoExc

Variable.Dto.toString dto

let eqs = [|
    "total = frequency * quantity"
    "quantity = rate * duration"
|]

// let api = Solver.Api (fun r m -> printfn "%A" m; r) (fun r m -> printfn "%A"; r)
// eqs |> api.init
//     |> api.setProp "frequency" "incr" "1"
//     |> api.setProp "frequency" "max" "24"
//     |> api.setProp "total" "max" "12"
//     |> api.setProp "quantity" "incr" "1"
