#load "load-project.fsx"

open Informedica.GenSolver.Lib

/// Calculation with `Values`
let v1 = Variable.Values.createValues [2N]
let v2 = Variable.Values.createValues [4N]


let dto = 
    let dto = Variable.Dto.createNew "test"
    let vals = [|1N..1N..20N|] |> Array.map (fun n -> n.ToString())
    let dto = dto |> Variable.Dto.setVals vals
    let dto = dto |> Variable.Dto.setIncr "2"
    let dto = dto |> Variable.Dto.setMax "10"
    dto

dto |> Variable.fromDto
