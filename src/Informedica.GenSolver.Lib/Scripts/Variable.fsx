#load "load-project.fsx"

open Informedica.GenSolver.Lib

/// Calculation with `Values`
let v1 = Variable.Values.createValues [2N]
let v2 = Variable.Values.createValues [4N]

v1 * v2 |> =* 

let dto = Variable.Dto.create "test"
dto.Values <- [|1N..1N..10N|]
dto.Increment <- Some(2N)
dto.Maximum <- Some(9N)

dto |> Variable.fromDto
