#load "load-project.fsx"

open Informedica.GenSolver.Lib

let dto = Variable.Dto.create "test"
dto.Values <- [|1N..1N..10N|]
dto.Increment <- Some(2N)
dto.Maximum <- Some(9N)

dto |> Variable.fromDto
