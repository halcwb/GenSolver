namespace GenSolver.Utils

module List =

    let replace pred x xs =
        let ind = xs |> List.findIndex pred
        (xs |> Seq.take ind |> Seq.toList) @ [x] @ 
        (xs |> Seq.skip (ind + 1) |> Seq.toList)




