namespace Informedica.GenSolver.Utils

module NullCheck =

    /// This is the F# 4 implementation of
    /// checking whether a value is null.
    [<CompiledName("IsNull")>]
    let inline isNull (value : 'T) = 
        match value with 
        | null -> true 
        | _ -> false

module List =

    /// Replace an element in a list
    /// when the `pred` function returns `true`.
    let replace pred x xs =
        let ind = xs |> List.findIndex pred
        (xs |> Seq.take ind |> Seq.toList) @ [x] @ 
        (xs |> Seq.skip (ind + 1) |> Seq.toList)




