namespace Informedica.GenSolver.Utils


/// Helper functions for `BigRational`
module BigRational = 

    open Informedica.GenUtils.Lib.BCL.BigRational
    

    /// Match an operator `op` to either
    /// multiplication, division, addition
    /// or subtraction, returns `NoOp` when
    /// the operation is neither.
    let (|Mult|Div|Add|Subtr|) op =
        match op with
        | _ when op |> opIsMult  -> Mult
        | _ when op |> opIsDiv   -> Div
        | _ when op |> opIsAdd   -> Add
        | _ when op |> opIsSubtr -> Subtr
        | _ -> failwith "Operator is not supported"


/// Helper functions for `List`
module List =

    /// Replace an element in a list
    /// when the `pred` function returns `true`.
    let replace pred x xs =
        match xs |> List.tryFindIndex pred with
        | Some(ind) ->
            (xs |> Seq.take ind |> Seq.toList) @ [x] @ 
            (xs |> Seq.skip (ind + 1) |> Seq.toList)
        | None -> xs

    let distinct xs = xs |> Seq.ofList |> Seq.distinct |> Seq.toList


    let replaceOrAdd pred x xs =
        if xs |> List.exists pred then
            xs 
            |> List.map (fun x' ->
                if x' |> pred then x else x'
            )
        else x::xs


/// Helper functions for `Array`
module Array = 
    
    let replace pred x xs = 
        xs 
        |> Array.toList 
        |> List.replace pred x
        |> List.toArray


/// Helper functions for `Option`
module Option = 

    let none _ = None




