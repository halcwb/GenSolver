namespace Informedica.GenSolver.Utils


/// Function to perform a safe null check
module NullCheck =

    /// This is the F# 4 implementation of
    /// checking whether a value is null.
    [<CompiledName("IsNull")>]
    let inline isNull (value : 'T) = 
        match value with 
        | null -> true 
        | _ -> false


/// Helper functions for `System.String`
module String = 

    open System
    
    module UT = Informedica.GenUtils.Lib.BCL.String

    let apply = UT.apply
    
    let get = UT.get

    let splitAt = UT.splitAt

    let contains = UT.contains

    let trim = UT.trim

    let toLower = UT.toLower

    let length = UT.length

    let isNullOrWhiteSpace = UT.isNullOrWhiteSpace

    let replace = UT.replace


/// Helper functions for `BigRational`
module BigRational = 

    module BR = Informedica.GenUtils.Lib.BCL.BigRational
    
    let apply= BR.apply

    let get = BR.get

    let parse = BR.parse

    let fromInt = BR.fromInt

    let tryParse = BR.tryParse

    let gcd = BR.gcd

    let toString = BR.toString

    /// Convert an optional `Value` to a `string`.
    /// If `None` then return empty `string`.
    let optToString = BR.optToString

    /// Convert `n` to a multiple of `d`.
    let toMultipleOf = BR.toMultipleOf

    /// Checks whether `v` is a multiple of `incr`
    let isMultiple = BR.isMultiple

    let zero = BR.zero

    let one = BR.one

    let two = BR.two

    let three = BR.three

    /// Check whether the operator is subtraction
    let opIsSubtr = BR.opIsSubtr

    /// Check whether the operator is addition
    let opIsAdd = BR.opIsAdd

    /// Check whether the operator is multiplication
    let opIsMult = BR.opIsMult

    /// Check whether the operator is divsion
    let opIsDiv = BR.opIsDiv

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




