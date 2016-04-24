namespace Informedica.GenSolver.Utils


module NullCheck =

    /// This is the F# 4 implementation of
    /// checking whether a value is null.
    [<CompiledName("IsNull")>]
    let inline isNull (value : 'T) = 
        match value with 
        | null -> true 
        | _ -> false


module String = 

    open System

    let apply f (s: String) = f s
    
    let get = apply id

    let splitAt c s = (s |> get).Split([|c|]) 

    let contains c s = (s |> get).Contains(c) 

    let trim s    = (s |> get).Trim()

    let toLower s = (s |> get).ToLower()

    let length s = (s |> get).Length


module BigRational = 
    
    let apply f (x: BigRational) = f x

    let get = apply id

    let parse = BigRational.Parse

    let tryParse s = 
        try 
            s |> parse |> Some 
        with 
        | _ -> None

    let rec gcd a b =
        match b with
        | _  when b = 0N -> abs a
        | _ -> gcd b ((a.Numerator % b.Numerator) |> BigRational.FromBigInt)

    let toString v = (v |> get).ToString()

    let toMultipleOf d n  =
        let m = (n / d) |> BigRational.ToInt32 |> BigRational.FromInt
        if m * d < n then (m + 1N) * d else m * d


module List =

    /// Replace an element in a list
    /// when the `pred` function returns `true`.
    let replace pred x xs =
        let ind = xs |> List.findIndex pred
        (xs |> Seq.take ind |> Seq.toList) @ [x] @ 
        (xs |> Seq.skip (ind + 1) |> Seq.toList)

module Array = 
    
    let replace pred x xs = 
        xs 
        |> Array.toList 
        |> List.replace pred x
        |> List.toArray


module Option = 

    let none _ = None


