#load "load-references.fsx"
#load "load-project.fsx"

open FsCheck

open Informedica.GenSolver.Lib

module VAR = Variable
module N = VAR.Name
module VR = VAR.ValueRange
module V = VR.Value
module E = Equation

let varCount v = v |> VAR.getValueRange |> VR.count

let varIsSolved v = 
    v |> varCount <= 1 &&
    v |> VAR.getValueRange |> VR.isUnrestricted |> not

let varIsSolvable =  varIsSolved >> not

let solve e = e |> E.solve, e

// Check whether an equation is solved
let isSolved = function
    | E.ProductEquation (y, xs) 
    | E.SumEquation (y, xs) ->
        [y] @ xs |> List.forall varIsSolved

// Check whether an equation will change by calc
// This is not the same as `isSolved`!! If all 
// the variables are unrestricted than the equation
// is not solvable but is also not solved.
let isSolvable = function 
    | E.ProductEquation (y, xs)
    | E.SumEquation (y, xs) ->
        [y] @ xs |> List.exists varIsSolvable &&
        [y] @ xs |> List.forall (fun v -> v |> VAR.getValueRange |> VR.isUnrestricted) |> not

let createVar n vs min incr max = 
    let min' = min |> Option.bind (V.createExc >> (VR.createMin false) >> Some)
    let max' = max |> Option.bind (V.createExc >> (VR.createMax false) >> Some)
    let incr' = incr |> Option.bind (V.createExc >> Some)

    let vs' =
        vs 
        |> List.map V.createExc
        |> Set.ofList

    let vr =
        match min, incr, max with
        | None, None, None when vs |> List.isEmpty -> VR.unrestricted
        | _ ->  VR.createExc vs' min' incr' max'

    VAR.createSucc (n |> N.createExc) vr

let y = createVar "y" [] None (Some 1N) (Some 4N)
let x1 = createVar "x1" [] (Some 1N) None None
let x2 = createVar "x2" [] None (Some 1N) None

E.createProductEqSucc y [] |> solve |> snd |> isSolvable
E.createSumEqSucc y []     |> solve

E.createProductEqSucc y [x1;x2] |> solve //|> snd |> isSolvable
E.createSumEqSucc y [x1;x2]     |> solve //|> snd |> isSolvable
