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

let varIsSolved v = v |> varCount <= 1

let solve e = e |> E.solve, e

let isSolved = function
    | E.ProductEquation (y, xs) 
    | E.SumEquation (y, xs) ->
        y |> varIsSolved &&
        xs |> List.forall varIsSolved

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

let y = createVar "y" [] None None None
let x1 = createVar "x1" [1N] None None None
let x2 = createVar "x2" [2N] None None None

E.createProductEqSucc y [] |> solve
E.createSumEqSucc y []     |> solve

E.createProductEqSucc y [x1;x2] |> solve
E.createSumEqSucc y [x1;x2]     |> solve
