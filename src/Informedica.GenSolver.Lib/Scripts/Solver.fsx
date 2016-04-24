#load "load-references.fsx"
#load "load-project.fsx"

open FsCheck

open Informedica.GenSolver.Lib

module VAR = Variable
module N = VAR.Name
module VR = VAR.ValueRange
module V = VR.Value
module E = Equation
module S = Solver

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

let y1 = createVar "y1" [] None None (Some 4N)
let x1 = createVar "x1" [] None (Some 1N) None
let x2 = createVar "x2" [] None (Some 1N) None

let e1 = [x1;x2] |> E.createProductEqSucc y1

let y2 = createVar "y2" [] None None (Some 4N)
let x3 = createVar "x3" [] (Some 1N) None None

let e2 = [x1;x3] |> E.createProductEqSucc y2


y1 |> VAR.isSolved
e2 |> E.isSolvable
[e1;e2] |> S.solve
