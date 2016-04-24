#load "load-references.fsx"
#load "load-project.fsx"

open FsCheck

open Informedica.GenSolver.Lib

module VAR = Variable
module N = VAR.Name
module VR = VAR.ValueRange
module V = VR.Value
module E = Equation

let varCount v = VAR.count

let varIsSolved v = VAR.isSolved

let varIsSolvable =  VAR.isSolvable

let solve e = e |> E.solve, e

let isSolved = E.isSolved

let isSolvable = E.isSolvable

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
        | _ ->  VR.createExc false vs' min' incr' max'

    VAR.createSucc (n |> N.createExc) vr

let y = createVar "y" [] None None (Some 4N)
let x1 = createVar "x1" [] None (Some 1N) None
let x2 = createVar "x2" [] None (Some 1N) None

E.createProductEqExc y [] |> solve |> snd |> isSolvable
E.createSumEqExc y []     |> solve

E.createProductEqExc y [x1;x2] |> solve //|> snd |> isSolvable
E.createSumEqExc y [x1;x2]     |> solve //|> snd |> isSolvable

E.createProductEqExc y [x1;x2]

