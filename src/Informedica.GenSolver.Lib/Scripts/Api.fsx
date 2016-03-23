#load "load-project.fsx"

open Swensen.Unquote


open Informedica.GenSolver.Lib


let printEqs = Solver.printEqs

let eqs = 
    Solver.init 
        [
            "total = freq * quant"
            "quant = time * rate"
            "total = comp.quant1 + comp.quant2"
        ]
eqs |> printEqs

Solver.solve eqs "total" "vals" "1,2,4" |> fst |> printEqs
Solver.solve eqs "total" "min" "1" |> fst |> printEqs
Solver.solve eqs "total" "max" "1" |> fst |> printEqs
Solver.solve eqs "total" "incr" "1" |> fst |> printEqs
Solver.solve eqs "total" "x" "1" |> fst |> printEqs

List.iter2