#load "load-references.fsx"
#load "load-project.fsx"

let s1 = [1..10] |> Set.ofList
let s2 = Set.empty
s1 |> Set.intersect s2
