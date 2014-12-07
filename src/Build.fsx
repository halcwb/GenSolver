#r @"packages\FAKE.3.11.3\tools\FakeLib.dll"

open Fake


Target "Start" (fun _ ->
    trace "Building GenSolver"
)

Target "Build GenSolver" (fun _ ->
    !! ("./**/*.fsproj")
       |> MSBuildRelease ("./GenSolver/bin/Release") "Build"
       |> Log "GenSolver output:"
)

Target "Finish" (fun _ -> 
    trace "Finished building GenSolver solution"
)

"Start"
==> "Build GenSolver"
==> "Finish"

RunTargetOrDefault "Finish"     
