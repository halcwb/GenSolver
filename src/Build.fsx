#r @"packages\FAKE.3.11.3\tools\FakeLib.dll"

open Fake

Target "Default" (fun _ ->
    trace "Building GenSolver"
)

RunTargetOrDefault "Default"
