// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#I "Scripts/"
#load "load-references.fsx"


// Define your library scripting code here
type variable =
    {
        Name: string
        Value: int
    }


module Variable =

    let create n v = { Name = n; Value = v }

