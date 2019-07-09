open System.IO
open Absyn
open Eval

[<EntryPoint>]
let main argv =
    if Array.length argv < 1 then
        printfn "Please provide path to program"
        -1
    else 
        let program = File.ReadAllText (Array.head argv)
        let ast = Parser.fromString program
        printfn "AST: %A" ast
        let result = ast |> Result.map Eval.run
        match result with
        | Ok i -> 
            printfn "%A" i
            0
        | Error e ->
            printfn "Error: %A" e 
            -1
