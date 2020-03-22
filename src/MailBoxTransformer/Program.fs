open System
open MailBoxTransformer.Transformer

[<EntryPoint>]
let main argv =
    match argv with
    | [|source; destination|] -> transformMailBox source destination
    | _ -> printfn "Please provide source and destination directories as arguments."
    0 // return an integer exit code
