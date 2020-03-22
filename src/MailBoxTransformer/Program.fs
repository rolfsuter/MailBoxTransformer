open System
open System.IO

open MailBoxTransformer
// open MailBoxTransformer.Tree
open MailBoxTransformer.Transformer

let rec readTree path =
    if File.Exists path
    then LeafNode path
    else
        let dirsAndFiles = Directory.EnumerateFileSystemEntries path
        let branches = Seq.map readTree dirsAndFiles |> Seq.toList
        InternalNode (path, branches)

/// Transforms the MailBoxes of the source directory and writes them 
/// to the destination.
let transformMailBox source destination = 
    let sourceTree = readTree source |> Tree.bimap FileInfo FileInfo
    let mailBoxTree = 
        sourceTree   
        |> findMailBoxes  // unknown files generate NONE leaves
        |> Tree.choose readMailBox // filter out NONE leaves and read the Mail Boxes
    let destinationTree =
        Option.map (moveTo destination >> calculateMoves) mailBoxTree
    Option.iter writeTree destinationTree


[<EntryPoint>]
let main argv =
    match argv with
    | [|source; destination|] -> transformMailBox source destination
    | _ -> printfn "Please provide source and destination directories as arguments."
    0 // return an integer exit code
