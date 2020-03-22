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

let writeTree t =
    let copy m =
        Directory.CreateDirectory m.Destination.DirectoryName |> ignore
        m.Source.CopyTo m.Destination.FullName |> ignore
        printfn "Copied to %s" m.Destination.FullName
    let compareFiles m =
        let sourceStream = File.ReadAllBytes m.Source.FullName
        let destinationStream = File.ReadAllBytes m.Destination.FullName
        sourceStream = destinationStream
    let move m =
        copy m
        // if compareFiles m then m.Source.Delete ()
    Tree.iterLeaves move t

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
