open System
open System.IO

// Tree datastructure and processing functions
// Reference: https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-3b

type Tree<'LeafData,'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> seq

module Tree = 
    /// Creates a leaf. Does not add much value
    let leaf x = LeafNode x

    /// Create a node
    let node x xs = InternalNode (x, xs)

    // cata is bottom-up recursion
    let rec cata fLeaf fNode (tree:Tree<'LeafData,'INodeData>) :'r = 
        let recurse = cata fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf leafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            fNode nodeInfo (subtrees |> Seq.map recurse)

    // choose is derived from https://blog.ploeh.dk/2019/09/16/picture-archivist-in-f/ (swapped the node an leave function)
    let choose f = cata (f >> Option.map LeafNode) (fun x -> Seq.choose id >> node x >> Some)

    // fold is top-down iteration    
    let rec fold fLeaf fNode acc (tree:Tree<'LeafData,'INodeData>) :'r = 
        let recurse = fold fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf acc leafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            // determine the local accumulator at this level
            let localAccum = fNode acc nodeInfo
            // thread the local accumulator through all the subitems using Seq.fold
            let finalAccum = subtrees |> Seq.fold recurse localAccum 
            // ... and return it
            finalAccum

    // bimap function
    /// map on the leaves as well as on the nodes
    let rec bimap fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = 
        let recurse = bimap fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            let newLeafInfo = fLeaf leafInfo
            LeafNode newLeafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            let newNodeInfo = fNode nodeInfo
            let newSubtrees = subtrees |> Seq.map recurse 
            InternalNode (newNodeInfo, newSubtrees)

    /// map on the leaves
    let map f = bimap f id

    // iter function
    let rec iter fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = 
        let recurse = iter fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf leafInfo
        | InternalNode (nodeInfo,subtrees) -> 
            fNode nodeInfo
            subtrees |> Seq.iter recurse 

        
type MailBoxInfo = 
    { Name: string 
      Path: string }

type FolderInfo = 
    { Name: string 
      Path: string }

type MailBoxItem = Tree<MailBoxInfo, FolderInfo>

let createMailBoxInfo (path:string) =
    { MailBoxInfo.Name = Path.GetFileName path
      MailBoxInfo.Path = path } 

let createFolderInfo (path:string) =
    { FolderInfo.Name = Path.GetFileName path
      FolderInfo.Path = path } 


let fromMailBox (mailBoxInfo:MailBoxInfo) =
    LeafNode mailBoxInfo

let rec fromFolder (folderInfo:FolderInfo) =
    let isMailBox (path: string) =
        path.EndsWith(".mbox")

    let subItems = seq {
        let dirs = Directory.EnumerateDirectories folderInfo.Path
        yield! 
            dirs 
            |> Seq.filter isMailBox 
            |> Seq.map (createMailBoxInfo >> fromMailBox)
        yield! 
            dirs 
            |> Seq.filter (isMailBox >> not) 
            |> Seq.map (createFolderInfo >> fromFolder) }
    InternalNode (folderInfo, subItems)

// --



let isMailBoxFile (tree:Tree<FileInfo, FileInfo>) =
    match tree with
    | LeafNode x when x.Name = "mbox" -> true
    | _ -> false 

let isMailBox (tree:Tree<FileInfo, FileInfo>) =
    match tree with
    | InternalNode (x, ys) when x.Name.EndsWith(".mbox") && (Seq.exists isMailBoxFile ys) 
        -> true 
    | _ -> false
    

let testMailBox =
    InternalNode ("EPFL.mbox", seq{ LeafNode "mbox" } )

testMailBox |> Tree.bimap (FileInfo) (FileInfo) |> isMailBox




let rec readTree path =
    if File.Exists path
    then LeafNode path
    else
        let dirsAndFiles = Directory.EnumerateFileSystemEntries path
        let branches = Seq.map readTree dirsAndFiles |> Seq.toList
        InternalNode (path, branches)


// this works
let rec printMailBoxes (tree:Tree<FileInfo, FileInfo>) =
    match tree with
    | LeafNode _ -> ()
    // | node when isMailBox node ->
    //     let (InternalNode(x, ys)) = node 
    //     printfn "#mbox: %s" x.FullName
    | InternalNode (x, ys) when InternalNode (x, ys) |> isMailBox ->
        printfn "#mbox: %s" x.FullName
    | InternalNode (x, ys) -> 
        printfn "#node: %s" x.FullName
        Seq.iter printMailBoxes ys

// this works
let printMailBoxes2 (tree:Tree<FileInfo, FileInfo>) =
    let rec recurse x =
        match x with
        | LeafNode _ -> ()
        | InternalNode (x, ys) when InternalNode (x, ys) |> isMailBox ->
            printfn "#mbox: %s" x.FullName
        | InternalNode (x, ys) -> 
            printfn "#node: %s" x.FullName
            Seq.iter recurse ys
    recurse tree      

// this works :-)
let rec findMailBoxes (sourceTree:Tree<FileInfo, FileInfo>) =
    // let rec recurse  (x:Tree<FileInfo, FileInfo>) =
        match sourceTree with
        | LeafNode x -> 
            printfn "#unknown: %s" x.Name
            LeafNode None
        | InternalNode (x, ys) when InternalNode (x, ys) |> isMailBox ->
            printfn "#mbox   : %s" x.FullName
            // let s = seq { yield LeafNode x }
            // s
            LeafNode (Some x)
        | InternalNode (x, ys) -> 
            printfn "#node   : %s" x.FullName
            let newNode = x
            let newSubtrees = ys |> Seq.map findMailBoxes//recurse
            InternalNode (newNode, newSubtrees)
    // recurse sourceTree  

// ---
type MailBoxFile = 
    { File: FileInfo }

let readMailBox file =
    file |> Option.map (fun x -> { File = x } )



// Creates tree with new structure
let moveTo destination t = //(t:Tree<MailBoxFile, FileInfo>) =
    let newTree =
        Tree.bimap (fun leave -> leave.File) (fun (node:FileInfo) -> node.Name) t
    match newTree with
    | InternalNode (_, xs) -> InternalNode(destination, xs)
    | _  -> Tree.node destination (seq { Tree.leaf(FileInfo("")) } )


// ### Composition ###
let folder = @"/Users/rolf/Documents/Mail_Export_backup"

let source = 
    { Name = Path.GetFileName folder 
      Path = folder }

source 
|> fromFolder
|> Tree.iter (fun x -> printfn "# MailBox: %s" x.Path) (fun y -> printfn "# Folder:  %s" y.Path) 

let dirListing mailBoxItem =
    let mapMailBox (mboxi:MailBoxInfo) = 
        sprintf "%s"  mboxi.Path
    let mapDir (diri:FolderInfo) = 
        diri.Path 
    Tree.bimap mapMailBox mapDir mailBoxItem

let workflow1 = 
    source
    |> fromFolder
    |> dirListing 
    |> Tree.iter (printfn "%s") (printfn "\n%s")

let workflow2 = 
    source
    |> fromFolder
    |> dirListing 
    |> Tree.bimap (printfn "%s") (printfn "%s")
    |> Tree.fold (fun a x -> () ) (fun a x -> () ) ()

let workflow3 =
    let destination = @""

    let sourceTree = readTree folder |> Tree.bimap FileInfo FileInfo
    let mailBoxTree = 
        sourceTree   
        |> findMailBoxes  // unknown files generate NONE leaves
        |> Tree.choose readMailBox // filter out NONE leaves and read the Mail Boxes
        
    // mailBoxTree
    // |> Option.map (Tree.iter (fun x -> printfn "          %s" x.File.FullName) (fun x -> printfn "          %s" x.FullName))

    // let destinationTree =
        // Option.map (moveTo destination >> calculateMoves) mailBoxTree
    Option.map (moveTo destination) mailBoxTree
        // |> Option.iter (Tree.iter (printfn "%A") (printfn "%A"))
    
    // Option.iter writeTree destinationTree

// Composition
let transformMailBox source destination =
    let sourceTree = readTree source |> Tree.bimap FileInfo FileInfo
    sourceTree

transformMailBox folder ()