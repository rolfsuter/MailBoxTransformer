open System.IO

// Tree datastructure and processing functions
// Reference: https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-3b

type Tree<'LeafData,'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> seq

// cata is bottom-up recursion
let rec cata fLeaf fNode (tree:Tree<'LeafData,'INodeData>) :'r = 
    let recurse = cata fLeaf fNode  
    match tree with
    | LeafNode leafInfo -> 
        fLeaf leafInfo 
    | InternalNode (nodeInfo,subtrees) -> 
        fNode nodeInfo (subtrees |> Seq.map recurse)

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

// map function
let rec map fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = 
    let recurse = map fLeaf fNode  
    match tree with
    | LeafNode leafInfo -> 
        let newLeafInfo = fLeaf leafInfo
        LeafNode newLeafInfo 
    | InternalNode (nodeInfo,subtrees) -> 
        let newNodeInfo = fNode nodeInfo
        let newSubtrees = subtrees |> Seq.map recurse 
        InternalNode (newNodeInfo, newSubtrees)

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

let createMailBoxInfo path =
    { MailBoxInfo.Name = Path.GetFileName path
      MailBoxInfo.Path = path } 

let createFolderInfo path =
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

let folder = @"/Users/rolf/Documents/Mail_Export_backup"

let source = 
    { Name = Path.GetFileName folder 
      Path = folder }

source 
|> fromFolder
|> iter (fun x -> printfn "# MailBox: %s" x.Path) (fun y -> printfn "# Folder:  %s" y.Path) 

let dirListing mailBoxItem =
    let mapMailBox (mboxi:MailBoxInfo) = 
        sprintf "%s"  mboxi.Path
    let mapDir (diri:FolderInfo) = 
        diri.Path 
    map mapMailBox mapDir mailBoxItem

source
|> fromFolder
|> dirListing 
|> iter (printfn "%s") (printfn "\n%s")

