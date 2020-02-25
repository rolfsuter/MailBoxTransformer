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
        subtrees |> Seq.iter recurse 
        fNode nodeInfo

type MailBoxInfo = 
    { Name: string 
      Path: string }

type FolderInfo = 
    { Name: string 
      Path: string}

type MailBoxItem = Tree<MailBoxInfo, FolderInfo>



let folder = @"/Users/rolf/Documents/Mail_Export_backup/"
let target = @"/Users/rolf/Documents/Mail_renamed/"


let folders = Directory.GetDirectories(folder, "*mbox", System.IO.SearchOption.AllDirectories)

folders
|> List.ofArray
// |> List.map (Path.GetFileName)
|> List.iter (printfn "%s")

