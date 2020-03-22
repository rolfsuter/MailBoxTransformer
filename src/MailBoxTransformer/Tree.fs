namespace MailBoxTransformer

// Tree datastructure and processing functions
// adapted from 
// * https://github.com/ploeh/picture-archivist/blob/master/FSharp/ArchivePictures/Tree.fs (Copyright (c) 2018 Mark Seemann)
// * https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-3b (Copyright by Scott Wlaschin)
// * https://gist.github.com/swlaschin/2b06fe266e3299a656c1 (Copyright by Scott Wlaschin)

type Tree<'LeafData,'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> seq

module Tree = 
    /// Creates a leaf
    let leaf x = LeafNode x

    /// Creates a node
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

//     // fold is top-down iteration    
//     let rec fold fLeaf fNode acc (tree:Tree<'LeafData,'INodeData>) :'r = 
//         let recurse = fold fLeaf fNode  
//         match tree with
//         | LeafNode leafInfo -> 
//             fLeaf acc leafInfo 
//         | InternalNode (nodeInfo,subtrees) -> 
//             // determine the local accumulator at this level
//             let localAccum = fNode acc nodeInfo
//             // thread the local accumulator through all the subitems using Seq.fold
//             let finalAccum = subtrees |> Seq.fold recurse localAccum 
//             // ... and return it
//             finalAccum

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

//     /// map on the leaves
//     let map f = bimap f id

//     // iter function
//     let rec iter fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = 
//         let recurse = iter fLeaf fNode  
//         match tree with
//         | LeafNode leafInfo -> 
//             fLeaf leafInfo
//         | InternalNode (nodeInfo,subtrees) -> 
//             fNode nodeInfo
//             subtrees |> Seq.iter recurse 

    /// iter through the leaves. Does not act on the nodes.
    let rec iterLeaves f t =
        let recurse = iterLeaves f
        match t with
        | LeafNode leafInfo -> f leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            subtrees |> Seq.iter recurse

//     /// implementation variant of iterLeaves. Requires validation.
//     let iterLeaves2 f t = iter f ignore t
    