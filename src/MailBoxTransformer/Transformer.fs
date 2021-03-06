namespace MailBoxTransformer

module Transformer =

    open System
    open System.IO

    type MailBoxFile = 
        { File: FileInfo }

    type Move = 
        { Source : FileInfo; 
          Destination : FileInfo }
           
    let isMailBoxFile (tree:Tree<FileInfo, FileInfo>) =
        match tree with
        | LeafNode x when x.Name = "mbox" -> true
        | _ -> false 

    let isMailBox (tree:Tree<FileInfo, FileInfo>) =
        match tree with
        | InternalNode (x, ys) when x.Name.EndsWith(".mbox") && (Seq.exists isMailBoxFile ys) 
            -> true 
        | _ -> false

    /// Look for the MailBoxes in a given Tree with directores and files. Returns a new tree with
    /// paths to the MailBoxes. Returns NONE if a leaf or node is not a MailBox.
    let rec findMailBoxes (sourceTree:Tree<FileInfo, FileInfo>) =
            match sourceTree with
            | LeafNode x ->                 LeafNode None
            | InternalNode (x, ys) when InternalNode (x, ys) |> isMailBox ->
                LeafNode (Some x)
            | InternalNode (x, ys) -> 
                let newNode = x
                let newSubtrees = ys |> Seq.map findMailBoxes
                InternalNode (newNode, newSubtrees)

    /// Creates a tree with new structure. The leaves carry the source path of the 
    /// MailBoxFiles and the nodes the directory name without path. The root node
    /// contains the destination path.
    let moveTo destination t = //(t:Tree<MailBoxFile, FileInfo>) =
        let newTree =
            Tree.bimap (fun leaf -> leaf.File) (fun (node:FileInfo) -> node.Name + ".sbd") t
        match newTree with
        | InternalNode (_, xs) -> InternalNode(destination, xs)             // Replace the path in the root node
        | _  -> Tree.node destination (seq { Tree.leaf(FileInfo("")) } )    // Default value if there is a failure/no mailbox

    /// Calculates the mailbox moves based on the source path in the leaves and
    /// the tree structere with the directory names stored in the tree nodes.
    let calculateMoves =
        let replaceDirectory (f : FileInfo) d =
            FileInfo (Path.Combine (d, f.Name))
        let rec imp path = function
            | LeafNode (x:FileInfo) ->
                LeafNode 
                    { Source =  (x.FullName, "mbox") |> Path.Combine |> FileInfo
                      Destination = 
                        let p =  (replaceDirectory x path).FullName
                        (Path.GetDirectoryName(p), Path.GetFileNameWithoutExtension(p))
                        |> Path.Combine
                        |> FileInfo }
            | InternalNode (x, xs) ->
                let newNPath = Path.Combine (path, x)
                Tree.node newNPath (Seq.map (imp newNPath) xs)
        imp ""