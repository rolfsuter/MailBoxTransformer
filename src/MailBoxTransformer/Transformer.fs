namespace MailBoxTransformer

module Transformer =

    open System
    open System.IO

    type MailBoxFile = 
        { File: FileInfo }

    type Move = 
        { Source : FileInfo; 
          Destination : FileInfo }
            
    // type MailBoxInfo = 
    //     { Name: string 
    //       Path: string }

    // type FolderInfo = 
    //     { Name: string 
    //       Path: string }

    // type MailBoxItem = Tree<MailBoxInfo, FolderInfo>

    // let createMailBoxInfo (path:string) =
    //     { MailBoxInfo.Name = Path.GetFileName path
    //       MailBoxInfo.Path = path } 

    // let createFolderInfo (path:string) =
    //     { FolderInfo.Name = Path.GetFileName path
    //       FolderInfo.Path = path } 


    // let fromMailBox (mailBoxInfo:MailBoxInfo) =
    //     LeafNode mailBoxInfo

    // let rec fromFolder (folderInfo:FolderInfo) =
    //     let isMailBox (path: string) =
    //         path.EndsWith(".mbox")

    //     let subItems = seq {
    //         let dirs = Directory.EnumerateDirectories folderInfo.Path
    //         yield! 
    //             dirs 
    //             |> Seq.filter isMailBox 
    //             |> Seq.map (createMailBoxInfo >> fromMailBox)
    //         yield! 
    //             dirs 
    //             |> Seq.filter (isMailBox >> not) 
    //             |> Seq.map (createFolderInfo >> fromFolder) }
    //     InternalNode (folderInfo, subItems)

    // // --



    let isMailBoxFile (tree:Tree<FileInfo, FileInfo>) =
        match tree with
        | LeafNode x when x.Name = "mbox" -> true
        | _ -> false 

    let isMailBox (tree:Tree<FileInfo, FileInfo>) =
        match tree with
        | InternalNode (x, ys) when x.Name.EndsWith(".mbox") && (Seq.exists isMailBoxFile ys) 
            -> true 
        | _ -> false
        

    // let testMailBox =
    //     InternalNode ("EPFL.mbox", seq{ LeafNode "mbox" } )

    // testMailBox |> Tree.bimap (FileInfo) (FileInfo) |> isMailBox







    // // this works
    // let rec printMailBoxes (tree:Tree<FileInfo, FileInfo>) =
    //     match tree with
    //     | LeafNode _ -> ()
    //     // | node when isMailBox node ->
    //     //     let (InternalNode(x, ys)) = node 
    //     //     printfn "#mbox: %s" x.FullName
    //     | InternalNode (x, ys) when InternalNode (x, ys) |> isMailBox ->
    //         printfn "#mbox: %s" x.FullName
    //     | InternalNode (x, ys) -> 
    //         printfn "#node: %s" x.FullName
    //         Seq.iter printMailBoxes ys

    // // this works
    // let printMailBoxes2 (tree:Tree<FileInfo, FileInfo>) =
    //     let rec recurse x =
    //         match x with
    //         | LeafNode _ -> ()
    //         | InternalNode (x, ys) when InternalNode (x, ys) |> isMailBox ->
    //             printfn "#mbox: %s" x.FullName
    //         | InternalNode (x, ys) -> 
    //             printfn "#node: %s" x.FullName
    //             Seq.iter recurse ys
    //     recurse tree      

    // // this works :-)

    /// Look for the MailBoxes in a given Tree with directores and files. Returns a new tree with
    /// paths to the MailBoxes. Returns NONE if a leaf or node is not a MailBox.
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

    // // ---






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



    // ### Composition ###
    // let folder = @"/Users/rolf/Documents/Mail_test"

    // let source = 
    //     { Name = Path.GetFileName folder 
    //       Path = folder }

    // source 
    // |> fromFolder
    // |> Tree.iter (fun x -> printfn "# MailBox: %s" x.Path) (fun y -> printfn "# Folder:  %s" y.Path) 

    // let dirListing mailBoxItem =
    //     let mapMailBox (mboxi:MailBoxInfo) = 
    //         sprintf "%s"  mboxi.Path
    //     let mapDir (diri:FolderInfo) = 
    //         diri.Path 
    //     Tree.bimap mapMailBox mapDir mailBoxItem



    // let destination = @"/Users/rolf/Desktop/test"

    // Composition



    // transformMailBox folder destination