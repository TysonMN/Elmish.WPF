module StructFilters

open System.Diagnostics
open System.IO
open System.Windows
open Elmish
open Elmish.WPF
open ShowDialog.subModelSeq.Views

module App =
    open System.Reflection
    open Newtonsoft.Json
    open Domain

    let mutable window = null
    let mkNewWindow () = 
        window <- StructFiltersWindow()
        window

    let mutable unmodifiedModel = { 
        MsgType = { ID = 0; Name = "NullMsg" }
        DummyRoot = FieldData.empty |> RoseTree.asLeaf
    }

    let mutable configFolderPath = ""

    let getConfigFolder path =
        let folder = Path.Combine(path, "StructFilters")
        if not (Directory.Exists(folder)) then Directory.CreateDirectory(folder) |> ignore
        folder

    let getConfigFile structName =
        Path.Combine(configFolderPath, sprintf "%s.json" structName)

    let deserializeStruct file =
        let str = File.ReadAllText file
        JsonConvert.DeserializeObject<RoseTree<FieldData>>(str)

    let loadSavedChanges dummyRoot = 
        let updateFromFile (field: RoseTree<FieldData>) =
            let updateFields fileFld inMemoryFld =
                inMemoryFld.Children 
                |> List.map (fun f ->
                    fileFld.Children
                    |> List.tryFind (fun fld -> fld.Data.Name = f.Data.Name)
                    |> Option.map (fun fld -> { f with Data = fld.Data })
                    |> Option.defaultValue f
                )
            let file = getConfigFile field.Data.Type
            if (not field.Children.IsEmpty) && File.Exists file then
                let ff = deserializeStruct file
                { field with (*Data = ff.Data;*) Children = updateFields ff field }
            else field

        let rec traverse fld =
            let fields' =
                fld.Children
                |> List.map (fun f -> 
                    updateFromFile f
                    |> traverse
                )
            { fld with Children = fields' }
        traverse dummyRoot

    let init (msgTypeID: int) (msgTypeName: string) (parentStructName: string) measureElapsedTime () =
        // TODO: Look up the headerFileName path based on the DLL version loaded.
        // TODO: Get the struct definitions without distributing the v*struct.h files.
        let exeFolderPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        let headerFileName = Path.Combine(exeFolderPath, """Data\structs.h""")
        let parentStruct = Translation.translate headerFileName parentStructName
        let dummyRoot =
            if parentStruct.Data.Type = "" then
                // Add one child to prevent a StackOverflow in Elmish.WPF.ViewModel.initializeBinding function.
                let dummyChild = FieldData.create "Parent struct not found" ""
                [ dummyChild |> RoseTree.asLeaf ] |> FieldData.asDummyRoot
            else
                [ parentStruct ] |> FieldData.asDummyRoot
        measureElapsedTime "App.init"
        unmodifiedModel <- 
            { MsgType = { ID = msgTypeID; Name = msgTypeName }
              DummyRoot = loadSavedChanges dummyRoot
            }
        unmodifiedModel

    type SubtreeMsg =
        | GmlSetChecked of gmlChecked: bool
        | CmlSetChecked of cmlChecked: bool
        | CmlChangeFieldSetChecked of changeChecked: bool
        | CmlEntitySetChecked of entityChecked: bool

    [<Struct>]
    type Msg =
        | SubtreeMsg of RoseTreeMsg<FieldId, SubtreeMsg>
        | Save
        | Cancel

  /// Updates the field using the specified function if the ID matches,
  /// otherwise passes the field through unmodified.
    //let updateField f id fld =
    //    if fld.Id = id then f fld else fld

    //let setIsGml isChecked = updateField <| FieldData.setIsGml isChecked
    //let setIsCml isChecked = updateField <| FieldData.setIsCml isChecked
    //let setIsCmlChangeField isChecked = updateField <| FieldData.setIsCmlChangeField isChecked
    //let setIsCmlEntity isChecked = updateField <| FieldData.setIsCmlEntity isChecked

    let getChangedStructs m =
        let isNotEqual (fldA: RoseTree<FieldData>) (fldB: RoseTree<FieldData>) = fldA.Data <> fldB.Data
        // Depth First Search
        let rec diff acc oldFld newFld =
            match oldFld.Children with
            | [] -> acc
            | fields -> 
                let acc' =
                    if (fields, newFld.Children) ||> List.exists2 isNotEqual then
                        RoseTree.depthAtMost1 newFld :: acc
                    else
                        acc

                (acc', fields, newFld.Children)
                |||> List.fold2 (fun accu oldChild newChild -> diff accu oldChild newChild)
        
        diff [] unmodifiedModel.DummyRoot.Children.Head m.DummyRoot.Children.Head

    let serializeStructs flds =
        let ser = JsonSerializer()
        ser.Formatting <- Newtonsoft.Json.Formatting.Indented
        flds
        |> List.iter (fun (f: RoseTree<FieldData>) ->
            let file = sprintf "%s.json" f.Data.Type
            let filePath = Path.Combine(configFolderPath, file)
            use writer = File.CreateText(filePath)
            ser.Serialize(writer, f)
        )

    let updateFieldData = function
        | GmlSetChecked isChecked ->
            //RoseTree.nodesCount <- 1 // just for testing how many nodes are in the tree.
            // Instead of using the mutable RoseTree.nodesCount, pass the tree into RoseTree.size
            isChecked |> FieldData.setIsGml
        | CmlSetChecked isChecked ->
            isChecked |> FieldData.setIsCml
        | CmlChangeFieldSetChecked isChecked ->
            isChecked |> FieldData.setIsCmlChangeField
        | CmlEntitySetChecked isChecked ->
            isChecked |> FieldData.setIsCmlEntity

    let updateSubtree msg = msg |> updateFieldData |> RoseTree.mapData

    let hasId id (fd: RoseTree<FieldData>) = fd.Data.Id = id

    let mapDummyRoot f m =
        { m with DummyRoot = m.DummyRoot |> f }

    let update msg m =
        match msg with
        | SubtreeMsg msg ->
            msg |> RoseTree.update hasId updateSubtree |> mapDummyRoot <| m
        | Save -> 
            let changedStructs = getChangedStructs m
            serializeStructs changedStructs
            window.Close()
            m
        | Cancel -> 
            window.Close()
            m

    let private checkboxVisibility fd =
        if FieldData.isParentStruct fd then "Collapsed" else "Visible"

    let rec fieldBindings level () : Binding<Model * (RoseTree<FieldData> * RoseTree<FieldData>), RoseTreeMsg<FieldId, SubtreeMsg>> list = [
        // TODO: Hide ignored fields: spares and pads.
        "Name" |> Binding.oneWay(fun (_, (_, fd)) -> fd.Data.Name)
        "Type" |> Binding.oneWay(fun (_, (_, fd)) -> fd.Data.Type)
        "IsGml" |> Binding.twoWay(
            (fun (_, (_, (fd: RoseTree<FieldData>))) -> fd.Data.IsGml),
            (fun v _ -> v |> GmlSetChecked |> LeafMsg)
        )
        "IsCml" |> Binding.twoWay(
            (fun (_, (_, (fd: RoseTree<FieldData>))) -> fd.Data.IsCml),
            (fun v _ -> v |> CmlSetChecked |> LeafMsg)
        )
        "IsCmlChangeField" |> Binding.twoWay(
            (fun (_, (_, (fd: RoseTree<FieldData>))) -> fd.Data.IsCmlChangeField),
            (fun v _ ->  v |> CmlChangeFieldSetChecked |> LeafMsg)
        )
        "IsCmlEntity" |> Binding.twoWay(
            (fun (_, (_, (fd: RoseTree<FieldData>))) -> fd.Data.IsCmlEntity),
            (fun v (_, (_, fd)) -> v |> CmlEntitySetChecked |> LeafMsg)
        )
        "IsEnabled" |> Binding.oneWay(fun (m, _) -> not m.IsEmpty)
        "IsExpanded" |> Binding.oneWay(fun _ -> level < 2)
        (* TODO: Special DX sub-MTs 
           1. Hide the GML checkbox.
           (because that would duplicate functionality on the Special DX tab).
           2. Add the sub-MT description to the display name.
        *)
        "GmlVisibility" |> Binding.oneWay(fun (_, (_, (fd: RoseTree<FieldData>))) -> checkboxVisibility fd.Data)
        "CmlChangeFieldVisibility" |> Binding.oneWay(fun (_, (_, (fd: RoseTree<FieldData>))) -> checkboxVisibility fd.Data)
        "CmlEntityVisibility" |> Binding.oneWay(fun (_, (_, fd)) -> checkboxVisibility fd.Data)
        "ChildFields" |> Binding.subModelSeq(
            (fun (_, (_, c)) -> c.Children |> Seq.map (fun gc -> (c, gc))),
            (fun ((m, _), gc) -> (m, gc)),
            (fun (_, (_, c)) -> c.Data.Id),
            (fun (id, msg) -> msg |> RoseTree.branchMsg id),
            fieldBindings (level + 1)
        )
    ]

    let rootBindings () : Binding<Model, Msg> list = [
        "IsEnabled" |> Binding.oneWay(fun m -> not m.IsEmpty)
        "MsgType" |> Binding.oneWay(fun m -> m.MsgType)
        "Fields" |> Binding.subModelSeq(
          (fun m -> m.DummyRoot.Children |> Seq.map (fun c -> (m.DummyRoot, c))),
          (fun (_, c) -> c.Data.Id),
          (fun (id, msg) -> msg |> RoseTree.branchMsg id |> SubtreeMsg),
          fieldBindings 0
        )
        "Save" |> Binding.cmd Save
        "Cancel" |> Binding.cmd Cancel
]

module PublicAPI =

    let loadWindow (msgTypeID: int) (msgTypeName: string) (parentStructName: string) (configFolder: string) =
        App.configFolderPath <- App.getConfigFolder configFolder
        let mutable timeMeasurementString = ""
        let watch = Stopwatch.StartNew()
        let measureElapsedTime label =
            let elapsed = watch.Elapsed // <-- explicit copy prevents level 5 warning FS0052.
            timeMeasurementString <- sprintf "%s | %s Elapsed Time: %.1f seconds" 
                timeMeasurementString label elapsed.TotalSeconds
            watch.Restart()

        let waitWindow = WaitWindow()
        App.mkNewWindow().Activated.Add (fun _ -> 
            measureElapsedTime "TreeView loading"
            waitWindow.Close()
            )
        let init = App.init msgTypeID msgTypeName parentStructName measureElapsedTime
        let showDialogWithConfig config (window: Window) program =
            waitWindow.Show()
            Program.startElmishLoop config window program
            window.ShowDialog ()

        Program.mkSimpleWpf init App.update App.rootBindings
        |> showDialogWithConfig ElmConfig.Default (App.window)
        |> ignore

        // Return diagnostic information in a string.
        sprintf "StructFilters Tree View (MT 0x%04X)%s" 
            msgTypeID timeMeasurementString
