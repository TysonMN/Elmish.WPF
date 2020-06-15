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
        DummyRoot = { Data = FieldData.empty; Fields = [] }
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
        JsonConvert.DeserializeObject<Field>(str)

    let loadSavedChanges dummyRoot = 
        let updateFromFile field =
            let updateFields fileFld inMemoryFld =
                inMemoryFld.Fields 
                |> List.map (fun f ->
                    fileFld.Fields
                    |> List.tryFind (fun fld -> fld.Data.Name = f.Data.Name)
                    |> Option.map (fun fld -> { f with Data = fld.Data })
                    |> Option.defaultValue f
                )
            let file = getConfigFile field.Data.Type
            if (not field.Fields.IsEmpty) && File.Exists file then
                let ff = deserializeStruct file
                { field with (*Data = ff.Data;*) Fields = updateFields ff field }
            else field

        let rec traverse fld =
            let fields' =
                fld.Fields
                |> List.map (fun f -> 
                    updateFromFile f
                    |> traverse
                )
            { fld with Fields = fields' }
        traverse dummyRoot

    let init (msgTypeID: int) (msgTypeName: string) (parentStructName: string) measureElapsedTime () =
        // TODO: Look up the headerFileName path based on the DLL version loaded.
        // TODO: Get the struct definitions without distributing the v*struct.h files.
        let exeFolderPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        let headerFileName = Path.Combine(exeFolderPath, """Data\structs.h""")
        let parentStruct = Translation.translate headerFileName parentStructName
        let dummyRoot =
            if parentStruct.IsEmpty then
                // Add one child to prevent a StackOverflow in Elmish.WPF.ViewModel.initializeBinding function.
                let dummyChild = FieldData.create "Parent struct not found" ""
                [ dummyChild |> Tree.asLeaf ] |> Tree.asDummyRoot
            else
                [ parentStruct ] |> Tree.asDummyRoot
        measureElapsedTime "App.init"
        unmodifiedModel <- 
            { MsgType = { ID = msgTypeID; Name = msgTypeName }
              DummyRoot = loadSavedChanges dummyRoot
            }
        unmodifiedModel

    [<Struct>]
    type Msg =
        | GmlSetChecked of gmlId: FieldId * gmlChecked: bool
        | CmlSetChecked of cmlId: FieldId * cmlChecked: bool
        | CmlChangeFieldSetChecked of changeId: FieldId * changeChecked: bool
        | CmlEntitySetChecked of entityId: FieldId * entityChecked: bool
        | Save
        | Cancel

  /// Updates the field using the specified function if the ID matches,
  /// otherwise passes the field through unmodified.
    let updateField f id fld =
        if fld.Id = id then f fld else fld

    let setIsGml isChecked = updateField <| FieldData.setIsGml isChecked
    let setIsCml isChecked = updateField <| FieldData.setIsCml isChecked
    let setIsCmlChangeField isChecked = updateField <| FieldData.setIsCmlChangeField isChecked
    let setIsCmlEntity isChecked = updateField <| FieldData.setIsCmlEntity isChecked

    let getChangedStructs m =
        let isNotEqual fldA fldB = fldA.Data <> fldB.Data
        // Depth First Search
        let rec diff acc oldFld newFld =
            match oldFld.Fields with
            | [] -> acc
            | fields -> 
                let acc' =
                    if (fields, newFld.Fields) ||> List.exists2 isNotEqual then
                        Tree.fieldwithFirstLevelFields newFld :: acc
                    else
                        acc

                (acc', fields, newFld.Fields)
                |||> List.fold2 (fun accu oldChild newChild -> diff accu oldChild newChild)
        
        diff [] unmodifiedModel.DummyRoot.Fields.Head m.DummyRoot.Fields.Head

    let serializeStructs flds =
        let ser = JsonSerializer()
        ser.Formatting <- Newtonsoft.Json.Formatting.Indented
        flds
        |> List.iter (fun f ->
            let file = sprintf "%s.json" f.Data.Type
            let filePath = Path.Combine(configFolderPath, file)
            use writer = File.CreateText(filePath)
            ser.Serialize(writer, f)
        )

    let update msg m =
        match msg with
        | GmlSetChecked (id, isChecked) ->
            Tree.nodesCount <- 1 // just for testing how many nodes are in the tree.
            { m with DummyRoot = m.DummyRoot |> Tree.mapData (setIsGml isChecked id) }
        | CmlSetChecked (id, isChecked) ->
            { m with DummyRoot = m.DummyRoot |> Tree.mapData (setIsCml isChecked id) }
        | CmlChangeFieldSetChecked (id, isChecked) ->
            { m with DummyRoot = m.DummyRoot |> Tree.mapData (setIsCmlChangeField isChecked id) }
        | CmlEntitySetChecked (id, isChecked) ->
            { m with DummyRoot = m.DummyRoot |> Tree.mapData (setIsCmlEntity isChecked id) }
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

    let rec fieldBindings level () : Binding<Model * FieldData, Msg> list = [
        // TODO: Hide ignored fields: spares and pads.
        "Name" |> Binding.oneWay(fun (_, fd) -> fd.Name)
        "Type" |> Binding.oneWay(fun (_, fd) -> fd.Type)
        "IsGml" |> Binding.twoWay(
            (fun (_, fd) -> fd.IsGml),
            (fun v (_, fd) -> GmlSetChecked (fd.Id, v))
        )
        "IsCml" |> Binding.twoWay(
            (fun (_, fd) -> fd.IsCml),
            (fun v (_, fd) -> CmlSetChecked (fd.Id, v))
        )
        "IsCmlChangeField" |> Binding.twoWay(
            (fun (_, fd) -> fd.IsCmlChangeField),
            (fun v (_, fd) -> CmlChangeFieldSetChecked (fd.Id, v))
        )
        "IsCmlEntity" |> Binding.twoWay(
            (fun (_, fd) -> fd.IsCmlEntity),
            (fun v (_, fd) -> CmlEntitySetChecked (fd.Id, v))
        )
        "IsEnabled" |> Binding.oneWay(fun (m, _) -> not m.IsEmpty)
        "IsExpanded" |> Binding.oneWay(fun (_, _) -> level < 2)
        (* TODO: Special DX sub-MTs 
           1. Hide the GML checkbox.
           (because that would duplicate functionality on the Special DX tab).
           2. Add the sub-MT description to the display name.
        *)
        "GmlVisibility" |> Binding.oneWay(fun (_, fd) -> checkboxVisibility fd)
        "CmlChangeFieldVisibility" |> Binding.oneWay(fun (_, fd) -> checkboxVisibility fd)
        "CmlEntityVisibility" |> Binding.oneWay(fun (_, fd) -> checkboxVisibility fd)
        "ChildFields" |> Binding.subModelSeq(
            (fun (m, fd) -> m |> Tree.childrenFieldsOf fd.Id),
            (fun ((m, _), childField) -> (m, childField)),
            (fun (_, fd) -> fd.Id),
            snd,
            fieldBindings (level + 1)
        )
    ]

    let rootBindings () : Binding<Model, Msg> list = [
        "IsEnabled" |> Binding.oneWay(fun m -> not m.IsEmpty)
        "MsgType" |> Binding.oneWay(fun m -> m.MsgType)
        "Fields" |> Binding.subModelSeq(
          (fun m -> m |> Tree.topLevelFields),
          (fun (fd:FieldData) -> fd.Id), 
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
