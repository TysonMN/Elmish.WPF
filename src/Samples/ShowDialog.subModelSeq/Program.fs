open System
open System.IO
open System.Reflection

(*
   In a real application, this project, instead of an exe, could be a library
   referenced by another application that may or may not be a WPF application.
   It could be an application written in any .NET language or even C++.
   The user could provide the arguments for the loadWindow function using
   a non-WPF GUI (e.g., Windows Forms, C++ MFC, etc.).
*)

[<EntryPoint; STAThread>]
let main argv =
    (* Typically, in a real application, the user's selections would be saved 
       in the user's AppData\Roaming folder or in a database somewhere,
       but for this sample we just store them with the exe, so we don't
       leave behind files in the user's AppData folder.
    *)
    let exeFolderPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let configFolder = Path.Combine(exeFolderPath, """Config""")
    (* Some parentStructName argument values for testing:
          cainism     6,749 nodes
          fawns       3,554 nodes
          marmota     41 nodes
    *)
    StructFilters.PublicAPI.loadWindow 0x0900 "Cainism_MT" "cainism" configFolder
    |> printfn "loadWindow result: %s"
    Console.ReadKey () |> ignore
    0 // return an integer exit code
