module Program

open System.Collections.ObjectModel

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Elmish.WPF


let ignoreLog _ _ _ = ()
let createAsId a _ = a
let ignoreUpdate _ _ _ = ()


type NoChange () =
    let observableCollection = ObservableCollection ()
    let mutable array = [||]

    [<Params (10, 20, 50, 100, 200, 500, 1000)>]
    member val public Length = 0 with get, set

    [<GlobalSetup>]
    member this.GlobalSetup() =
      array <- [1..this.Length] |> List.toArray
      array |> Array.iter observableCollection.Add

    [<Benchmark>]
    member _.ElmStyleMerge () =
      elmStyleMerge
        id
        id
        createAsId
        ignoreUpdate
        observableCollection
        array

    [<Benchmark(Baseline = true)>]
    member _.HistoricalMerge () =
      historicalMerge
        ignoreLog
        ignoreLog
        id
        id
        createAsId
        ignoreUpdate
        observableCollection
        array


type ReverseWithoutDelay () =
    let observableCollection = ObservableCollection ()
    let mutable arrayA = [||]
    let mutable arrayB = [||]

    let swaparrays () =
      let temp = arrayA
      arrayA <- arrayB
      arrayB <- temp
      
    [<Params (10, 20, 50, 100, 200, 500, 1000)>]
    member val public Length = 0 with get, set

    [<GlobalSetup>]
    member this.GlobalSetup() =
      arrayA <- [1..this.Length] |> List.toArray
      arrayA |> Array.iter observableCollection.Add
      arrayB <- arrayA |> Array.rev

    [<Benchmark>]
    member _.ElmStyleMerge () =
      elmStyleMerge
        id
        id
        createAsId
        ignoreUpdate
        observableCollection
        arrayB
      swaparrays ()

    [<Benchmark(Baseline = true)>]
    member _.HistoricalMerge () =
      historicalMerge
        ignoreLog
        ignoreLog
        id
        id
        createAsId
        ignoreUpdate
        observableCollection
        arrayB
      swaparrays ()


type ReverseWithDelay () =
    let observableCollection = ObservableCollection ()
    let mutable arrayA = [||]
    let mutable arrayB = [||]

    let swaparrays () =
      let temp = arrayA
      arrayA <- arrayB
      arrayB <- temp

    let createAsIdWithDelay a _ =
      1 |> Async.Sleep |> Async.RunSynchronously
      a
      
    [<Params (10, 20, 50, 100, 200, 500, 1000)>]
    member val public Length = 0 with get, set

    [<GlobalSetup>]
    member this.GlobalSetup() =
      arrayA <- [1..this.Length] |> List.toArray
      arrayA |> Array.iter observableCollection.Add
      arrayB <- arrayA |> Array.rev

    [<Benchmark>]
    member _.ElmStyleMerge () =
      elmStyleMerge
        id
        id
        createAsIdWithDelay
        ignoreUpdate
        observableCollection
        arrayB
      swaparrays ()

    [<Benchmark(Baseline = true)>]
    member _.HistoricalMerge () =
      historicalMerge
        ignoreLog
        ignoreLog
        id
        id
        createAsIdWithDelay
        ignoreUpdate
        observableCollection
        arrayB
      swaparrays ()


[<EntryPoint>]
let Main _ =
    let switcher =
      [| typeof<NoChange>
         typeof<ReverseWithDelay>
         typeof<ReverseWithoutDelay> |]
      |> BenchmarkSwitcher
    switcher.Run() |> ignore
    0
