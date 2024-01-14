open Microsoft.FSharp.NativeInterop
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.IO.MemoryMappedFiles


[<Struct>]
type Summary =
    {
        Min: float
        Max: float
        Sum: float
        Count: int
    }

// baseline: 2m43.200s

[<Literal>]
let path = "/prosiectau/Prosiectau/1brc/1brc/measurements.txt"
let encoding = System.Text.Encoding.UTF8

let stopwatch = Stopwatch.StartNew()

let dict = Dictionary<string,Summary>()

let mmf = MemoryMappedFile.CreateFromFile(path, FileMode.Open)
let accessor = mmf.CreateViewAccessor()

#nowarn "9"
let mutable filePtr: nativeptr<byte> = NativePtr.nullPtr<byte>
accessor.SafeMemoryMappedViewHandle.AcquirePointer(&filePtr)

let length = accessor.Capacity

let indexOf (b: byte) (start: int64) (finish: int64) (accessor: MemoryMappedViewAccessor) =
    let mutable i = start
    let mutable running = true
    let mutable idx = -1L
    while running do
        let x = accessor.ReadByte(i)
        if x = b then
            running <- false
            idx <- i
        elif i = finish then
            running <- false
        else
            i <- i + 1L
    idx

let mutable stationLength = 0
let mutable temperatureLength = 0
//let mutable count = 0L
let mutable lineStart = 0L
let mutable c = 0uy
let mutable i = 0L
while i < length do
    // count <- count + 1L
    // if count % 10_000_000L = 0L then
    //     printfn $"Count: %d{count}"
    // find the ; and get the station name
    let stationStart = filePtr
    stationLength <- 0
    c <- 0uy
    while c <> ';'B do        
        stationLength <- stationLength + 1
        filePtr <- NativePtr.add filePtr 1
        i <- i + 1L
        c <- NativePtr.read filePtr
    let stationName = encoding.GetString(stationStart, stationLength)
    filePtr <- NativePtr.add filePtr 1
    i <- i + 1L
    
    // Read until newline to get temperature
    let temperatureStart = filePtr
    temperatureLength <- 0
    c <- 0uy
    while c <> '\n'B do
        temperatureLength <- temperatureLength + 1
        filePtr <- NativePtr.add filePtr 1
        i <- i + 1L
        c <- NativePtr.read filePtr
    let temperatureStr = encoding.GetString(temperatureStart, temperatureLength)
    let temperature = float temperatureStr
    filePtr <- NativePtr.add filePtr 1
    i <- i + 1L
    let summary =
        match dict.TryGetValue stationName with
        | true, value ->
            { Min = min value.Min temperature ; Max = max value.Max temperature ; Count = value.Count + 1 ; Sum = value.Sum + temperature }
        | false, _ ->
            { Min = temperature ; Max = temperature ; Sum = temperature ; Count = 1}
    dict[stationName] <- summary
accessor.SafeMemoryMappedViewHandle.ReleasePointer()

let results =
    dict
    |> Seq.sortBy _.Key
    |> Seq.map (fun kv ->
        let station = kv.Key
        let minVal = kv.Value.Min
        let maxVal = kv.Value.Max
        let meanVal = kv.Value.Sum / float kv.Value.Count
        $"%s{station}=%.1f{minVal}/%.1f{meanVal}/%.1f{maxVal}"
        )
    |> fun xs ->
        "{" + String.Join(", ", xs) + "}"

printfn $"%s{results}"        

stopwatch.Stop()
printfn $"Elapsed: %d{stopwatch.ElapsedMilliseconds}ms / %s{stopwatch.Elapsed.ToString()}"
