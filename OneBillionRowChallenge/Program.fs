open Microsoft.FSharp.NativeInterop
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.IO.MemoryMappedFiles
open FSharp.Collections.ParallelSeq

// Stop the warning for doing naughty unsafe things uwu
#nowarn "9"

[<Struct>]
type Summary =
    {
        Min: float
        Max: float
        Sum: float
        Count: int
    }

type Chunk =
    {
        Id: int
        StartIndex: int64
        EndIndex: int64
        StartPtr: nativeptr<byte>
        Length: int64
    }

// baseline: 2m43.200s

[<Literal>]
let path = "/prosiectau/Prosiectau/1brc/1brc/measurements.txt"
let encoding = System.Text.Encoding.UTF8

let stopwatch = Stopwatch.StartNew()


let getChunks startingPtr chunkSize fileSize =
    let mutable chunkStartPtr = startingPtr
    let chunks = ResizeArray<Chunk>()
    let mutable chunkStartIdx = 0L
    let mutable chunkEndIdx = 0L
    let mutable chunkId = 0
    while chunkStartIdx < fileSize do
        // find end of chunk by looking for newline
        chunkEndIdx <- min (chunkStartIdx + chunkSize) (fileSize-1L)
        let offset = int (chunkEndIdx - chunkStartIdx)
        let mutable p = NativePtr.add chunkStartPtr offset
        let mutable c : byte = NativePtr.read p
        while c <> '\n'B do            
            chunkEndIdx <- chunkEndIdx + 1L
            if chunkEndIdx < fileSize then
                p <- NativePtr.add p 1
                c <- NativePtr.read p
            else
                c <- '\n'B            
        chunks.Add({ Id = chunkId ; StartIndex = chunkStartIdx ; EndIndex = chunkEndIdx ;  StartPtr = chunkStartPtr ; Length = chunkEndIdx - chunkStartIdx })
        chunkStartIdx <- chunkEndIdx + 1L
        chunkStartPtr <- NativePtr.add p 1
        chunkId <- chunkId + 1
    chunks

let processChunk (chunk: Chunk) =
    let dict = Dictionary<string,Summary>()
    let mutable i = 0L
    let mutable filePtr = chunk.StartPtr

    while i < chunk.Length do
        // Read station name
        let mutable stationStart = filePtr
        let mutable stationLength = 0
        let mutable c = 0uy
        while c <> ';'B do        
            stationLength <- stationLength + 1
            filePtr <- NativePtr.add filePtr 1
            i <- i + 1L
            c <- NativePtr.read filePtr
        let stationName = encoding.GetString(stationStart, stationLength)
        filePtr <- NativePtr.add filePtr 1
        i <- i + 1L
        
        // Read temperature
        let mutable isNeg = false
        let mutable temperature = 0.0
        c <- NativePtr.read filePtr
        while i < chunk.Length && ((c >= '0'B && c <= '9'B) || c = '-'B) do
            if c = '-'B then
                isNeg <- true
            else
                temperature <- (temperature * 10.0) + float ((int c) - 48)    
            filePtr <- NativePtr.add filePtr 1
            i <- i + 1L
            c <- NativePtr.read filePtr
        if i < chunk.Length && c = '.'B then
            i <- i + 1L
            filePtr <- NativePtr.add filePtr 1
            c <- NativePtr.read filePtr
        let mutable decPlaces = 1.0
        while i < chunk.Length && c >= '0'B && c <= '9'B do
            temperature <- temperature + ((10.0 ** (-decPlaces)) * float (c - '0'B))
            decPlaces <- decPlaces + 1.0
            i <- i + 1L
            filePtr <- NativePtr.add filePtr 1
            c <- NativePtr.read filePtr       
        if isNeg then
            temperature <- -temperature
        
        // Skip newline
        if i < chunk.Length then
            filePtr <- NativePtr.add filePtr 1
            i <- i + 1L

        let summary =
            match dict.TryGetValue stationName with
            | true, value ->
                { Min = min value.Min temperature ; Max = max value.Max temperature ; Count = value.Count + 1 ; Sum = value.Sum + temperature }
            | false, _ ->
                { Min = temperature ; Max = temperature ; Sum = temperature ; Count = 1}
        dict[stationName] <- summary
    dict
    
let mergeStations (summaries: Dictionary<string,Summary> seq) =
    summaries
    |> Seq.fold (fun (merged: Dictionary<string,Summary>) dict ->
        for kv in dict do
            let summary =
                if merged.ContainsKey(kv.Key) then
                    {
                        Min = min merged[kv.Key].Min kv.Value.Min
                        Max = max merged[kv.Key].Max kv.Value.Max
                        Sum = merged[kv.Key].Sum + kv.Value.Sum
                        Count = merged[kv.Key].Count + kv.Value.Count 
                    }
                else
                    {
                        Min = kv.Value.Min
                        Max = kv.Value.Max
                        Sum = kv.Value.Sum
                        Count = kv.Value.Count 
                    }
            merged[kv.Key] <- summary
        merged
        ) (Dictionary<string,Summary>())


let mmf = MemoryMappedFile.CreateFromFile(path, FileMode.Open)
let accessor = mmf.CreateViewAccessor()
let length = accessor.Capacity

let mutable filePtr: nativeptr<byte> = NativePtr.nullPtr<byte>
accessor.SafeMemoryMappedViewHandle.AcquirePointer(&filePtr)

try
    let processorCount = Environment.ProcessorCount
    let chunkSize = length / (int64 (processorCount/2))
    printfn $"processorCount=%d{processorCount}"
    printfn $"chunkSize=%d{chunkSize}"
    printfn $"fileSize=%d{length}"
    let chunks = getChunks filePtr chunkSize length
    printfn $"Number of chunks: %d{chunks.Count}"
    // let processedChunks =
    //     chunks
    //     |> PSeq.mapi (fun i  chunk ->
    //         printfn $"Processing chunk %d{i}"
    //         processChunk chunk)
    //     |> PSeq.toList
    let chunkResults = Array.zeroCreate<Dictionary<string,Summary> option> chunks.Count
    let threads =
        chunks
        |> Seq.mapi (fun i chunk ->
            new Threading.Thread(fun () ->
                printfn $"Processing chunk: {chunk.Id}"
                chunkResults[i] <- Some (processChunk chunk))
            )
        |> Seq.toArray
    for thread in threads do
        thread.Start()
        
    for thread in threads do
        thread.Join()

    let processedChunks =
        chunkResults
        |> Array.choose id

    printfn "Merging..."        
    let dict = mergeStations processedChunks

    printfn "Outputting..."
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
        |> Seq.toList
        |> fun xs ->
            "{" + String.Join(", ", xs) + "}"

    printfn $"%s{results}"        

finally
    accessor.SafeMemoryMappedViewHandle.ReleasePointer()

stopwatch.Stop()
printfn $"Elapsed: %d{stopwatch.ElapsedMilliseconds}ms / %s{stopwatch.Elapsed.ToString()}"
