open System
open System.Collections.Generic
open System.Diagnostics
open System.IO

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
let reader = new StreamReader(path = path, encoding = encoding, detectEncodingFromByteOrderMarks = false, bufferSize = 64 * 1024 ) //4096)

let stopwatch = Stopwatch.StartNew()
let mutable count = 0
let dict = Dictionary<string,Summary>()
let mutable running = true
while running do
    let line = reader.ReadLine()
    if line = null then
        running <- false
    else
        count <- count + 1
        let idx = line.IndexOf(';')
        let station = line.Substring(0, idx-1)
        let temperatureStr = line.Substring(idx+1)
        let temperature = float temperatureStr          
        let summary =
            match dict.TryGetValue station with
            | true, value ->
                { Min = min value.Min temperature ; Max = max value.Max temperature ; Count = value.Count + 1 ; Sum = value.Sum + temperature }
            | false, _ ->
                { Min = temperature ; Max = temperature ; Sum = temperature ; Count = 1}
        dict[station] <- summary
        ()

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
