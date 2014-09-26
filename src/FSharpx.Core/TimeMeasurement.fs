module FSharpx.TimeMeasurement

let inline private collectGarbage() =
    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()
    System.GC.Collect()

/// Stops the runtime for a given function
let stopTime f = 
    collectGarbage()
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()
    let result = f()
    collectGarbage()
    sw.Stop()
    result,float sw.ElapsedMilliseconds

/// Stops the average runtime for a given function and applies it the given count
let stopAverageTime count f = 
    collectGarbage()
    let sw = new System.Diagnostics.Stopwatch()
    let list = [1..count]
    sw.Start()
    let results = List.map (fun _ -> f()) list
    collectGarbage()
    sw.Stop()
    results,float sw.ElapsedMilliseconds / float count

/// Stops the average runtime for a given function and applies it the given count
/// Afterwards it reports it with the given description
let stopAndReportAvarageTime count desc f =
    let results,time = stopAverageTime count f
    printfn "%s %Ams" desc time
    results,time

/// Stops the average runtime for the given functions
/// Afterwards it reports it with the given descriptions
let compareTwoRuntimes count desc1 f1 desc2 f2 =
    let _,time1 = stopAndReportAvarageTime count desc1 f1
    let _,time2 = stopAndReportAvarageTime count desc2 f2

    printfn "  Ratio:  %A" (time1 / time2)