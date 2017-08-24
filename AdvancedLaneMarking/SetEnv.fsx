#I @"..\packages\OpenCvSharp3-AnyCPU.3.2.0.20170419\lib\net40"
#r @"..\packages\FSharp.Charting.0.90.14\lib\net40\FSharp.Charting.dll"
#r @"..\packages\Accord.3.6.0\lib\net45\Accord.dll"
#r @"..\packages\Accord.Math.3.6.0\lib\net45\Accord.Math.dll"
//#r @"..\packages\Accord.Math.3.6.0\lib\net45\Accord.Math.Core.dll"
#r @"..\packages\Accord.Statistics.3.6.0\lib\net45\Accord.Statistics.dll"
#r @"..\packages\Accord.MachineLearning.3.6.0\lib\net45\Accord.MachineLearning.dll"
#r @"..\packages\MathNet.Numerics.3.20.0\lib\net40\MathNet.Numerics.dll"
#r @"..\packages\ILNumerics.4.8.0.0\lib\ILNumerics.Core.dll"
#r @"..\packages\ILNumerics.4.8.0.0\lib\ILNumerics.Computing.dll"
#r @"..\packages\ILNumerics.4.8.0.0\lib\ILNumerics.Toolboxes.MachineLearning.dll"

#r @"OpenCvSharp.dll"
#r @"OpenCvSharp.Blob.dll"
#r @"OpenCvSharp.Extensions.dll" 
#r @"OpenCvSharp.UserInterface.dll"
#r "System.Windows.Forms.DataVisualization.dll"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"..\..\packages\OpenCvSharp3-AnyCPU.3.2.0.20170419\NativeDlls\x86"
#load "OpenCVCommon.fs"
#load "CalibrateCamera.fs"
open FSharp.Charting
fsi.AddPrinter(fun (ch:FSharp.Charting.ChartTypes.GenericChart) -> ch.ShowChart() |> ignore; "(Chart)")

let uiCtx = System.Threading.SynchronizationContext.Current
open OpenCvSharp
open CalibrateCamera
open System
open System.IO

//show image in an opencv window (background thread)
let win t i = 
    async{
        do! Async.SwitchToContext uiCtx
        new Window((t:string), WindowMode.AutoSize,i) |> ignore 
        } |> Async.Start


