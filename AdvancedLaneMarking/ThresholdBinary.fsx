//script file for REPL evaluation of processing code
#load "SetEnv.fsx"
open OpenCVCommon
open OpenCvSharp
open CalibrateCamera
open FSharp.Charting
open System
open System.IO
open SetEnv 
open BaseTypes
open ImageProc
open Utils
open LaneFind
open LineFitting
open VideoProcessing

let v_fldr = @"D:\repodata\adv_lane_find"
let v_chlng2 = Path.Combine(v_fldr,"challenge_video.mp4")
let v_chlng1 = Path.Combine(v_fldr,"harder_challenge_video.mp4")
let v_prjct  = Path.Combine(v_fldr,"project_video.mp4")

let testProcessing() =
    let p = VParms.Default
    findLanes p v_prjct
    findLanes p v_chlng1
    findLanes p v_chlng2

let topDown h xs = xs |> Seq.map(fun x -> float h - x)

let imageDumps() =
    dumpImageProjectAll v_prjct 

let i_path = @"D:\repodata\adv_lane_find\imgs\img2.jpg"

let expFrame() = 
    let p = VParms.Default
    let m = getTransform()
    let minv = m.Inv()
    let i = Cv2.ImRead(i_path)
    let out = new Mat()
    let cH = processFrame p m minv [] i out
    win i_path out
    ()

let expFrameV() = 
    let p = VParms.Default
    let m = getTransform()
    let minv = m.Inv()
    use clipIn = new VideoCapture(v_prjct)
    let r = ref 0
    while !r <= 2  &&  clipIn.Grab()do  r := !r + 1; printfn "r=%d" !r
    let i = clipIn.RetrieveMat()
    let out = new Mat()
    let cH = processFrame p m minv [] i out
    win "outv" out

