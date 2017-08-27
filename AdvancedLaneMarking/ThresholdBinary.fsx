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
    dumpImageProject v_prjct [0..625]

let expFrame() = 
    let p = VParms.Default
    let m = getTransform()
    let minv = m.Inv()
    let i = Cv2.ImRead( @"D:\repodata\adv_lane_find\imgs\img499.jpg")
    let out = new Mat()
    let lH,rH = processFrame p m minv [] [] i out
    win "out" out
    ()

let experimentalLines() = 
    let p = VParms.Default
    let m = getTransform()
    let minv = m.Inv()
    let i = Cv2.ImRead( @"D:\repodata\adv_lane_find\imgs\img498.jpg")
    let out = new Mat()
    let lH,rH = processFrame p m minv [] [] i out
    let warped = new Mat()
    transformFrame p m i warped
    //win "warped" warped
    let sz = warped.Size()
    let a = peaks warped                                    //find peeks in lower part of image on X axis
    Chart.Column a
    let leftCtr = argmaxX p a.[0..sz.Width/2]
    let rightCtr = argmaxX p a.[sz.Width/2..] + sz.Width/2
    let winHeight = warped.Height/p.nWindows
    let baseLeftRect  = makeRect leftCtr sz p.winMargin sz.Height winHeight
    let baseRigthRect = makeRect rightCtr sz p.winMargin sz.Height winHeight
    let startNzPxL,startLeftRect = recenterWindow p warped baseLeftRect
    let startNzPxR,startRightRect = recenterWindow p warped baseRigthRect
    let startState = 
            (
                startLeftRect,                                           //bottom rectangles
                startRightRect,
                ResizeArray[startNzPxL,midpBot startLeftRect,startLeftRect.Y],    //nonzero count, coordinates of bottom rects
                ResizeArray[startNzPxR,midpBot startRightRect,startRightRect.Y]   
            )
    let _,_,leftPtsR,righPtsR =
        (startState,[1..p.nWindows-1]) 
        ||> Seq.fold(fun (wL,wR,accL,accR) i ->
            let wL' = moveUp wL winHeight             //move rectangles up 1 level
            let wR' = moveUp wR winHeight          
            //printfn "Up wL'=%A,wR'%A" wL' wR'
            let nzPxL,wL' = recenterWindow p warped wL'       //recenter rectangles on mean of pixels
            //printfn "RC wL'2=%A" wL'
            let nzPxR,wR' = recenterWindow p warped wR'
            //printfn "RC wR'2=%A" wR'
            Cv2.Rectangle(warped,wL', Scalar(255.,0.,0.))   //draw rectanges
            Cv2.Rectangle(warped,wR', Scalar(255.,0.,0.))
            accL.Add(nzPxL,midpBot wL',wL'.Y)
            accR.Add(nzPxR,midpBot wR',wR'.Y)
            (   
                wL',
                wR',
                accL, 
                accR
            ))
    let leftPoints = leftPtsR  |> Seq.collect(fun (p,_,_)->p)
    let rightPoints = righPtsR |> Seq.collect(fun (p,_,_)->p) 
    let leftMids = leftPtsR |> Seq.map (fun (_,m,_) -> m)
    let rightMids = righPtsR |> Seq.map (fun (_,m,_) -> m)
    let lineLeft = fitLine leftPoints
    let lineRight = fitLine rightPoints
    let lpx = linePoints (warped.Width-1) lineLeft   //generate points from equations
    let rpx = linePoints (warped.Width-1) lineRight

    //i,warped,minv,stL,stR,lineLeft,leftMids,leftPoints,lineRight,rightMids,rightPoints
    i,warped,minv,leftPoints,rightPoints,leftMids,rightMids,lpx,rpx,lineLeft,lineRight

//experiment with image processing (not used)
let experimental() =
    let i,warped,minv,leftPoints,rightPoints,leftMids
        ,rightMids,lpx,rpx,
        lineLeft,lineRight = experimentalLines()

    leftPoints |> Seq.map(fun p->p.X,p.Y)|> Chart.FastPoint |> Chart.WithTitle "Left Raw"
    rightPoints |> Seq.map(fun p->p.X,p.Y)|> Chart.FastPoint |> Chart.WithTitle "Right Raw"
    Chart.Point(leftMids |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Left mids"
    Chart.Point(rightMids |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Right mids"
    Chart.Point (lpx |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Left"
    Chart.Point (rpx |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Right"
    [
        leftPoints |> Seq.map(fun p->p.X,p.Y)|> Chart.FastPoint |> Chart.WithTitle "Left Raw"
        Chart.Point (lpx |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Left"
    ] |> Chart.Combine
    [
        rightPoints |> Seq.map(fun p->p.X,p.Y)|> Chart.FastPoint |> Chart.WithTitle "Right Raw"
        Chart.Point (rpx |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Right"
    ] |> Chart.Combine
    
    let lpxS = lpx |> Seq.sortBy (fun x -> x.Y)
    let rpxS = rpx |> Seq.sortByDescending (fun x->x.Y)
    ////let pts = Seq.append  lpxT rpxT
    let pts = Seq.append lpxS rpxS
    let o2 = i.EmptyClone()
    plotPoints o2 lpxS
    //plotPoints o2 lpx^\
    //lpxT|>Seq.iter (printfn "%A")
    plotPoints o2 rpxS
    //win "o2" o2
    //win "warped" warped
    let cLeft = lineLeft  |> toWorldLine 720 |> curvature 720
    let cRight = lineRight |> toWorldLine 720 |> curvature 720
    let crv = (cLeft + cRight) / 2000.0
    Cv2.FillPoly((o2:Mat),[pts], Scalar(50.,215.,50.)) 
    //***unwraped

    let o2c = new Mat()
    let ow = new Mat()
    Cv2.CvtColor(!>warped, !> ow, ColorConversionCodes.GRAY2BGR)
    Cv2.AddWeighted(!>ow,1.,!>o2,0.6,0.,!>o2c)

    //****img

    //Cv2.WarpPerspective(!>o2,!>o2,!>minv,o2.Size())
    //let o2c = new Mat()
    //let ow = new Mat()
    //Cv2.CvtColor(!>warped, !> ow, ColorConversionCodes.GRAY2BGR)
    //Cv2.AddWeighted(!>i,1.,!>o2,0.6,0.,!>o2c)

    //***
    Cv2.PutText(!>o2c,(sprintf "curve %0.2f" crv), Point(10.,200.), HersheyFonts.HersheyPlain,2.,Scalar(1.,1.,1.))
    win "w" o2c


experimental()
