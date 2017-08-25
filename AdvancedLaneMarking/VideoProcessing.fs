module VideoProcessing
open OpenCVCommon
open OpenCvSharp
open BaseTypes
open ImageProc
open LaneFind
open LineFitting
open Utils
open System.IO

let findLines (p:VParms)  (warped:Mat) =
    let sz = warped.Size()
    let a = peaks warped                                    //find peeks in lower part of image on X axis
    let leftCtr = argmaxX a.[0..sz.Width/2]
    let rightCtr = argmaxX a.[sz.Width/2..] + sz.Width/2
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
    let stL,lineLeft = fitLine p sz leftMids leftPoints
    let stR,lineRight = fitLine p sz rightMids rightPoints
    stL,stR,lineLeft,leftMids,leftPoints,lineRight,rightMids,rightPoints
    
let processFrame (p:VParms) (m:Mat) (minv:Mat) (inp:Mat) (outp:Mat) = 
    let sz = inp.Size()
    use warped = new Mat()
    transformFrame p m inp warped                     //threshold and warp frame 
    //win "warped" warped
    let stL,stR,lineLeft,leftMids,leftPoints,lineRight,rightMids,rightPoints = findLines p warped      //find lines
    let lpx = linePoints (warped.Width-1) lineLeft   //generate points from equations
    let rpx = linePoints (warped.Width-1) lineRight
    let isLeft = stL + stR >= 0
    let lpxT = trimLine sz lineLeft (stL > 0) lpx
    let rpxT = trimLine sz lineRight (stR > 0) rpx
    printfn "stL:%d, stR%d" stL stR
    let lpxS = lpxT |> Seq.sortBy (fun x -> x.Y)
    let rpxS = rpxT |> Seq.sortByDescending (fun x->x.Y)
    let pts = Seq.append lpxS rpxS

    //let pts = Seq.append lpxT rpxT        //join line points to form 1 polygon
    use o2 = inp.EmptyClone()
    plotPoints o2 lpxT
    plotPoints o2 rpxT
    //Cv2.FillConvexPoly((o2:Mat),pts, Scalar(50.,215.,50.)) //draw polygon
    Cv2.FillPoly((o2:Mat),[pts], Scalar(50.,215.,50.)) //draw polygon
    //win "o2" o2    
    Cv2.WarpPerspective(!>o2,!>o2,!>minv,o2.Size())    //unwrap
    //Cv2.CvtColor(!>warped, !> warped, ColorConversionCodes.GRAY2BGR )
    //Cv2.AddWeighted(!>warped,1.,!>o2,0.6,0.,!>outp)       //overlay on input
    Cv2.AddWeighted(!>inp,1.,!>o2,0.6,0.,!>outp)       //overlay on input


let findLanes (p:VParms) videoFile =
    let maskDir = Path.GetDirectoryName(videoFile) + "/masked"
    if Directory.Exists maskDir |> not then Directory.CreateDirectory maskDir |> ignore
    let outTemplate = maskDir + "/o.jpg"
    let m = getTransform()
    let minv = m.Inv()
    let ofn = append2Name videoFile "_lanes"
    printfn "processing: %s" ofn
    use clipIn = new VideoCapture(videoFile)
    let (w,h) = clipIn.FrameWidth,clipIn.FrameHeight
    use clipOut = new VideoWriter()
    clipOut.Open(ofn,FourCC.DIVX,clipIn.Fps,Size(w,h))
    if not(clipOut.IsOpened()) then failwith "file not opened"
    let r = ref 0
    while clipIn.Grab() do ///&& !r < 200 do
        let inp = clipIn.RetrieveMat()
        let outp = new Mat()
        printfn "%d" !r
        try 
            processFrame p m minv inp outp
            outp.SaveImage(append2Name outTemplate (string !r)) |> ignore
        with ex -> 
            printfn "%s" ex.Message
            printfn "frame miss %d"  !r
        r := !r + 1
        clipOut.Write(outp)
        inp.Release()
        outp.Release()

let topDown h xs = xs |> Seq.map(fun x -> float h - x)
