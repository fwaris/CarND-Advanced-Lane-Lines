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
    let lineLeft = fitLine  leftPoints
    let lineRight = fitLine   rightPoints
    lineLeft,leftMids,leftPoints,lineRight,rightMids,rightPoints

let updateLine (p:VParms) (QuadTrns(c,b,a) as line) cs =
    match cs with
    | [] -> line,[line]
    | xs  -> 
        let c' = xs |> List.averageBy (fun (QuadTrns(c,_,_)) -> c)
        let b' = xs |> List.averageBy (fun (QuadTrns(_,b,_)) -> b)
        let a' = xs |> List.averageBy (fun (QuadTrns(_,_,a)) -> a)
        let line' = QuadTrns(c',b',a)
        line', line::cs |> List.truncate 3
                
let processFrame (p:VParms) (m:Mat) (minv:Mat) lH rH (inp:Mat) (outp:Mat) = 
    let sz = inp.Size()
    use warped = new Mat()
    transformFrame p m inp warped                     //threshold and warp frame 
    //win "warped" warped
    let lineLeft,leftMids,leftPoints,lineRight,rightMids,rightPoints = findLines p warped      //find lines
    let lpx = linePoints (warped.Width-1) lineLeft   //generate points from equations
    let rpx = linePoints (warped.Width-1) lineRight
    let lineLeft,lH = updateLine p lineLeft lH
    let lineRigth,rH = updateLine p lineRight rH
    let lpxS = lpx |> Seq.sortBy (fun x -> x.Y)
    let rpxS = rpx |> Seq.sortByDescending (fun x->x.Y)
    let pts = Seq.append lpxS rpxS
    let cLeft = lineLeft  |> toWorldLine 720 |> curvature 720
    let cRight = lineRight |> toWorldLine 720 |> curvature 720
    let crv = (cLeft + cRight) / 2.0

    //let pts = Seq.append lpxT rpxT        //join line points to form 1 polygon
    use o2 = inp.EmptyClone()
    plotPoints o2 lpxS
    plotPoints o2 rpxS
    //Cv2.FillConvexPoly((o2:Mat),pts, Scalar(50.,215.,50.)) //draw polygon
    Cv2.FillPoly((o2:Mat),[pts], Scalar(50.,215.,50.)) //draw polygon
    //win "o2" o2    
    Cv2.WarpPerspective(!>o2,!>o2,!>minv,o2.Size())    //unwrap
    Cv2.CvtColor(!>warped, !> warped, ColorConversionCodes.GRAY2BGR )
    //Cv2.AddWeighted(!>warped,1.,!>o2,0.6,0.,!>outp)       //overlay on input
    Cv2.AddWeighted(!>inp,1.,!>o2,0.6,0.,!>outp)       //overlay on input
    Cv2.PutText(!>outp,(sprintf "curve %0.0f (M)" crv), Point(200.,200.), HersheyFonts.HersheyPlain,3.,Scalar(1.,50.,255.))
    Cv2.PutText(!>outp,(sprintf "L %0.2f" cLeft), Point(200.,235.), HersheyFonts.HersheyPlain,3.,Scalar(1.,50.,255.))
    Cv2.PutText(!>outp,(sprintf "R %0.2f" cRight), Point(200.,275.), HersheyFonts.HersheyPlain,3.,Scalar(1.,50.,255.))
    lH,rH


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
    let mutable leftLineHist = []
    let mutable rightLineHist = []
    while clipIn.Grab() do ///&& !r < 200 do
        let inp = clipIn.RetrieveMat()
        let outp = new Mat()
        printfn "%d" !r
        try 
            let lH,rH = processFrame p m minv leftLineHist rightLineHist inp outp
            leftLineHist <- lH; rightLineHist <- rH
            outp.SaveImage(append2Name outTemplate (string !r)) |> ignore
        with ex -> 
            printfn "%s" ex.Message
            printfn "frame miss %d"  !r
        r := !r + 1
        clipOut.Write(outp)
        inp.Release()
        outp.Release()

let topDown h xs = xs |> Seq.map(fun x -> float h - x)
