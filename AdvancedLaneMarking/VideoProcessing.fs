module VideoProcessing
open OpenCVCommon
open OpenCvSharp
open BaseTypes
open ImageProc
open LaneFind
open LineFitting
open Utils
open System.IO
open System

//find the lines in a frame which is already thresholded and warped
let findLines (p:VParms)  (warped:Mat) =
    let sz = warped.Size()
    let a = peaks warped                                    //find peeks in lower part of image on X axis
    let leftEdgePos = argmaxX p a.[0..sz.Width/2]
    let rightEdgePos = argmaxX p a.[sz.Width/2..] + sz.Width/2
    let winHeight = warped.Height/p.nWindows
    let baseLeftRect  = makeRect leftEdgePos sz p.winMargin sz.Height winHeight
    let baseRigthRect = makeRect rightEdgePos sz p.winMargin sz.Height winHeight
    let startNzPxL,startLeftRect = recenterWindow p warped baseLeftRect
    let startNzPxR,startRightRect = recenterWindow p warped baseRigthRect
    let startState = 
            (
                startLeftRect,                                           //bottom rectangles
                startRightRect,
                ResizeArray[startNzPxL,midpBot startLeftRect,startLeftRect.Y],    //nonzero count, coordinates of bottom rects
                ResizeArray[startNzPxR,midpBot startRightRect,startRightRect.Y]   
            )
    Cv2.Rectangle(warped,startLeftRect, Scalar(255.,0.,0.))   //draw base rectanges
    Cv2.Rectangle(warped,startRightRect, Scalar(255.,0.,0.))
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
    //let leftMids = leftPtsR |> Seq.map (fun (_,m,_) -> m)
    //let rightMids = righPtsR |> Seq.map (fun (_,m,_) -> m)
    let lineLeft = fitLine  leftPoints
    let lineRight = fitLine   rightPoints
    lineLeft,leftEdgePos,lineRight,rightEdgePos

//smooth curvature using left right and a list of historic curvatures
let smoothCurvature (p:VParms) cLeft cRight cHist =
    let newCrvtr = 
        match cLeft, cRight with
        | l,r when Double.IsInfinity l && Double.IsInfinity r   -> infinity
        | l,r when Double.IsInfinity l                          -> r
        | l,r when Double.IsInfinity r                          -> l
        | l,r                                                   -> (l + r) / 2.0
    if newCrvtr |> Double.IsInfinity then
        newCrvtr,cHist
    else
        let cH = newCrvtr::cHist |> List.truncate p.curveSmoothN
        let crvtr = cH |> List.average
        crvtr,cH

//calcualte vehicle offset from center of the frame
let offsetFromCenter (sz:Size) leftPos rightPos =
    let frameCtr = sz.Width / 2
    let laneCtr  = (leftPos + rightPos) / 2
    let offSet = frameCtr - laneCtr 
    let offSetMeters = (float offSet) * Wm_per_pix
    offSetMeters //return offset in meters
 
//process video frame given in inp and put the results in outp
let processFrame (p:VParms) (m:Mat) (minv:Mat) cH (inp:Mat) (outp:Mat) = 
    let sz = inp.Size()
    use warped = new Mat()
    transformFrame p m inp warped                     //threshold and warp frame 
    //win "warped" warped
    let lineLeft,leftEdgePos,lineRight,rightEdgePos = findLines p warped      //find lines
    let lpx = linePoints (warped.Width-1) lineLeft   //generate points from equations
    let rpx = linePoints (warped.Width-1) lineRight
    let lpxS = lpx |> Seq.sortBy (fun x -> x.Y)                 //sort points for better
    let rpxS = rpx |> Seq.sortByDescending (fun x->x.Y)         //polyfill results
    let pts = Seq.append lpxS rpxS
    let crvtrLeft  = lineLeft  |> toWorldLine 720 |> curvatureAtBottom 
    let crvtrRight = lineRight |> toWorldLine 720 |> curvatureAtBottom 
    let crvtr,cH =  smoothCurvature p crvtrLeft crvtrRight cH
    let offset = offsetFromCenter sz leftEdgePos rightEdgePos
    use o2 = inp.EmptyClone()
    plotPoints o2 lpxS
    plotPoints o2 rpxS
    //Cv2.FillConvexPoly((o2:Mat),pts, Scalar(50.,215.,50.)) //draw polygon
    Cv2.FillPoly((o2:Mat),[pts], Scalar(50.,215.,50.)) //draw polygon
    //win "o2" o2    
    //Cv2.WarpPerspective(!>o2,!>o2,!>minv,o2.Size())    //unwrap
    //Cv2.AddWeighted(!>inp,1.,!>o2,0.6,0.,!>outp)       //overlay on input
    Cv2.CvtColor(!>warped, !> warped, ColorConversionCodes.GRAY2BGR )
    Cv2.AddWeighted(!>warped,1.,!>o2,0.6,0.,!>outp)       //overlay on input
    Cv2.PutText(!>outp,(sprintf "Curvature %0.0f (M)" crvtr), Point(400.,100.), HersheyFonts.HersheyPlain,3.,Scalar(50.,250.,255.))
    Cv2.PutText(!>outp,(sprintf "Offset %0.1f (M)" offset), Point(400.,140.), HersheyFonts.HersheyPlain,3.,Scalar(50.,250.,255.))
    //Cv2.PutText(!>outp,(sprintf "L %0.2f" crvtrLeft), Point(400.,135.), HersheyFonts.HersheyPlain,3.,Scalar(50.,250.,255.))
    //Cv2.PutText(!>outp,(sprintf "R %0.2f" crvtrRight), Point(400.,175.), HersheyFonts.HersheyPlain,3.,Scalar(50.,250.,255.))
    cH

//main method that processess an input video file
//and outputs new file with lanes marked
//the name of the new file is "<input file>_lanes"
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
    let mutable curvatureHist = []              //list to store curvature history for smoothing
    while clipIn.Grab() do ///&& !r < 200 do
        let inp = clipIn.RetrieveMat()
        let outp = new Mat()
        printfn "%d" !r
        try 
            let cH = processFrame p m minv curvatureHist inp outp
            curvatureHist <- cH
            outp.SaveImage(append2Name outTemplate (string !r)) |> ignore
        with ex -> 
            printfn "%s" ex.Message
            printfn "frame miss %d"  !r
        r := !r + 1
        clipOut.Write(outp)
        inp.Release()
        outp.Release()

