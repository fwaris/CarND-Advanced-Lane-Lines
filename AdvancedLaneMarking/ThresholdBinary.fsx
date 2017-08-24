#load "SetEnv.fsx"
open OpenCVCommon
open OpenCvSharp
open CalibrateCamera
open FSharp.Charting
open System
open System.IO
open SetEnv 
open Microsoft.FSharp.NativeInterop
open Accord.MachineLearning 
open Accord.Statistics.Models.Regression
open Accord.Statistics.Models.Regression.Fitting
open ILNumerics
open ILNumerics.Toolboxes
#nowarn "9"
//s

open MathNet.Numerics
open Accord.Statistics.Models.Regression.Linear

let f = @"D:\Repos\CarND-Advanced-Lane-Lines\challenge_video.mp4"
let f2 = @"D:\Repos\CarND-Advanced-Lane-Lines\harder_challenge_video.mp4"
let pf = @"D:\Repos\CarND-Advanced-Lane-Lines\project_video.mp4"

//constants
let nWindows = 9
let winMargin = 100
let minpx = 50
let LINE_ST_TH = 100 //px

let append2Name f n =
    let fldr = Path.GetDirectoryName(f)
    let fn = Path.GetFileNameWithoutExtension(f)
    let xt = Path.GetExtension(f)
    let fn = fn + n + xt
    Path.Combine(fldr,fn)

let getTransform() =
    let src = [691,450;1150,720;250,720;597,450]
    let dst = [1150,10;1150,720;250,720;250,10]
    let src = src |> List.map(fun (x,y) -> Point2f(float32 x, float32 y))
    let dst = dst |> List.map(fun (x,y) -> Point2f(float32 x ,float32 y))
    let m = Cv2.GetPerspectiveTransform(src,dst)
    m

let colorThreshold (img:Mat) (gray:Mat) =
    let ims = img.Split()
    let imB,imG,imR = ims.[0], ims.[1], ims.[2]
    use tB = new Mat()
    use tG = new Mat()
    use tR = new Mat()
    Cv2.Threshold(!> imB, !> tB, 170., 255., ThresholdTypes.Binary) |> ignore
    Cv2.Threshold(!> imG, !> tG, 170., 255., ThresholdTypes.Binary) |> ignore
    Cv2.Threshold(!> imR, !> tR, 170., 255., ThresholdTypes.Binary) |> ignore
    use o = new Mat()
    Cv2.BitwiseAnd(!>tB, !> tG, !> o)
    Cv2.BitwiseAnd(!>tR, !>o, !> gray)
    [imB;imG;imR]|>List.iter(fun x -> x.Release())

let gradientThreshold (img:Mat) (gray:Mat) =
    use mutable tmp =  new Mat()
    Cv2.CvtColor(!>img, !> tmp, ColorConversionCodes.BGR2GRAY )
    tmp <- tmp.Sobel( !> MatType.CV_64F,1,0)
    Cv2.ConvertScaleAbs(!>tmp, !> tmp)
    Cv2.InRange(!>tmp,Scalar(60.),!>100.,!>gray)

let thresholdFrame (inP:Mat) (gray:Mat) =
    use m1 = new Mat()
    use m2 = new Mat()
    colorThreshold inP m1
    gradientThreshold inP m2
    Cv2.BitwiseOr(!>m1,!>m2,!>gray)
    
let warpFrame (m:Mat) (inp:Mat) (gray:Mat) = Cv2.WarpPerspective(!>inp, !>gray, !>m, inp.Size())    
let toGray (i:Mat) (o:Mat) = Cv2.CvtColor(!>i,!>o,ColorConversionCodes.BGR2GRAY)

let warpGray (m:Mat) (inp:Mat) (gray:Mat) =
    let t = new Mat()
    warpFrame m inp t
    toGray t gray

//threshold and warp
let transformFrame (m:Mat) (inp:Mat) (gray:Mat) =
    use t = new Mat()
    thresholdFrame inp t
    warpFrame m t gray

let dumpImageProject() = 
    let f = @"D:\Repos\CarND-Advanced-Lane-Lines\project_video.mp4"
    use clipIn = new VideoCapture(f)
    let (w,h) = clipIn.FrameWidth,clipIn.FrameHeight
    let frames = clipIn |> Seq.unfold (fun x -> if x.Grab() then Some(x.RetrieveMat(),x) else None)
    let fn = frames |> Seq.skip 25 |> Seq.head //480
    fn.SaveImage(Path.Combine(Path.GetDirectoryName(f),"im1.jpg"))

let peaks (inp:Mat) = 
    let a = Array.zeroCreate inp.Width
    for r in inp.Rows-1..-1..inp.Rows/2 do
        for c in 0..inp.Cols-1 do
            let v = inp.Get(r,c)
            if v > 0 then
                a.[c] <- a.[c] + 1
    let mutable mi = ref 0
    a

let argmaxX (a:int[]) =
    use aI = InputArray.Create(a)
    let mutable minVal = 0.
    let mutable maxVal = 0.
    let mutable minIdx = 0
    let mutable maxIdx = 0
    Cv2.MinMaxIdx(aI, &minVal, &maxVal, &minIdx, &maxIdx)
    maxIdx

let makeRect center (size:Size) margin bottom  winHeight =
    let x = center - margin |> max 0 |> min (size.Width-1)
    let y = bottom - winHeight |> max 0 |> min (size.Height-1)
    let baseX = 2*margin
    let w = baseX |> min (size.Width - x)
    let h = winHeight |> min (size.Height - y)            
    Rect(x,y,w,h)

let toPointList (inp:Mat) (offsetRect:Rect) =
    [
        for r in 0..inp.Rows-1 do
            let p = inp.Get<Point>(r,0)
            let p = Point(p.X + offsetRect.X, p.Y + offsetRect.Y)
            yield p
    ]

//return mean shifted rectangle and #of nonzero pixels found
let recenterWindow(img:Mat) (r:Rect) =
    use w = img.[r]
    use nz = w.FindNonZero()          //nonzero pixels
    if nz.Height > minpx then
        let xs = [for r in 0..nz.Rows-1 -> let v = nz.Get<Vec2i>(r,0) in v.Item0 |> float] //x indexes of non zero pixels
        let newCtr = xs |> List.average |> int // mean of xs
        let prevCtr = r.Width/2
        let shift = newCtr - prevCtr
        let midp = r.X + shift + prevCtr
        toPointList nz r,makeRect midp (img.Size()) winMargin (r.Y + r.Height) r.Height
    else
        [],r

let dmp2 (m:Mat) =
    for r in 0..m.Rows-1 do
        for c in 0..m.Cols-1 do
            let v = m.Get<Vec2i>(r,c)
            printfn "%d, %d = %A" r c (v.Item0, v.Item1)

let dmp1 (m:Mat) =
    for r in 0..m.Rows-1 do
        for c in 0..m.Cols-1 do
            let v = m.Get<uint8>(r,c)
            printfn "%d, %d = %d" r c v

let midpBot (r:Rect) = Point(r.X + (r.Width / 2),r.Y + (r.Height))

let moveUp (r:Rect) by = Rect(r.X, r.Y - by |> max 0, r.Width, r.Height)

let plotPoints (img:Mat) (pts:Point seq) =
    for p in pts do
        Cv2.Circle(!> img, p, 10, Scalar(255.,0.,0.))

let plotVertex (img:Mat) (pts:Point seq) =
    for p in pts do
        Cv2.Circle(!> img, p, 5, Scalar(0.,0.,255.))

let splitPoints (pts:Point seq) = 
    let xs = pts |> Seq.map (fun p->float p.X) |> Seq.toArray
    let ys = pts |> Seq.map (fun p->float p.Y) |> Seq.toArray
    xs,ys

type Line = 
| Linear    of c:float*b:float 
| Quadratic of c:float*b:float*a:float 
| LinTrns   of c:float*b:float
| QuadTrns  of c:float*b:float*a:float 

let linePoints sz line =
    match line with
    | Linear (c,b)      -> [for i in 0..sz -> let x = float i in x, c + b * x]
    | LinTrns (c,b)     -> [for i in 0..sz -> let x = float i in c + b * x, x]
    | Quadratic (c,b,a) -> [for i in 0..sz -> let x = float i in x, c + b * x + a * (x**2.)]
    | QuadTrns (c,b,a) -> [for i in 0..sz -> let x = float i in c + b * x + a * (x**2.), x]
    |> List.map Point

let solveForY (c,b,a) y =
    let y = float y
    FindRoots.Quadratic(c - y, b, a)

let lineVertex (c,b,a) = 
    let x = -b/(2.*a)
    let y = c + b*x + a*(x**2.0)
    Point(x,y)

let trimLeft (v:Point) (pts:Point seq) = 
    //printfn "trim left"
    pts |> Seq.filter (fun p -> p.X >= v.X)

let trimRight (v:Point) (pts:Point seq) = 
    //printfn "trim right"
    pts |> Seq.filter (fun p -> p.X <= v.X)

let trimSegment (sz:Size) (r1:Numerics.Complex,r2:Numerics.Complex) (vertex:Point) isLeft (pts:Point seq) =
    let iL,iR = if r1.Real < r2.Real then int r1.Real,int r2.Real else int r2.Real, int r1.Real
    let midp = sz.Width/2
    //printfn "iL,iR,midp,isLeft=%d,%d,%d,%A" iL iR midp isLeft
    if isLeft then
        if iR < midp then trimLeft vertex pts else trimRight vertex pts
    else
        //right line
        if iL > midp then trimRight vertex pts else trimLeft vertex pts

let trimFrame (sz:Size) (pts:Point seq) =  pts |> Seq.filter (fun p->p.X >= 0 && p.X <= sz.Width && p.Y >= 0 && p.Y <= sz.Height)

let trimLine (sz:Size) line isLeft (pts:Point seq) =
    match line with
    | Linear _ | LinTrns _ ->  pts |> trimFrame sz
    | Quadratic (c,b,a) -> 
        let v = lineVertex (c,b,a)                     //find parabola vertex
        let roots = solveForY (c,b,a) sz.Height
        trimSegment sz roots v isLeft pts |> trimFrame sz
    | QuadTrns (c,b,a) ->
        let v = lineVertex (c,b,a)                     //find parabola vertex
        let roots = solveForY (c,b,a) sz.Width
        trimSegment sz roots v isLeft pts |> trimFrame sz
       

let isStraight (mids:Point seq) = 
    let minX,maxX = mids |> Seq.minBy (fun p->p.X), mids |> Seq.maxBy (fun p->p.X)
    if abs (maxX.X - minX.X) < LINE_ST_TH then
        true
    else
        false

let straightNess (mids:Point seq) = 
    let minY,maxY = mids |> Seq.minBy (fun p->p.Y), mids |> Seq.maxBy (fun p->p.Y)
    maxY.X - minY.X

let fitLineToBoxes (sz:Size) (midpBots:Point seq) =
    let w2 = sz.Width/2
    let xs,ys = 
        midpBots 
        |> Seq.collect (fun p -> 
        [
            p
            Point(p.X-w2,p.Y)
            Point(p.X + w2, p.Y)
            Point(p.X - w2, p.Y - sz.Height)
            Point(p.X + w2, p.Y - sz.Height)
        ])
        |> splitPoints
    Fit.Line(xs,ys) |> Linear

let fitTrans (pts:Point seq) = 
    let xs, ys = splitPoints pts
    Fit.Line(ys,xs) |> LinTrns

let fitLine sz mids (pts:Point seq) =
    let xs,ys = splitPoints pts
        //let xs,ys = splitPoints mids
    let stn = straightNess mids
    let line = 
        if abs stn < LINE_ST_TH then
            fitTrans pts
        elif abs stn < 200 then
            let m =  LinearRegression.DirectRegressionMethod.NormalEquations
            let ps = Fit.Polynomial(ys,xs,2,m)
            let c,b,a = ps.[0],ps.[1],ps.[2]
            QuadTrns (c,b,a)
        
        else   
            let m =  LinearRegression.DirectRegressionMethod.NormalEquations
            let ps = Fit.Polynomial(xs,ys,2,m)
            let c,b,a = ps.[0],ps.[1],ps.[2]
            Quadratic (c,b,a)
    stn,line

let findLines (warped:Mat) =
    let sz = warped.Size()
    let a = peaks warped                                    //find peeks in lower part of image on X axis
    let leftCtr = argmaxX a.[0..sz.Width/2]
    let rightCtr = argmaxX a.[sz.Width/2..] + sz.Width/2
    let winHeight = warped.Height/nWindows
    let baseLeftRect  = makeRect leftCtr sz winMargin sz.Height winHeight
    let baseRigthRect = makeRect rightCtr sz winMargin sz.Height winHeight
    let startNzPxL,startLeftRect = recenterWindow warped baseLeftRect
    let startNzPxR,startRightRect = recenterWindow warped baseRigthRect
    let startState = 
            (
                startLeftRect,                                           //bottom rectangles
                startRightRect,
                ResizeArray[startNzPxL,midpBot startLeftRect,startLeftRect.Y],    //nonzero count, coordinates of bottom rects
                ResizeArray[startNzPxR,midpBot startRightRect,startRightRect.Y]   
            )
    let _,_,leftPtsR,righPtsR =
        (startState,[1..nWindows-1]) 
        ||> Seq.fold(fun (wL,wR,accL,accR) i ->
            let wL' = moveUp wL winHeight             //move rectangles up 1 level
            let wR' = moveUp wR winHeight          
            //printfn "Up wL'=%A,wR'%A" wL' wR'
            let nzPxL,wL' = recenterWindow warped wL'       //recenter rectangles on mean of pixels
            //printfn "RC wL'2=%A" wL'
            let nzPxR,wR' = recenterWindow warped wR'
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
    let stL,lineLeft = fitLine sz leftMids leftPoints
    let stR,lineRight = fitLine sz rightMids rightPoints
    stL,stR,lineLeft,leftMids,leftPoints,lineRight,rightMids,rightPoints
    
let processFrame (m:Mat) (minv:Mat) (inp:Mat) (outp:Mat) = 
    let sz = inp.Size()
    use warped = new Mat()
    transformFrame m inp warped                     //threshold and warp frame 
    //win "warped" warped
    let stL,stR,lineLeft,leftMids,leftPoints,lineRight,rightMids,rightPoints = findLines warped      //find lines
    let lpx = linePoints (warped.Width-1) lineLeft   //generate points from equations
    let rpx = linePoints (warped.Width-1) lineRight
    let lpxT = lpx//trimLine sz lineLeft true lpx
    let rpxT = rpx//trimLine sz lineLeft false rpx
    printfn "stL:%d, stR%d" stL stR
    let pts = 
        if stL < 0 then
            Seq.append lpxT (Seq.rev rpxT)
        else
            Seq.append  (Seq.rev lpxT) rpxT  //join line points to form 1 polygon
    //let pts = Seq.append lpxT rpxT        //join line points to form 1 polygon
    use o2 = inp.EmptyClone()
    plotPoints o2 lpxT
    plotPoints o2 rpxT
    //Cv2.FillConvexPoly((o2:Mat),pts, Scalar(50.,215.,50.)) //draw polygon
    Cv2.FillPoly((o2:Mat),[pts], Scalar(50.,215.,50.)) //draw polygon
    //win "o2" o2    
    Cv2.WarpPerspective(!>o2,!>o2,!>minv,o2.Size())    //unwrap
    Cv2.AddWeighted(!>inp,1.,!>o2,0.6,0.,!>outp)       //overlay on input

let testProcessFrame() =
    let m = getTransform()
    let minv = m.Inv()
    let f = @"D:\Repos\CarND-Advanced-Lane-Lines\project_video.mp4"
    use clipIn = new VideoCapture(f)
    //while clipIn.Grab() do
    //    let inp = clipIn.RetrieveMat()
    //    let outp = new Mat()
    //    processFrame m minv inp outp
    for i in 1..1 do printfn "%d" i; clipIn.Grab() |> ignore
    //let inp = Cv2.ImRead( @"D:\Repos\CarND-Advanced-Lane-Lines\im1.jpg")
    let inp = clipIn.RetrieveMat()
    win "inp" inp
    let outp = new Mat()
    processFrame m minv inp outp
    win "outp" outp


let findLanes videoFile =
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
            processFrame m minv inp outp
        with ex -> 
            printfn "%s" ex.Message
            printfn "frame miss %d"  !r
        r := !r + 1
        clipOut.Write(outp)
        inp.Release()
        outp.Release()

let testProcessing() =
    findLanes @"D:\Repos\CarND-Advanced-Lane-Lines\project_video.mp4"
    findLanes @"D:\Repos\CarND-Advanced-Lane-Lines\project_video_alt.mp4"
    findLanes @"D:\Repos\CarND-Advanced-Lane-Lines\challenge_video.mp4"
    findLanes @"D:\Repos\CarND-Advanced-Lane-Lines\harder_challenge_video.mp4"

let topDown h xs = xs |> Seq.map(fun x -> float h - x)

let experimentalLines() = 
    let m = getTransform()
    let minv = m.Inv()
    let i = Cv2.ImRead( @"D:\Repos\CarND-Advanced-Lane-Lines\im1.jpg")
    let warped = new Mat()
    transformFrame m i warped
    //win "warped" warped
    let sz = warped.Size()
    let a = peaks warped                                    //find peeks in lower part of image on X axis
    let leftCtr = argmaxX a.[0..sz.Width/2]
    let rightCtr = argmaxX a.[sz.Width/2..] + sz.Width/2
    let winHeight = warped.Height/nWindows
    let baseLeftRect  = makeRect leftCtr sz winMargin sz.Height winHeight
    let baseRigthRect = makeRect rightCtr sz winMargin sz.Height winHeight
    let startNzPxL,startLeftRect = recenterWindow warped baseLeftRect
    let startNzPxR,startRightRect = recenterWindow warped baseRigthRect
    let startState = 
            (
                startLeftRect,                                           //bottom rectangles
                startRightRect,
                ResizeArray[startNzPxL,midpBot startLeftRect,startLeftRect.Y],    //nonzero count, coordinates of bottom rects
                ResizeArray[startNzPxR,midpBot startRightRect,startRightRect.Y]   
            )
    let _,_,leftPtsR,righPtsR =
        (startState,[1..nWindows-1]) 
        ||> Seq.fold(fun (wL,wR,accL,accR) i ->
            let wL' = moveUp wL winHeight             //move rectangles up 1 level
            let wR' = moveUp wR winHeight          
            //printfn "Up wL'=%A,wR'%A" wL' wR'
            let nzPxL,wL' = recenterWindow warped wL'       //recenter rectangles on mean of pixels
            //printfn "RC wL'2=%A" wL'
            let nzPxR,wR' = recenterWindow warped wR'
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
    let _,lineLeft = fitLine sz leftMids leftPoints
    let _,lineRight = fitLine sz rightMids rightPoints
    //lineLeft.[2] <- 0.
    //lineRight.[2] <- 0.
    let lpx = linePoints (warped.Width-1) lineLeft   //generate points from equations
    let rpx = linePoints (warped.Width-1) lineRight
    let lpxT = trimLine sz lineLeft true lpx
    let rpxT = trimLine sz lineLeft false rpx
    i,warped,minv,leftPoints,rightPoints,leftMids,rightMids,lpx,rpx,lpxT,rpxT

//experiment with image processing (not used)
let experimental() =
    let i,warped,minv,leftPoints,rightPoints,leftMids,rightMids,lpx,rpx,lpxT,rpxT = experimentalLines()

    leftPoints |> Seq.map(fun p->p.X,p.Y)|> Chart.FastPoint |> Chart.WithTitle "Left Raw"
    rightPoints |> Seq.map(fun p->p.X,p.Y)|> Chart.FastPoint |> Chart.WithTitle "Right Raw"
    Chart.Point(leftMids |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Left mids"
    Chart.Point(rightMids |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Right mids"
    Chart.Point (lpx |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Left"
    Chart.Point (rpx |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Right"
    Chart.Point (lpxT |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Fit Left"
    Chart.Point (rpxT |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Fit Right"
    [
        leftPoints |> Seq.map(fun p->p.X,p.Y)|> Chart.FastPoint |> Chart.WithTitle "Left Raw"
        Chart.Point (lpxT |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Left"
    ] |> Chart.Combine
    [
        rightPoints |> Seq.map(fun p->p.X,p.Y)|> Chart.FastPoint |> Chart.WithTitle "Right Raw"
        Chart.Point (rpxT |> Seq.map(fun p->p.X,p.Y)) |> Chart.WithTitle "Right"
    ] |> Chart.Combine
    

    //let pts = Seq.append  (Seq.rev lpxT) rpxT
    let pts = Seq.append  lpxT rpxT
    let o2 = i.EmptyClone()
    plotPoints o2 lpxT
    //plotPoints o2 lpx
    plotPoints o2 rpxT
    //win "o2" o2
    //win "warped" warped
    Cv2.FillPoly((o2:Mat),[pts], Scalar(50.,215.,50.)) 
    Cv2.WarpPerspective(!>o2,!>o2,!>minv,o2.Size())
    let o2c = new Mat()
    Cv2.AddWeighted(!>i,1.,!>o2,0.3,0.,!>o2c)
    win "w" o2c


