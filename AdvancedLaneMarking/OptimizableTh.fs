module ThOptimizable
open OpenCVCommon
open OpenCvSharp
open CalibrateCamera
open FSharp.Charting
open System
open System.IO
open Microsoft.FSharp.NativeInterop
open Accord.MachineLearning
open Accord.Statistics.Models.Regression
open Accord.Statistics.Models.Regression.Fitting
open ILNumerics
open ILNumerics.Toolboxes
#nowarn "9"

open MathNet.Numerics
open Accord.Statistics.Models.Regression.Linear

let chlng_v = @"D:\Repos\CarND-Advanced-Lane-Lines\challenge_video.mp4"
let chlngH_v = @"D:\Repos\CarND-Advanced-Lane-Lines\harder_challenge_video.mp4"
let prjct_v = @"D:\Repos\CarND-Advanced-Lane-Lines\project_video.mp4"

type VParms =
    {
        nWindows        : int //9
        winMargin       : int // 100
        minpx           : int // 50
        LINE_ST_TH      : int // 100 //px
        clrThB          : int // 170
        clrThG          : int // 170
        clrThR          : int // 170
        grdThL          : int // 60
        grdThU          : int // 100
    }
    with
    static member  Default =
        {
            nWindows        = 9
            winMargin       =  100
            minpx           =  50
            LINE_ST_TH      =  100 //px
            clrThB          =  170
            clrThG          =  170
            clrThR          =  170
            grdThL          =  60
            grdThU          =  100
        }
        
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

let colorThreshold (p:VParms) (img:Mat) (gray:Mat) =
    let ims = img.Split()
    let imB,imG,imR = ims.[0], ims.[1], ims.[2]
    use tB = new Mat()
    use tG = new Mat()
    use tR = new Mat()
    Cv2.Threshold(!> imB, !> tB, float p.clrThB, 255., ThresholdTypes.Binary) |> ignore
    Cv2.Threshold(!> imG, !> tG, float p.clrThG, 255., ThresholdTypes.Binary) |> ignore
    Cv2.Threshold(!> imR, !> tR, float p.clrThR, 255., ThresholdTypes.Binary) |> ignore
    use o = new Mat()
    Cv2.BitwiseAnd(!>tB, !> tG, !> o)
    Cv2.BitwiseAnd(!>tR, !>o, !> gray)
    [imB;imG;imR]|>List.iter(fun x -> x.Release())

let gradientThreshold (p:VParms) (img:Mat) (gray:Mat) =
    use mutable tmp =  new Mat()
    Cv2.CvtColor(!>img, !> tmp, ColorConversionCodes.BGR2GRAY )
    tmp <- tmp.Sobel( !> MatType.CV_64F,1,0)
    Cv2.ConvertScaleAbs(!>tmp, !> tmp)
    Cv2.InRange(!>tmp, Scalar(float p.grdThL),Scalar(float p.grdThU),!>gray)

let thresholdFrame (p:VParms) (inP:Mat) (gray:Mat) =
    use m1 = new Mat()
    use m2 = new Mat()
    colorThreshold p inP m1
    gradientThreshold p inP m2
    Cv2.BitwiseOr(!>m1,!>m2,!>gray)
    
let warpFrame (m:Mat) (inp:Mat) (gray:Mat) = Cv2.WarpPerspective(!>inp, !>gray, !>m, inp.Size())    
let toGray (i:Mat) (o:Mat) = Cv2.CvtColor(!>i,!>o,ColorConversionCodes.BGR2GRAY)

let warpGray (m:Mat) (inp:Mat) (gray:Mat) =
    let t = new Mat()
    warpFrame m inp t
    toGray t gray

//threshold and warp
let transformFrame (p:VParms) (m:Mat) (inp:Mat) (gray:Mat) =
    use t = new Mat()
    thresholdFrame p inp t
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
let recenterWindow (p:VParms) (img:Mat) (r:Rect) =
    use w = img.[r]
    use nz = w.FindNonZero()          //nonzero pixels
    if nz.Height > p.minpx then
        let xs = [for r in 0..nz.Rows-1 -> let v = nz.Get<Vec2i>(r,0) in v.Item0 |> float] //x indexes of non zero pixels
        let newCtr = xs |> List.average |> int // mean of xs
        let prevCtr = r.Width/2
        let shift = newCtr - prevCtr
        let midp = r.X + shift + prevCtr
        toPointList nz r,makeRect midp (img.Size()) p.winMargin (r.Y + r.Height) r.Height
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

type Line = Linear of c:float*b:float | Quadratic of c:float*b:float*a:float | LinTrns of c:float*b:float

let linePoints sz line =
    match line with
    | Linear (c,b)      -> [for i in 0..sz -> let x = float i in x, c + b * x]
    | LinTrns (c,b)     -> [for i in 0..sz -> let x = float i in c + b * x, x]
    | Quadratic (c,b,a) -> [for i in 0..sz -> let x = float i in x, c + b * x + a * (x**2.)]
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

let isStraight (p:VParms) (mids:Point seq) = 
    let minX,maxX = mids |> Seq.minBy (fun p->p.X), mids |> Seq.maxBy (fun p->p.X)
    if abs (maxX.X - minX.X) < p.LINE_ST_TH then
        true
    else
        false

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

let fitLine (p:VParms) sz mids (pts:Point seq) =
    let xs,ys = splitPoints pts
        //let xs,ys = splitPoints mids
    if isStraight p mids then
        //mids |> splitPoints |> Fit.Line |> Linear
        fitTrans pts

        //let ransac = new RANSAC<SimpleLinearRegression>(5)
        //ransac.Fitting <- (fun (samples:int[]) -> 
        //    let xs = [|for i in 0..samples.Length-1 -> xs.[i]|]
        //    let ys = [|for i in 0..samples.Length-1 -> ys.[i]|]
        //    SimpleLinearRegression.FromData(xs,ys)
        //)
        //ransac.Degenerate <- (fun r -> false)
        //ransac.Distances <- (fun r ds ->
        //    xs
        //    |> Array.mapi (fun i x -> i,r.Transform(x) - ys.[i])
        //    |> Array.filter (fun (i,tx) -> tx * tx < ds)
        //    |> Array.map (fun (i,_) -> i)
        //)
        //let line = ransac.Compute(xs.Length)
        //if line = null then 
        //   fitLineToBoxes sz pts
        //else
        //    Linear(line.Intercept,line.Slope)
    else   
        let m =  LinearRegression.DirectRegressionMethod.NormalEquations
        let ps = Fit.Polynomial(xs,ys,2,m)
        let c,b,a = ps.[0],ps.[1],ps.[2]
        Quadratic (c,b,a)

let findLines (p:VParms) (warped:Mat) =
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
    let lineLeft = fitLine p sz leftMids leftPoints
    let lineRight = fitLine p sz rightMids rightPoints
    lineLeft,leftMids,leftPoints,lineRight,rightMids,rightPoints
    
let processFrame (p:VParms) (m:Mat) (minv:Mat) (inp:Mat) (outp:Mat) = 
    let sz = inp.Size()
    use warped = new Mat()
    transformFrame p m inp warped                     //threshold and warp frame 
    //win "warped" warped
    let lineLeft,leftMids,leftPoints,lineRight,rightMids,rightPoints = findLines p warped      //find lines
    let lpx = linePoints (warped.Width-1) lineLeft   //generate points from equations
    let rpx = linePoints (warped.Width-1) lineRight
    let lpxT = trimLine sz lineLeft true lpx
    let rpxT = trimLine sz lineLeft false rpx
    //let pts = Seq.append  (Seq.rev lpxT) rpxT        //join line points to form 1 polygon
    let pts = Seq.append lpxT rpxT        //join line points to form 1 polygon
    use o2 = inp.EmptyClone()
    plotPoints o2 lpxT
    plotPoints o2 rpxT
    Cv2.FillPoly((o2:Mat),[pts], Scalar(50.,215.,50.)) //draw polygon
    //win "o2" o2    
    Cv2.WarpPerspective(!>o2,!>o2,!>minv,o2.Size())    //unwrap
    Cv2.AddWeighted(!>inp,1.,!>o2,0.6,0.,!>outp)       //overlay on input

let processFrameFit (p:VParms) (m:Mat) (minv:Mat) (inp:Mat) (outp:Mat) = 
    let sz = inp.Size()
    use warped = new Mat()
    transformFrame p m inp warped                     //threshold and warp frame 
    //win "warped" warped
    let lineLeft,leftMids,leftPoints,lineRight,rightMids,rightPoints = findLines p warped      //find lines
    lineLeft,lineRight
    let lpx = linePoints (warped.Width-1) lineLeft   //generate points from equations
    let rpx = linePoints (warped.Width-1) lineRight
    let lpxT = trimLine sz lineLeft true lpx
    let rpxT = trimLine sz lineLeft false rpx
    //let pts = Seq.append  (Seq.rev lpxT) rpxT        //join line points to form 1 polygon
    let pts = Seq.append lpxT rpxT        //join line points to form 1 polygon
    use o2 = inp.EmptyClone()
    plotPoints o2 lpxT
    plotPoints o2 rpxT
    Cv2.FillPoly((o2:Mat),[pts], Scalar(50.,215.,50.)) //draw polygon
    //win "o2" o2    
    Cv2.WarpPerspective(!>o2,!>o2,!>minv,o2.Size())    //unwrap
    Cv2.AddWeighted(!>inp,1.,!>o2,0.6,0.,!>outp)       //overlay on input

let findLanes (p:VParms) videoFile =
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
        with _ -> printfn "frame miss %d"  !r
        r := !r + 1
        clipOut.Write(outp)
        inp.Release()
        outp.Release()

let testProcessing() =
    let p = VParms.Default
    findLanes p @"D:\Repos\CarND-Advanced-Lane-Lines\project_video.mp4"
    findLanes p @"D:\Repos\CarND-Advanced-Lane-Lines\challenge_video.mp4"
    findLanes p @"D:\Repos\CarND-Advanced-Lane-Lines\harder_challenge_video.mp4"


