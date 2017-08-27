module LaneFind
open BaseTypes
open OpenCvSharp
open OpenCVCommon

let peaks (inp:Mat) = 
    let a = Array.zeroCreate inp.Width
    let maxRow = float inp.Rows / 2.5 |> int
    for r in inp.Rows-1..-1..maxRow do
        for c in 0..inp.Cols-1 do
            let v = inp.Get(r,c)
            if v > 0 then
                a.[c] <- a.[c] + 1
    a

let argmaxX (p:VParms) (a:int[]) =
    let maxG = 
        a 
        |> Seq.windowed p.argmaxWidth 
        |> Seq.mapi (fun i xs -> i,xs|> Seq.sum) 
        |> Seq.maxBy snd
    fst maxG
    //use aI = InputArray.Create(a)
    //let mutable minVal = 0.
    //let mutable maxVal = 0.
    //let mutable minIdx = 0
    //let mutable maxIdx = 0
    //Cv2.MinMaxIdx(aI, &minVal, &maxVal, &minIdx, &maxIdx)
    //maxIdx

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
    if nz.Height >  p.minpx then
        let xs = [for r in 0..nz.Rows-1 -> let v = nz.Get<Vec2i>(r,0) in v.Item0 |> float] //x indexes of non zero pixels
        let newCtr = xs |> List.average |> int // mean of xs
        let prevCtr = r.Width/2
        let shift = newCtr - prevCtr
        let midp = r.X + shift + prevCtr
        toPointList nz r,makeRect midp (img.Size()) p.winMargin (r.Y + r.Height) r.Height
    else
        [],r

let midpBot (r:Rect) = Point(r.X + (r.Width / 2),r.Y + (r.Height))

let moveUp (r:Rect) by = Rect(r.X, r.Y - by |> max 0, r.Width, r.Height)

let plotPoints (img:Mat) (pts:Point seq) =
    for p in pts do
        Cv2.Circle(!> img, p, 20, Scalar(255.,255.,0.),5)

let plotVertex (img:Mat) (pts:Point seq) =
    for p in pts do
        Cv2.Circle(!> img, p, 5, Scalar(0.,0.,255.))

let splitPoints (pts:Point seq) = 
    let xs = pts |> Seq.map (fun p->float p.X) |> Seq.toArray
    let ys = pts |> Seq.map (fun p->float p.Y) |> Seq.toArray
    xs,ys

