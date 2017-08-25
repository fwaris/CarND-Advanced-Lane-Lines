module LineFitting
open LaneFind
open OpenCvSharp
open MathNet.Numerics
open System
open BaseTypes

type Line = 
| Linear    of c:float*b:float 
| Quadratic of c:float*b:float*a:float 
| LinTrns   of c:float*b:float
| QuadTrns  of c:float*b:float*a:float 

let linePoints sz line =
    match line with
    | Linear (c,b)      -> [for i in -1..sz -> let x = float i in x, c + b * x]
    | LinTrns (c,b)     -> [for i in -1..sz -> let x = float i in c + b * x, x]
    | Quadratic (c,b,a) -> [for i in -1..sz -> let x = float i in x, c + b * x + a * (x**2.)]
    | QuadTrns (c,b,a) -> [for i in -1..sz -> let x = float i in c + b * x + a * (x**2.), x]
    |> List.map Point

//let solveForY (c,b,a) y =
//    let y = float y
//    FindRoots.Quadratic(c, b, a)

let lineVertex (c,b,a) = 
    let x = -b/(2.*a)
    let y = c + b*x + a*(x**2.0)
    Point(x,y)

let trimLeft (v:Point) (pts:Point seq) = 
    //printfn "trim left"
    pts |> Seq.filter (fun p -> p.X <= v.X)

let trimRight (v:Point) (pts:Point seq) = 
    //printfn "trim right"
    pts |> Seq.filter (fun p -> p.X >= v.X)


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
        if isLeft then trimRight v pts  else trimLeft v pts 
        //let roots = solveForY (c,b,a) sz.Height
        //trimSegment sz roots v isLeft pts |> trimFrame sz
    | QuadTrns (c,b,a) ->
        pts |> Seq.filter (fun p -> p.X < 0) //x and y are transformed
       

let isStraight (p:VParms) (mids:Point seq) = 
    let minX,maxX = mids |> Seq.minBy (fun p->p.X), mids |> Seq.maxBy (fun p->p.X)
    if abs (maxX.X - minX.X) < p.LINE_ST_TH then
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

let fitLine (p:VParms) sz mids (pts:Point seq) =
    let xs,ys = splitPoints pts
        //let xs,ys = splitPoints mids
    let m =  LinearRegression.DirectRegressionMethod.NormalEquations
    let ps = Fit.Polynomial(xs,ys,2,m)
    let c,b,a = ps.[0],ps.[1],ps.[2]
    let v = lineVertex (c,b,a)   
    let line =
        if a < 0. || a < 0.002 || v.Y > 10 then
            fitTrans pts
        else
            Quadratic (c,b,a)
    let stn = straightNess mids
    //let line = 
    //    if abs stn < p.LINE_ST_TH then
            
    //    //elif abs stn < p.TRANS_PB_TH then
    //    //    let m =  LinearRegression.DirectRegressionMethod.NormalEquations
    //    //    let ps = Fit.Polynomial(ys,xs,2,m)
    //    //    let c,b,a = ps.[0],ps.[1],ps.[2]
    //    //    QuadTrns (c,b,a)
    //    else   
    stn,line

let zeroIntersect = function
    | Linear (c,b)      -> -c/b //y = c + bx => x = (y-c)/b
    | LinTrns (c,b)     -> c //x = c + by
    | Quadratic (c,b,a) -> let (r1,r2)=FindRoots.Quadratic(c, b, a) in max r1.Real r2.Real
    | QuadTrns (c,b,a) -> c
