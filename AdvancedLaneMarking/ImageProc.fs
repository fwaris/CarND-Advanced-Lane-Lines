module ImageProc
open OpenCVCommon
open OpenCvSharp
open BaseTypes

let PrspctvTxSrc = [691,450;1150,720;250,720;597,450]
//let PrspctvTxDst = [1150,10;1150,720;250,720;250,10]
let PrspctvTxDst = [1150,0;1150,720;250,720;250,0]

let getTransform() =
    //let src = [653,431;1134,720;228,720;626,431]
    //let dst = [1134,0;1134,720;228,720;228,0]
    //let src = [691,450;1150,720;250,720;597,450]
    //let dst = [1150,10;1150,720;250,720;250,10]
    let src = PrspctvTxSrc |> List.map(fun (x,y) -> Point2f(float32 x, float32 y))
    let dst = PrspctvTxDst |> List.map(fun (x,y) -> Point2f(float32 x ,float32 y))
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

