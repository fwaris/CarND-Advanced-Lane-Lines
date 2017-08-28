module ImageProc
open OpenCVCommon
open OpenCvSharp
open BaseTypes

//coordinates for perspective transform
let PrspctvTxSrc = [691,450;1150,720;250,720;597,450]
let PrspctvTxDst = [1150,0;1150,720;250,720;250,0]

//returns the perspective transform matrix
let getTransform() =
    let src = PrspctvTxSrc |> List.map(fun (x,y) -> Point2f(float32 x, float32 y))
    let dst = PrspctvTxDst |> List.map(fun (x,y) -> Point2f(float32 x ,float32 y))
    let m = Cv2.GetPerspectiveTransform(src,dst)
    m

//threshold image using hsv
let colorThrldHSV (p:VParms) (img:Mat) (gray:Mat) =
    use hsvMat = new Mat()
    Cv2.CvtColor(!>img,!>hsvMat,ColorConversionCodes.BGR2HSV)
    Cv2.InRange(!>hsvMat, 
                Scalar(float p.hsvHueTh, float p.hsvStrTh, float p.hsvBrtTh), 
                Scalar(180., 255., 255.), !>gray)
    //Cv2.InRange(!>hsvMat, Scalar(20., 85., 85.), Scalar(255., 255., 255.), !>gray)

//threshold image for sobel gradients
let gradientThreshold (p:VParms) (img:Mat) (gray:Mat) =
    use mutable tmp =  new Mat()
    Cv2.CvtColor(!>img, !> tmp, ColorConversionCodes.BGR2GRAY )
    tmp <- tmp.Sobel( !> MatType.CV_64F,1,0,p.sobelKernel)
    Cv2.ConvertScaleAbs(!>tmp, !> tmp)
    Cv2.InRange(!>tmp, Scalar(float p.grdThL),Scalar(float p.grdThU),!>gray)

//apply all thresholds to image (hsv and sobel gradient)
let thresholdFrame (p:VParms) (inP:Mat) (gray:Mat) =
    use m1 = new Mat()
    use m2 = new Mat()
    colorThrldHSV p inP m1
    gradientThreshold p inP m2
    Cv2.BitwiseOr(!>m1,!>m2,!>gray) //merge thresholds
    
let warpFrame (m:Mat) (inp:Mat) (gray:Mat) = Cv2.WarpPerspective(!>inp, !>gray, !>m, inp.Size())    

//threshold and warp
let transformFrame (p:VParms) (m:Mat) (inp:Mat) (gray:Mat) =
    use t = new Mat()
    thresholdFrame p inp t
    warpFrame m t gray

