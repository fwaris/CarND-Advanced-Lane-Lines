module LineFitting
open LaneFind
open OpenCvSharp
open MathNet.Numerics
open System
open BaseTypes

type Line = QuadTrns  of c:float*b:float*a:float 

let linePoints sz  (QuadTrns(c,b,a)) =
    [for i in  0..sz -> let y = float i in c + b * y + a * (y**2.), y]
    |> List.map Point

let lineVertex (c,b,a) = 
    let x = -b/(2.*a)
    let y = c + b*x + a*(x**2.0)
    Point(x,y)

let fitLine (pts:Point seq) =
    let xs,ys = splitPoints pts
    let m =  LinearRegression.DirectRegressionMethod.NormalEquations
    let ps = Fit.Polynomial(ys,xs,2,m)
    let c,b,a = ps.[0],ps.[1],ps.[2]
    QuadTrns (c,b,a)

let Hm_per_pix = 30./720. //meters per pixel in y dimension
let Wm_per_pix = 3.7/700. //meters per pixel in x dimension

let toWorldLine sz (QuadTrns(c,b,a)) =
    let worldPts = 
        [for i in  0..sz -> let x = float i in x,c + b * x + a * (x**2.)]
        |> List.map (fun (tX,tY) -> (float sz) - tX, tY) //from top down to bottom up
        |> List.map (fun (tX,tY) -> tX * Hm_per_pix, tY * Wm_per_pix) //x y are swapped 
        |> List.map Point
    let xs,ys = splitPoints worldPts
    let m =  LinearRegression.DirectRegressionMethod.NormalEquations
    let ps = Fit.Polynomial(xs,ys,2,m)
    let c,b,a = ps.[0],ps.[1],ps.[2]
    QuadTrns (c,b,a)

let curvature maxH (QuadTrns(c,b,a)) = 
    let x = float maxH
    let y = c //+ b * x + a * (x**2.)
    //y_eval = np.max(ploty)
    let c = ((1. + (2.*a*0. + b)**2.)**1.5) / abs (2.0 * a)
    c
    //left_curverad = ((1 + (2*left_fit[0]*y_eval + left_fit[1])**2)**1.5) / np.absolute(2*left_fit[0])
    //right_curverad = ((1 + (2*right_fit[0]*y_eval + right_fit[1])**2)**1.5) / np.absolute(2*right_fit[0])
    //print(left_curverad, right_curverad)

let curvatureOld maxH (QuadTrns(c,b,a)) = 
    let x = float maxH
    let y = c + b * x + a * (x**2.)
    //y_eval = np.max(ploty)
    let c = ((1. + (2.*a*x + b)**2.)**1.5) / abs (2.0 * a)
    c
    //left_curverad = ((1 + (2*left_fit[0]*y_eval + left_fit[1])**2)**1.5) / np.absolute(2*left_fit[0])
    //right_curverad = ((1 + (2*right_fit[0]*y_eval + right_fit[1])**2)**1.5) / np.absolute(2*right_fit[0])
    //print(left_curverad, right_curverad)
        