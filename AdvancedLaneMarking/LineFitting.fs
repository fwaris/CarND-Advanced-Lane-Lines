module LineFitting
open LaneFind
open OpenCvSharp
open MathNet.Numerics
open System
open BaseTypes

//structure to hold line equation for hyperbola
// (x and y axis are swapped for most processing)
type Line = Hyperbola  of c:float*b:float*a:float 

//generate a list of points from line equation
let linePoints sz  (Hyperbola(c,b,a)) =
    [for i in  0..sz -> let y = float i in c + b * y + a * (y**2.), y] //x,y swapped
    |> List.map Point

//fit a hyperbola to points
let fitLine (pts:Point seq) =
    let xs,ys = splitPoints pts
    let m =  LinearRegression.DirectRegressionMethod.NormalEquations
    let ps = Fit.Polynomial(ys,xs,2,m)
    let c,b,a = ps.[0],ps.[1],ps.[2]
    Hyperbola (c,b,a)

let Hm_per_pix = 30./720. //meters per pixel in y dimension
let Wm_per_pix = 3.7/700. //meters per pixel in x dimension

//convert a line equation in pixel units to world units in meters 
//y is flipped so 0 is at bottom 
//(note y here is really swapped x)
let toWorldLine sz (Hyperbola(c,b,a)) =
    let worldPts = 
        [for i in  0..sz -> let x = float i in x,c + b * x + a * (x**2.)]
        |> List.map (fun (tX,tY) -> (float sz) - tX, tY) //from top down to bottom up
        |> List.map (fun (tX,tY) -> tX * Hm_per_pix, tY * Wm_per_pix) //x y are swapped 
        |> List.map Point
    let xs,ys = splitPoints worldPts
    let m =  LinearRegression.DirectRegressionMethod.NormalEquations
    let ps = Fit.Polynomial(xs,ys,2,m)
    let c,b,a = ps.[0],ps.[1],ps.[2]
    Hyperbola (c,b,a)

//given a hyperbola equation find curvature at bottom x=0
let curvatureAtBottom (Hyperbola(c,b,a)) = 
    //x is zero at the bottom for this evaluation
    //as the points have been flipped top down
    let y = c //+ b * x + a * (x**2.) 
    //y_eval = np.max(ploty)
    let c = ((1. + (2.*a*0. + b)**2.)**1.5) / abs (2.0 * a)
    c
    //left_curverad = ((1 + (2*left_fit[0]*y_eval + left_fit[1])**2)**1.5) / np.absolute(2*left_fit[0])
    //right_curverad = ((1 + (2*right_fit[0]*y_eval + right_fit[1])**2)**1.5) / np.absolute(2*right_fit[0])
    //print(left_curverad, right_curverad)
        