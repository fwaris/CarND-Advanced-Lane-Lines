module CalibrateCamera
open OpenCvSharp
open System
open OpenCVCommon
open System.IO
let folder = @"D:\Repos\CarND-Advanced-Lane-Lines\camera_cal"
let sqaure_size = 120 //(1280-60)/10
let p_width = 9
let p_height = 6

let objectPoints = 
    seq{ 
        for i in 0 .. p_height - 1 do 
            for j in 0 .. p_width - 1 do
                yield  Point3f(float32 (j*sqaure_size), float32 (i*sqaure_size), 0.0f)
    } 
    |> Seq.toArray

let isCalibrationFile (s:string) =
   let fn = Path.GetFileName(s)
   fn.Contains("calibration") && Path.GetExtension(s) = ".jpg"

let calibrtionFiles() = Directory.GetFiles(folder) |> Array.filter isCalibrationFile

let calibrateCamera(folder,pattern:Size,objp:Point3f[]) =
    let files = calibrtionFiles()
    use temp = Cv2.ImRead(files.[0])
    let objpts,imgpts = 
        (([],[]),files)
        ||> Array.fold(fun (ops,ips) f ->
            use m = Cv2.ImRead(f)
            use g = new Mat()
            Cv2.CvtColor(!>m,!>g,ColorConversionCodes.BGR2GRAY)
            let mutable cnrns:Point2f[] = null
            let r = Cv2.FindChessboardCorners(!>g, pattern, &cnrns)
            if r then
                objp::ops, cnrns::ips
            else
                printfn "no read for %s" f
                ops,ips
            )    
    let size = temp.Size()
    let mutable mtx:float[,] = Array2D.zeroCreate 3 3
    let mutable dist:float[] = Array.zeroCreate 8
    let mutable rvecs:Vec3d[] = null
    let mutable tvecs:Vec3d[] = null
    let seqObj = seq{for p in objpts -> Array.toSeq p}
    let seqImg = seq{for i in imgpts -> Array.toSeq i}
    let m = Cv2.CalibrateCamera(seqObj,seqImg,size, mtx, dist, &rvecs, &tvecs)
    let mtxA  = InputArray.Create(mtx,!>MatType.CV_64F)
    let distA = InputArray.Create(dist, !> MatType.CV_64F)
    mtxA,distA

let findCBC() =
    let files = calibrtionFiles()
    use i = Cv2.ImRead(files.[3])
    use g = new Mat()
    Cv2.CvtColor(!>i,!>g,ColorConversionCodes.BGR2GRAY)
    let mutable cnrns:Point2f[] = null
    let r = Cv2.FindChessboardCorners(!>g, p_width@p_height, &cnrns)
    Cv2.DrawChessboardCorners(!>i,p_width@p_height,cnrns,true)
    let fn = folder + "/drawncb.jpg"
    i.SaveImage(fn)
    //Utils.win "i" i
    

(*
let mtx,dist = calibrateCamera(folder, p_width @ p_height, objectPoints)
let t1 = Cv2.ImRead(folder + "/calibration1.jpg")
let u1 = new Mat()
Cv2.Undistort(!> t1, !> u1,  mtx,dist)
u1.SaveImage(folder + "/undist.jpg")
win "t1" t1
win "undist" u1
*)

