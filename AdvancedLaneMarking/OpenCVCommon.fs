module OpenCVCommon
open OpenCvSharp
//utility operator for F# implicit conversions 
let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)
let inline (@) x y = Size( (x:int) , (y:int))
