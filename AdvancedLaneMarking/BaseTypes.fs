module BaseTypes

type VParms =
    {
        nWindows        : int //9
        winMargin       : int // 100
        minpx           : int // 50
        argmaxWidth     : int // 30
        sobelKernel     : int //3
        lineSmoothN     : int //3
        clrThB          : int // 180
        clrThG          : int // 180
        clrThR          : int // 180
        grdThL          : int // 60
        grdThU          : int // 100
    }
    with
    static member  Default =
        {
            nWindows        = 9
            winMargin       = 100
            minpx           = 50
            sobelKernel     = 3
            lineSmoothN     = 5
            argmaxWidth     = 60 //30
            clrThB          = 170//170
            clrThG          = 170//170
            clrThR          = 170//170
            grdThL          = 25//60
            grdThU          = 100
        }
