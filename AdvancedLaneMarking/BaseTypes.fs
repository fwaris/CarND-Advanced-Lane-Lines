module BaseTypes

// a data structure that encapsulates
// hyperparameters for easier (automated) tuning
// of hyperparameters, if need be
type VParms =
    {
        nWindows        : int //         number of windows
        winMargin       : int //         window margin
        minpx           : int //         min pixels for window recentering
        argmaxWidth     : int //         width in pixels over which lane edge is located
        sobelKernel     : int //         kernel size for sobel
        curveSmoothN    : int //         N for smoothing curvature
        hsvHueThLo      : int //         hsv thresholds hue low
        hsvHueThHi      : int //         hsv thresholds hue high
        hsvStrTh        : int //                        saturation
        hsvBrtTh        : int //                        brightness
        whiteSnstvty    : int //         white color sensitivity
        grdThL          : int //         gradient cuttoff low
        grdThU          : int //         gradient cuttoff high
    }
    with
    static member  Default =         //values assigned to hyperparamemeters for this project
        {
            nWindows        = 9
            winMargin       = 100
            minpx           = 30
            sobelKernel     = 3
            curveSmoothN    = 3
            argmaxWidth     = 30
            hsvHueThLo      = 20
            hsvHueThHi      = 35
            hsvStrTh        = 85
            hsvBrtTh        = 90
            whiteSnstvty    = 20
            grdThL          = 35
            grdThU          = 100
        }
