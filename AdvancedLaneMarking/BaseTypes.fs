module BaseTypes

type VParms =
    {
        nWindows        : int //9
        winMargin       : int // 100
        minpx           : int // 50
        LINE_ST_TH      : int // 100 //px
        TRANS_PB_TH     : int // 150 should be greater than LINE_ST_TH
        clrThB          : int // 170
        clrThG          : int // 170
        clrThR          : int // 170
        grdThL          : int // 60
        grdThU          : int // 100
    }
    with
    static member  Default =
        {
            nWindows        = 9
            winMargin       =  100
            minpx           =  50
            LINE_ST_TH      =  50 //px
            TRANS_PB_TH     =  100
            clrThB          =  170
            clrThG          =  170
            clrThR          =  170
            grdThL          =  60
            grdThU          =  100
        }
