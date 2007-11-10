Public Class TSettings

    'Background 
    Private FBigSky As Boolean
    Private FSea As Boolean
    Private FMountains As Boolean
    Private FHills As Boolean
    Private FIndistinct As Boolean

    'Midground 
    Private FMountainous As Boolean
    Private FRollingHills As Boolean
    Private FPrairie As Boolean
    Private FLocalArea As Boolean
    Private FHazy As Boolean

    'Foreground
    Private FLake As Boolean
    Private FRiver As Boolean
    Private FRoad As Boolean
    Private FTrees As Boolean
    Private FWaterFall As Boolean
    Private FBuildings As Boolean
    Private FFlowers As Boolean
    Private FFarmStuff As Boolean
    Private FNil As Boolean

    'Sky
    Private FClear As Boolean
    Private FClouds As Boolean
    Private FOvercast As Boolean

    'Season 
    Private FSpring As Boolean
    Private FSummer As Boolean
    Private FFall As Boolean
    Private FWinter As Boolean

    'Options 
    Private FFog As Boolean
    Private FSunset As Boolean
    Private FOutlines As Boolean
    Private FNeither As Boolean

    Private FValueDominance As Boolean
    Private FShapeDominance As Boolean
    Private FLineDominance As Boolean
    Private FIntensityDominance As Boolean
    Private Fforeground As Boolean
    Private FScheme As Integer
    Private FDominantColor As Integer
    Private FColours(14) As Integer
    Private FTonalComposition As Integer
    Private FDesign As Integer
    Private FCanvasHeight As Integer
    Private FCanvasWidth As Integer

    'ColorTest'
    Private FTintsShades As Boolean
    Private FCMB As Boolean
    Private FNtr As Boolean 'Neither

    Private Fshanum As Integer
    Private Fdarktone As Integer
    Private Fdarkcolor As Integer
    Private Flighttone As Integer
    Private Flightcolor As Integer

    'Intensity
    Private FIntensity As String
    Private FIntense As Integer
    Private FFullRange As Boolean
    Private FHigh As Boolean
    Private FMedium As Boolean
    Private FLow As Boolean

    'Values design
    Private Ftonalcomp As String
    Private FLMD As Boolean
    Private FLDM As Boolean
    Private FMLD As Boolean
    Private FMDL As Boolean
    Private FDML As Boolean
    Private FDLM As Boolean

    'Sun Placement
    Private FLeft As Boolean
    Private FAbove As Boolean
    Private FRight As Boolean
    Private FAmbient As Boolean
    Private FEvening As Boolean

    ' random seeds
    Private FMix As String
    Private FObjectSeed As Integer
    Private FColorSeed As Integer
    Private FSubject As String
    Private Fside As Integer
    Property side() As Integer
        Get
            Return Fside
        End Get
        Set(ByVal value As Integer)
            Fside = value
        End Set
    End Property
    Property darktone() As Integer
        Get
            Return Fdarktone
        End Get
        Set(ByVal value As Integer)
            Fdarktone = value
        End Set
    End Property
    Property darkcolor() As Integer
        Get
            Return Fdarkcolor
        End Get
        Set(ByVal value As Integer)
            Fdarkcolor = value
        End Set
    End Property
    Property lighttone() As Integer
        Get
            Return Flighttone
        End Get
        Set(ByVal value As Integer)
            Flighttone = value
        End Set
    End Property
    Property lightcolor() As Integer
        Get
            Return Flightcolor
        End Get
        Set(ByVal value As Integer)
            Flightcolor = value
        End Set
    End Property
    Property tonalcomp() As String
        Get
            Return Ftonalcomp
        End Get
        Set(ByVal value As String)
            Ftonalcomp = value
        End Set
    End Property
 
    Property BigSky() As Boolean
        Get
            Return FBigSky
        End Get
        Set(ByVal value As Boolean)
            FBigSky = value
        End Set
    End Property
    Property Sea() As Boolean
        Get
            Return FSea
        End Get
        Set(ByVal value As Boolean)
            FSea = value
        End Set
    End Property
    Property Mountains() As Boolean
        Get
            Return FMountains
        End Get
        Set(ByVal value As Boolean)
            FMountains = value
        End Set
    End Property
 
    Property Hills() As Boolean
        Get
            Return FHills
        End Get
        Set(ByVal value As Boolean)
            FHills = value
        End Set
    End Property
    Property Indistinct() As Boolean
        Get
            Return FIndistinct
        End Get
        Set(ByVal value As Boolean)
            FIndistinct = value
        End Set
    End Property
    Property Mountainous() As Boolean
        Get
            Return FMountainous
        End Get
        Set(ByVal value As Boolean)
            FMountainous = value
        End Set
    End Property

    Property RollingHills() As Boolean
        Get
            Return FRollingHills
        End Get
        Set(ByVal value As Boolean)
            FRollingHills = value
        End Set
    End Property
    Property Prairie() As Boolean
        Get
            Return FPrairie
        End Get
        Set(ByVal value As Boolean)
            FPrairie = value
        End Set
    End Property

    Property Lake() As Boolean
        Get
            Return FLake
        End Get
        Set(ByVal value As Boolean)
            FLake = value
        End Set
    End Property
    Property River() As Boolean
        Get
            Return FRiver
        End Get
        Set(ByVal value As Boolean)
            FRiver = value
        End Set
    End Property
    Property LocalArea() As Boolean
        Get
            Return FLocalArea
        End Get
        Set(ByVal value As Boolean)
            FLocalArea = value
        End Set
    End Property
    Property Hazy() As Boolean
        Get
            Return FHazy
        End Get
        Set(ByVal value As Boolean)
            FHazy = value
        End Set
    End Property
  
    Property Road() As Boolean
        Get
            Return FRoad
        End Get
        Set(ByVal value As Boolean)
            FRoad = value
        End Set
    End Property
    Property Waterfall() As Boolean
        Get
            Return FWaterFall
        End Get
        Set(ByVal value As Boolean)
            FWaterFall = value
        End Set
    End Property
    Property Buildings() As Boolean
        Get
            Return FBuildings
        End Get
        Set(ByVal value As Boolean)
            FBuildings = value
        End Set
    End Property
    Property Farmstuff() As Boolean
        Get
            Return FFarmStuff
        End Get
        Set(ByVal value As Boolean)
            FFarmStuff = value
        End Set
    End Property
    Property Flowers() As Boolean
        Get
            Return FFlowers
        End Get
        Set(ByVal value As Boolean)
            FFlowers = value
        End Set
    End Property
    Property Nil() As Boolean
        Get
            Return FNil
        End Get
        Set(ByVal value As Boolean)
            FNil = value
        End Set
    End Property

    Property Clear() As Boolean
        Get
            Return FClear
        End Get
        Set(ByVal value As Boolean)
            FClear = value
        End Set
    End Property
    Property Clouds() As Boolean
        Get
            Return FClouds
        End Get
        Set(ByVal value As Boolean)
            FClouds = value
        End Set
    End Property
    Property Overcast() As Boolean
        Get
            Return FOvercast
        End Get
        Set(ByVal value As Boolean)
            FOvercast = value
        End Set
    End Property

    Property Spring() As Boolean
        Get
            Return FSpring
        End Get
        Set(ByVal value As Boolean)
            FSpring = value
        End Set
    End Property
    Property Summer() As Boolean
        Get
            Return FSummer
        End Get
        Set(ByVal value As Boolean)
            FSummer = value
        End Set
    End Property
    Property Fall() As Boolean
        Get
            Return FFall
        End Get
        Set(ByVal value As Boolean)
            FFall = value
        End Set
    End Property
    Property Winter() As Boolean
        Get
            Return FWinter
        End Get
        Set(ByVal value As Boolean)
            FWinter = value
        End Set
    End Property

    Property Fog() As Boolean
        Get
            Return FFog
        End Get
        Set(ByVal value As Boolean)
            FFog = value
        End Set
    End Property
    Property Sunset() As Boolean
        Get
            Return FSunset
        End Get
        Set(ByVal value As Boolean)
            FSunset = value
        End Set
    End Property

    Property Outlines() As Boolean
        Get
            Return FOutlines
        End Get
        Set(ByVal value As Boolean)
            FOutlines = value
        End Set
    End Property

    Property Neither() As Boolean
        Get
            Return FNeither
        End Get
        Set(ByVal value As Boolean)
            FNeither = value
        End Set
    End Property

    Property Cmb() As Boolean
        Get
            Return FCmb
        End Get
        Set(ByVal value As Boolean)
            FCmb = value
        End Set
    End Property
    Property TintsShades() As Boolean
        Get
            Return FTintsShades
        End Get
        Set(ByVal value As Boolean)
            FTintsShades = value
        End Set
    End Property
    Property Ntr() As Boolean
        Get
            Return FNtr
        End Get
        Set(ByVal value As Boolean)
            FNtr = value
        End Set
    End Property
    Property shanum() As Integer
        Get
            Return Fshanum
        End Get
        Set(ByVal value As Integer)
            Fshanum = value
        End Set
    End Property
    Property left() As Boolean
        Get
            Return FLeft
        End Get
        Set(ByVal value As Boolean)
            FLeft = value
        End Set
    End Property
    Property Above() As Boolean
        Get
            Return FAbove
        End Get
        Set(ByVal value As Boolean)
            FAbove = value
        End Set
    End Property
    Property Right() As Boolean
        Get
            Return FRight
        End Get
        Set(ByVal value As Boolean)
            FRight = value
        End Set
    End Property
    Property Ambient() As Boolean
        Get
            Return FAmbient
        End Get
        Set(ByVal value As Boolean)
            FAmbient = value
        End Set
    End Property
    Property Evening() As Boolean
        Get
            Return FEvening
        End Get
        Set(ByVal value As Boolean)
            FEvening = True
        End Set
    End Property
    Property Intensity() As String
        Get
            Return FIntensity
        End Get
        Set(ByVal value As String)
            FIntensity = value
        End Set
    End Property
    Property FullRange() As Boolean
        Get
            Return FFullRange
        End Get
        Set(ByVal value As Boolean)
            FFullRange = value
        End Set
    End Property
    Property High() As Boolean
        Get
            Return FHigh
        End Get
        Set(ByVal value As Boolean)
            FHigh = value
        End Set
    End Property
    Property Medium() As Boolean
        Get
            Return FMedium
        End Get
        Set(ByVal value As Boolean)
            FMedium = value
        End Set
    End Property
    Property Low() As Boolean
        Get
            Return FLow
        End Get
        Set(ByVal value As Boolean)
            FLow = value
        End Set
    End Property
    Property Mix() As String
        Get
            Return FMix
        End Get
        Set(ByVal value As String)
            FMix = value
        End Set
    End Property
    Property subject() As String
        Get
            Return FSubject
        End Get
        Set(ByVal value As String)
            FSubject = value
        End Set
    End Property
  
    Property TonalComposition() As Boolean
        Get
            Return FTonalComposition
        End Get
        Set(ByVal value As Boolean)
            FTonalComposition = value
        End Set
    End Property
    Property LMD() As Boolean
        Get
            Return FLMD
        End Get
        Set(ByVal value As Boolean)
            FLMD = value
        End Set
    End Property
    Property LDM() As Boolean
        Get
            Return FLDM
        End Get
        Set(ByVal value As Boolean)
            FLDM = value
        End Set
    End Property
    Property MLD() As Boolean
        Get
            Return FMLD
        End Get
        Set(ByVal value As Boolean)
            FMLD = value
        End Set
    End Property
    Property MDL() As Boolean
        Get
            Return FMDL
        End Get
        Set(ByVal value As Boolean)
            FMDL = value
        End Set
    End Property
    Property DML() As Boolean
        Get
            Return FDML
        End Get
        Set(ByVal value As Boolean)
            FDML = value
        End Set
    End Property
    Property DLM() As Boolean
        Get
            Return FDLM
        End Get
        Set(ByVal value As Boolean)
            FDLM = value
        End Set
    End Property
    Property CanvasHeight() As Integer
        Get
            Return FCanvasHeight
        End Get
        Set(ByVal value As Integer)
            FCanvasHeight = value
        End Set
    End Property

    Property CanvasWidth() As Integer
        Get
            Return FCanvasWidth
        End Get
        Set(ByVal value As Integer)
            FCanvasWidth = value
        End Set
    End Property

    Property Scheme() As Integer
        Get
            Return FScheme
        End Get
        Set(ByVal value As Integer)
            FScheme = value
        End Set
    End Property

    Property Colours(ByVal index) As Integer
        Get
            Return FColours(index)
        End Get
        Set(ByVal value As Integer)
            FColours(index) = value
        End Set
    End Property

    Property DominantColor() As Integer
        Get
            Return FDominantColor
        End Get
        Set(ByVal value As Integer)
            FDominantColor = value
        End Set
    End Property

    Property ObjectSeed() As Integer
        Get
            Return FObjectSeed
        End Get
        Set(ByVal value As Integer)
            FObjectSeed = value
        End Set
    End Property

    Sub NewObjectSeed()
        FObjectSeed = Rnd() * 1000000
    End Sub

    Property ColorSeed() As Integer
        Get
            Return FColorSeed
        End Get
        Set(ByVal value As Integer)
            FColorSeed = value
        End Set
    End Property

    Sub NewColorSeed()
        FColorSeed = Rnd() * 1000000
    End Sub
End Class
