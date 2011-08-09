Public Class TSettings

   
    'Landscape 
    Private FBigSky As Boolean
    Private FSea As Boolean

    'Landmass
    Private FMountainous As Boolean
    Private FHills As Boolean
    Private FFlat As Boolean

    'Landscape Focus
    Private FRiver As Boolean
    Private FLake As Boolean
    Private FStream As Boolean
    Private FRoad As Boolean
    Private FTrees As Boolean

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
    Private FMist As Boolean
    Private FOutlines As Boolean
    Private FEvening As Boolean
    Private FCanvasHeight As Integer
    Private FCanvasWidth As Integer

    'ColorTest'
    Private FTintsShades As Boolean
    Private FCMB As Boolean
    Private FNeither As Boolean

    'projects
    Private FProject1 As Boolean
    Private FProject2 As Boolean
    Private FProject3 As Boolean
    Private FProject4 As Boolean

    'Sun Placement
    Private FLeft As Boolean
    Private FAbove As Boolean
    Private FRight As Boolean

    Private FFarm As Boolean
    Private FHome As Boolean
    Private FFlowers As Boolean
    Private FNil As Boolean
    Private FNone As Boolean
    Private FNor As Boolean
    Private FLessIntensity As Boolean
    Private FNeartrees As Boolean
    Private fcolorcode As Boolean
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
    Property bigsky() As Boolean
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
    Property Mountainous() As Boolean
        Get
            Return FMountainous
        End Get
        Set(ByVal value As Boolean)
            FMountainous = value
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
    Property Flat() As Boolean
        Get
            Return FFlat
        End Get
        Set(ByVal value As Boolean)
            FFlat = value
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
    Property Stream() As Boolean
        Get
            Return FStream
        End Get
        Set(ByVal value As Boolean)
            FStream = value
        End Set
    End Property
    Property Mist() As Boolean
        Get
            Return FMist
        End Get
        Set(ByVal value As Boolean)
            FMist = value
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
    Property Trees() As Boolean
        Get
            Return FTrees
        End Get
        Set(ByVal value As Boolean)
            FTrees = value
        End Set
    End Property
    Property Home() As Boolean
        Get
            Return FHome
        End Get
        Set(ByVal value As Boolean)
            FHome = value
        End Set
    End Property
    Property Farm() As Boolean
        Get
            Return FFarm
        End Get
        Set(ByVal value As Boolean)
            FFarm = value
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
    Property Nor() As Boolean
        Get
            Return FNor
        End Get
        Set(ByVal value As Boolean)
            FNor = value
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
    Property None() As Boolean
        Get
            Return FNone
        End Get
        Set(ByVal value As Boolean)
            FNone = value
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
    Property Evening() As Boolean
        Get
            Return FEvening
        End Get
        Set(ByVal value As Boolean)
            FEvening = True
        End Set
    End Property
    Property LessIntensity() As Boolean
        Get
            Return FLessIntensity
        End Get
        Set(ByVal value As Boolean)
            FLessIntensity = value
        End Set
    End Property
    Property Neartrees() As Boolean
        Get
            Return FNeartrees
        End Get
        Set(ByVal value As Boolean)
            FNeartrees = value
        End Set
    End Property
    Property project1() As Boolean
        Get
            Return FProject1
        End Get
        Set(ByVal value As Boolean)
            FProject1 = value
        End Set
    End Property
    Property Project2() As Boolean
        Get
            Return FProject2
        End Get
        Set(ByVal value As Boolean)
            FProject2 = value
        End Set
    End Property
    Property Project3() As Boolean
        Get
            Return FProject3
        End Get
        Set(ByVal value As Boolean)
            FProject3 = value
        End Set
    End Property
    Property Project4() As Boolean
        Get
            Return FProject4
        End Get
        Set(ByVal value As Boolean)
            FProject4 = value
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
    
    Property Colorcode() As Boolean
        Get
            Return fcolorcode
        End Get
        Set(ByVal value As Boolean)
            fcolorcode = value
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
