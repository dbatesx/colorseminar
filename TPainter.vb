Imports ColorWorkshop.modColors, ColorWorkshop.modCMBcolors

Public Class TPainter
    Private g As Graphics = Nothing
    Private FImage As Image = Nothing
    Private FSettings As TSettings = Nothing

    Private horizon As Integer
    Private cumuchoice As String = ""
    Private i, j As Integer
    Private streampts, curvepoints3, riverpts, strleftx, strlefty, _
    rbeachx, lbeachx, strrightx, riverbed_path, head, skybrush
    Private filename As String
    Private FObjectRandom As Random
    Private FColorRandom As Random
    '    Private FObjectSeed As Integer
    '    Private FColorSeed As Integer
    Private oceanpoint As Integer
    Dim stainedglasspen As New Pen(Color.Black, 1)
    Dim sidecolor


    Property Image() As Image
        Get
            Return FImage
        End Get
        Set(ByVal value As Image)
            FImage = value

            g = Graphics.FromImage(Image)
            g.SmoothingMode = Drawing2D.SmoothingMode.HighQuality
            ' g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias
            g.Clear(Color.White)
        End Set
    End Property

    Property Settings() As TSettings
        Get
            Return FSettings
        End Get
        Set(ByVal value As TSettings)
            FSettings = value
        End Set
    End Property

    'Property ObjectSeed() As Integer
    '    Get
    '        Return FObjectSeed
    '    End Get
    '    Set(ByVal value As Integer)
    '        FObjectSeed = value
    '    End Set
    'End Property

    'Property ColorSeed() As Integer
    '    Get
    '        Return FColorSeed
    '    End Get
    '    Set(ByVal value As Integer)
    '        FColorSeed = value
    '    End Set
    'End Property

    Sub New()
        MyBase.New()
        '        FObjectSeed = Rnd() * 1000000
        '        FColorSeed = Rnd() * 1000000
    End Sub

    Protected Overrides Sub Finalize()
        MyBase.Finalize()
    End Sub

    Sub Make____Nice____Painting()

        FObjectRandom = New Random(Settings.ObjectSeed)
        FColorRandom = New Random(Settings.ColorSeed)

        'trial_distanceformula()
        ' Dim Ax, Ay, distance, depth, portion As Integer
        'If True Then
      
        If Settings.BigSky = True Then
            horizon = randh(Image.Height / 2, Image.Height * 0.75)
        Else
            horizon = randh(50, Image.Height * 0.5)
        End If
        ' g.DrawLine(Pens.Blue, 0, horizon, Image.Width, horizon)



        If Settings.Cmb = True Then CMBcolordisplay() : Exit Sub
        If Settings.Other = True Then colordisplayreg() : Exit Sub

        '

        'makeellipsetrees()
        'makemidpines() 'pie shapes, good start
        'trialtrees()'bunched ellipses
        'evergreen()'wierd,wonderful
        'deciduous()'good start
        'makeevergreentree()'nothing
        'rivertrees(300, 200, 100, 200)

        'Exit Sub

        makeskycolor()
        ' makeskyevening()
        'trial_distanceformula()
        makeocean()

        'makeskyfluffclouds()
        makeskymistyclouds()
        ' makeskystriateclouds()
        makeskycumulusclouds2()
        makeskyfluffclouds()

        If Settings.Mountains = True Then makedistantmountains()
        ' Exit Sub

        If Settings.River = True Then
            makeriveraswinding()
            If Settings.Mountains = True Then
                makerivermountains(randh(15, 30))
            ElseIf Settings.Prairie = True Then
                makerivermountains(0)
            Else : makerivermountains(randh(7, 15))
            End If
            makerivertrees()
        End If

        If Settings.RollingHills = True Then
            'BUILD LANDSHAPE (begy,numdiv,yvar,grade,distance)
            Dim begy, numdiv, yvar, grade, typ, curv, dist As Integer
            begy = horizon + yrand(300)
            Dim i As Integer = yrand(3)
            For n As Integer = 0 To i
                dist = 700 - n * 100
                begy = begy + n * 10
                numdiv = randh(6, 10)
                typ = yrand(5)
                'typ = randh(3, 5)
                Select Case typ
                    Case 1 'FLATTISH LAND
                        yvar = yrand(4) : grade = 0.1 : curv = 0.1
                    Case 2 'SLOPE DOWN TO RIGHT
                        yvar = yrand(7) : grade = 3 : curv = 0.1
                    Case 3 'SHARPER SLOPE DOWN TO RIGHT
                        yvar = yrand(4) : grade = 10 : curv = 0.1
                    Case 4 'CURVE DOWN TO RIGHT            
                        yvar = yrand(6) : grade = 8 : curv = 3
                    Case 5  'DROP OFF 
                        yvar = yrand(5) : grade = 12 : curv = 6
                End Select
                If randsign() > 0 Then yvar = -yvar
                landshape(typ, begy, numdiv, yvar, grade, curv, dist)
            Next

        End If
        Dim ay, ax, depth, portion, distance As Single

        If Settings.Buildings = True Then
            If yrand(5) > 2 Then
                ay = randh(horizon, Image.Height - 90)
                ax = randh(60, Image.Width - 100)
                depth = Image.Height - horizon
                portion = ay - horizon
                distance = (1.0 - (portion * 5 / depth)) * 1000
                distance = randh(100, 300)
                makebuildings(distance, ax, ay, depth)
            Else
                'MAKE MULTIPLE BUILDLINGS
                depth = randh(1000, 3000)
                For ay = horizon - 100 To Image.Height Step 40
                    ' ay = horizon + 200
                    ax = randh(100, Image.Width - 100)
                    portion = ay - horizon
                    distance = (1.0 - (portion * 5 / depth)) * 1000
                    ' the smaller the depth, the deeper the building
                    ' distance = 100
                    makebuildings(distance, ax, ay, depth)
                Next
            End If
        End If

        ' makerivertrees()

        If Settings.Fog = True Then makemist()
        'makemidpines()
        ' makeskyfluffclouds()
        ' makemidpines()

        'treeheight()
        'makebezierspears()
        'makewaterfall()
        'trial()

        ' makepathpoints()
        ' CMBcolordisplay()
        ' Else

        ' trial()
        ' End If

    End Sub

    Private Sub vanishingpoints(ByVal x1, ByVal y1, ByVal angle1, ByRef LVP, ByRef RVP, ByRef LMP, ByRef RMP)

        Dim angle2 As Integer
        Dim LVPx, LVPy, LMPx, LMPy, RVPx, RVPy, RMPx, RMPy, x, y, angle3, angle4, angle5, angle6, angle7 As Single

        If angle1 = 0 Then angle1 = randh(1, 89)
        angles(angle1, x, y)
        ' g.DrawLine(Pens.Black, x1, y1, x1 - x, y1 - y)
        intersection(x1, y1, x1 - x, y1 - y, 0, horizon, Image.Width, horizon, LVPx, LVPy) 'get left vanishing point
        '  g.DrawString(CStr(angle1), New Font("verdana", 10), Brushes.Black, New RectangleF(x1 - x, y1 - y, 60, 30))
        angle2 = 90 - angle1
        angles(angle2, x, y)
        '  g.DrawLine(Pens.Black, x1, y1, x1 + x, y1 - y)
        intersection(x1, y1, x1 + x, y1 - y, 0, horizon, Image.Width, horizon, RVPx, RVPy) 'get right vanishing point
        angle3 = angle2 / 2
        angle4 = angle1 + angle3
        angles(angle4, x, y)
        ' g.DrawLine(Pens.Pink, x1, y1, x1 - x, y1 - y)
        angle5 = 80 - angle4
        angles(angle5, x, y)
        ' g.DrawLine(Pens.Pink, x1, y1, x1 + x, y1 - y)
        intersection(x1, y1, x1 + x, y1 - y, 0, horizon, Image.Width, horizon, RMPx, RMPy) 'get right measuring point
        angle6 = angle1 / 2 + angle2
        angles(angle6, x, y)
        'g.DrawLine(Pens.SkyBlue, x1, y1, x1 + x, y1 - y)
        angle7 = 80 - angle6
        angles(angle7, x, y)
        ' g.DrawLine(Pens.SkyBlue, x1, y1, x1 - x, y1 - y)
        Dim ad As Integer = 10
        intersection(x1, y1, x1 - x, y1 - y, 0, horizon, Image.Width, horizon, LMPx, LMPy) 'get left measuring point
        g.DrawLine(Pens.Blue, x1, y1, LVPx, LVPy)
        g.DrawLine(Pens.Blue, x1, y1, RVPx, RVPy)
        g.DrawLine(Pens.Green, x1, y1, LMPx, LMPy)
        g.DrawLine(Pens.Green, x1, y1, RMPx, RMPy)
        RVP = RVPx : RMP = RMPx : LVP = LVPx : LMP = LMPx

    End Sub
    Private Sub angles(ByVal n, ByRef x, ByRef y)
        Dim m, add As Single
        add = 0
        m = n
        If n > 45 Then n = 90 - n
        Dim h As Integer = n
        If h <> n Then n = h : add = 1
        x = 100.0 : y = 100.0

        If n = 1 Then x = 2
        If n = 1.5 Then x = 3
        If n = 2 Then x = 4
        If n = 2.5 Then x = 5
        If n = 3 Then x = 6
        If n = 3.5 Then x = 6.5
        If n = 4 Then x = 7
        If n = 4.5 Then x = 8
        If n = 5 Then x = 9
        If n = 5.5 Then x = 10
        If n = 6 Then x = 11
        If n = 6.5 Then x = 11.5
        If n = 7 Then x = 12
        If n = 7.5 Then x = 13
        If n = 8 Then x = 14
        If n = 8.5 Then x = 15
        If n = 9 Then x = 16
        If n = 9.5 Then x = 17
        If n = 10 Then x = 18
        If n = 10.5 Then x = 19
        If n = 11 Then x = 20
        If n = 11.5 Then x = 20.5
        If n = 12 Then x = 21
        If n = 12.5 Then x = 22
        If n = 13 Then x = 23
        If n = 13.5 Then x = 24
        If n = 14 Then x = 25
        If n = 14.5 Then x = 26
        If n = 15 Then x = 27
        If n = 15.5 Then x = 28
        If n = 16 Then x = 29
        If n = 16.5 Then x = 30
        If n = 17 Then x = 31
        If n = 17.5 Then x = 32
        If n = 18 Then x = 33
        If n = 18.8 Then x = 34
        If n = 19 Then x = 35
        If n = 19.5 Then x = 36
        If n = 20 Then x = 37
        If n = 20.5 Then x = 38
        If n = 21 Then x = 39
        If n = 21.5 Then x = 40
        If n = 22 Then x = 41
        If n = 22.5 Then x = 42
        If n = 23 Then x = 43
        If n = 23.5 Then x = 44
        If n = 24 Then x = 45
        If n = 24.5 Then x = 46
        If n = 25 Then x = 47
        If n = 25.5 Then x = 48
        If n = 26 Then x = 49
        If n = 26.5 Then x = 50
        If n = 27 Then x = 51
        If n = 27.5 Then x = 52
        If n = 28 Then x = 53
        If n = 28.5 Then x = 54.5
        If n = 29 Then x = 56
        If n = 29.5 Then x = 57
        If n = 30 Then x = 58
        If n = 30.5 Then x = 59
        If n = 31 Then x = 60
        If n = 31.5 Then x = 61.5
        If n = 32 Then x = 63
        If n = 32.5 Then x = 64
        If n = 33 Then x = 65
        If n = 33.5 Then x = 66.5
        If n = 34 Then x = 68
        If n = 34.5 Then x = 69
        If n = 35 Then x = 70
        If n = 35.5 Then x = 71.5
        If n = 36 Then x = 73
        If n = 36.5 Then x = 74
        If n = 37 Then x = 75
        If n = 37.5 Then x = 76.5
        If n = 38 Then x = 78
        If n = 38.5 Then x = 79.5
        If n = 39 Then x = 81
        If n = 39.5 Then x = 82.5
        If n = 40 Then x = 84
        If n = 40.5 Then x = 85.6
        If n = 41 Then x = 87
        If n = 41.5 Then x = 88.5
        If n = 42 Then x = 90
        If n = 42.5 Then x = 91.5
        If n = 43 Then x = 93
        If n = 43.5 Then x = 95
        If n = 44 Then x = 97
        If n = 44.5 Then x = 98.5
        If n = 45 Then x = 100
        If n = 45.5 Then x = 101.5

        If m > 45 Then y = x : x = 100


    End Sub
    Private Sub makemo__________()

    End Sub

    Private Sub makemoartylevels()

        Randomize()
        Dim h, addit, xbit, ybit, x(9), y(9), start As Integer
        Dim var As Single
        xbit = Image.Width / 5
        ybit = (Image.Height - horizon) / 6
        h = horizon - ybit
        addit = 15
        start = -100

        Dim pen As New Pen(Color.LightBlue, 50)
        ' g.DrawLine(pen, 0, horizon, Width, horizon)

        For j = 1 To 9 'do 7 lines across the picture
            addit += 10 'increase size of fluctuation
            For i = 0 To 7 'get 8 points along the width to draw a line
                x(i) = xbit * i + yrand(addit) + start
                var = randsign() * yrand(addit) ' get plus or minus value
                y(i) = h + ybit * j + var
            Next
            x(8) = x(7) : y(8) = Image.Height
            x(9) = 0 : y(9) = Image.Height
            Dim point0 As New Point(x(0), y(0)) : Dim point1 As New Point(x(1), y(1))
            Dim point2 As New Point(x(2), y(2)) : Dim point3 As New Point(x(3), y(3))
            Dim point4 As New Point(x(4), y(4)) : Dim point5 As New Point(x(5), y(5))
            Dim point6 As New Point(x(6), y(6)) : Dim point7 As New Point(x(7), y(7))
            Dim point8 As New Point(x(8), y(8)) : Dim point9 As New Point(x(9), y(9))
            Dim curvePoints As Point() = {point0, point1, point2, point3, point4, _
                   point5, point6, point7, point8, point9}
            Settings.Colours(0) = Settings.DominantColor
            If Settings.Scheme > 9 Then
                Dim light, colors As Integer
                Dim season As Integer = Settings.Scheme - 10
                light = c_randh(0, 2)
                Dim brush As New SolidBrush(Color.Orange)
                colors = c_randh(1, 14)
                brush.Color = ModCMBcolors.getCMBcolor(season, light, colors)
                g.FillClosedCurve(brush, curvePoints)
            Else
                Dim colsymb, domin As Integer
                Dim tension As Single = 1
                Dim brush As New SolidBrush(Color.Orange)
                Dim scheme As Integer = Settings.Scheme
                Dim colorchoice, colorlevel, grayness As Integer
                If Settings.Scheme > 4 Then domin = 1 Else domin = 2
                colsymb = c_randh(0, scheme * domin)
                If colsymb > Settings.Scheme Then colsymb = 0 'setting dominance through weighting
                colorchoice = Settings.Colours(colsymb)
                grayness = 3
                colorlevel = 9 - j
                brush.Color = modColors.getcolor(colorchoice, grayness, colorlevel)
                g.FillClosedCurve(brush, curvePoints)
            End If
            If j = 2 Then makeadistantlake()

            'g.FillPolygon(brush, curvePoints)
        Next

        ' path1.AddCurve(curvePoints)
        'g.FillPath(Brushes.Blue, path1)

        'g.DrawLines(Pens.Aquamarine, points())
        ' g.DrawCurve(New Pen(Color.Aquamarine, 1), curvePoints)

    End Sub
    Private Sub makesky__________()

    End Sub

    Private Sub makeskycolor()

        Dim sky As New Rectangle(0, 0, Image.Width, horizon + 50)

CMB:
        If Settings.Scheme > 9 Then
            Dim season As Integer = Settings.Scheme - 10
            Dim light1 As Integer = c_randh(0, 2)
            Dim colors1 As Integer = c_randh(1, 14)
            Dim skybrush As New SolidBrush(Color.Orange)
            Dim groundbrush As New SolidBrush(Color.Orange)
            ' paint basic sky color
            skybrush.Color = ModCMBcolors.getCMBcolor(season, light1, colors1)
            g.FillRectangle(skybrush, sky)

            ' paint light gradient in sky
            Dim Brush2 As New Drawing2D.LinearGradientBrush(sky, _
                    Color.FromArgb(10, 255, 255, 255), _
                    Color.FromArgb(210, 255, 255, 255), _
                    Drawing2D.LinearGradientMode.Vertical)
            Dim a As Integer = 0
            If horizon < Image.Height / 3 Then a = 3 Else a = 2
            For n As Integer = 1 To a
                g.FillRectangle(Brush2, sky)
            Next

nonCMB:
        Else
            Dim colsymb, colsymb2, domin As Integer
            Dim brush As New SolidBrush(Color.Orange)
            Dim scheme As Integer = Settings.Scheme
            Dim colorchoice, colorchoice2, colorlevel, grayness As Integer
            If Settings.Scheme > 4 Then domin = 1 Else domin = 2
            colsymb = c_randh(0, scheme * domin)
            colsymb2 = c_randh(0, scheme)
            If colsymb > Settings.Scheme Then colsymb = 0 'setting dominance through weighting
            colorchoice = Settings.Colours(colsymb)
            colorchoice2 = Settings.Colours(colsymb2)
            grayness = 2
            colorlevel = 6

            Dim Brush2 As New Drawing2D.LinearGradientBrush(sky, _
            modColors.getcolor(colorchoice, grayness, colorlevel), _
            modColors.getcolor(colorchoice2, grayness, colorlevel), _
            Drawing2D.LinearGradientMode.Vertical)

            g.FillRectangle(Brush2, sky)

            Dim Brush0 As New Drawing2D.LinearGradientBrush(sky, _
            Color.FromArgb(10, 255, 255, 255), _
            Color.FromArgb(210, 255, 255, 255), _
            Drawing2D.LinearGradientMode.Vertical)
            Dim a As Integer = 0
            If horizon < Image.Height / 3 Then a = 3 Else a = 2
            For n As Integer = 1 To a
                g.FillRectangle(Brush0, sky)
            Next n
        End If
    End Sub
    Private Sub makeocean()
        'paint ocean
        paintdualcolors(0, horizon, Image.Width, Image.Height - horizon, 7, 1)
        If Settings.Outlines = True Then g.DrawLine(stainedglasspen, 0, horizon, Image.Width, horizon)
    End Sub
    Private Sub makeskymistyclouds()
        Dim x0, y0, x1, x2, x3, x4, y1, y2, y3, y4 As Integer
        'For n = 1 To 2
        x0 = yrand(Image.Width) - Image.Width
        y0 = yrand(Image.Height - horizon)
        x1 = yrand(Image.Width)
        y1 = yrand(Image.Height - horizon)
        x2 = yrand(Image.Width)
        y2 = yrand(Image.Height - horizon)
        x3 = yrand(Image.Width)
        x3 = yrand(Image.Height - horizon)
        x4 = yrand(Image.Width)
        y4 = yrand(Image.Height - horizon)
        Dim mistyclouds As New Drawing2D.GraphicsPath
        Dim point0 As New Point(x0, y0)
        Dim point1 As New Point(x1, y1) : Dim point2 As New Point(x2, y2)
        Dim point3 As New Point(x3, y3) : Dim point4 As New Point(x4, y4)

        Dim curvePoints As Point() = {point0, point1, point2, point3, point4}
        Dim tension As Single = 3.0
        Dim mistybrush As New SolidBrush(Color.FromArgb(30, 255, 255, 255))
        g.FillClosedCurve(mistybrush, curvePoints)
        ' Next


    End Sub
    Private Sub makeskycumulusclouds()
        Dim cumclouds As New Drawing2D.GraphicsPath
        Dim x, y, x1, y1, x2, y2, h, diam As Integer
        h = Image.Height - (Image.Height - horizon)
        For m As Integer = 1 To 3  ' find cumulous centers
            x = randh(50, Image.Width - 100)
            y = randh(40, h)
            diam = 50
            For i As Integer = 10 To 30
                x1 = x + randsign() * yrand(diam)
                y1 = y + randsign() * yrand(diam - i)
                If y1 > y Then y1 = y
                x2 = randh(30, 60)
                'y2 = randh(20, 40)
                y2 = x2 * 0.5
                If y2 > y Then y2 = y
                Dim rect As New Rectangle(x1, y1, x2, y2)

                Dim Brush0 As New Drawing2D.LinearGradientBrush(rect, Color.FromArgb(255, _
                      (255), (255), (255)), Color.FromArgb(245, 210, 230, 200), _
                       Drawing2D.LinearGradientMode.Vertical)
                g.FillEllipse(Brush0, rect)
                cumclouds.AddEllipse(rect)

            Next
            'Dim stainedglasspen2 As New Pen(Color.White, 2)
            'If settings.outlines = true Then g.DrawPath(stainedglasspen2, cumclouds)
            ' makestriateclouds()
        Next

    End Sub
    Private Sub makeskycumulusclouds2()
        Dim cumclouds As New Drawing2D.GraphicsPath
        Dim x, y, x1, y1, h, yheight, xwidth, sendx, sendy As Integer
        Dim p, n, r1, r2 As Integer
        Dim v, d, height1, width1 As Single
        v = 1
        p = 800 'pixels per foot
        h = 50  'number to represent general thickness of clouds
        Dim shade
        shade = findcolor(2, 0)
        Dim red, green, blue, a As Integer
        red = shade.r
        green = shade.g
        blue = shade.b
        a = 40
        For n = 0 To 4
            ' take sky five sections at a time starting at horizon
            For j As Integer = 1 To 4 - n   'total no. of clouds increases towards horizon
                x = yrand(Image.Width)  'place first cloud bundle base
                r1 = horizon - 40 * n   'the extent of each level = 40
                r2 = r1 - 40
                y = randh(r2, r1 - 1)   ' -1 so that r2 is not 0 
                sendx = x : sendy = y
                d = 30000 / (horizon - y) ' a guessed distance in the sky
                height1 = v * h * p / d    ' height of individual cloud formation
                width1 = height1 * randh(13, 15) / 10
                If randsign() = 1 Then
                    Dim xbase, ybase As Integer 'place flat cloud underneath
                    xbase = x - randh(10, 20)
                    ybase = y
                    Dim base_rect As New Rectangle(xbase, ybase, width1 + 20, -8)
                    Dim base_brush As New SolidBrush(Color.FromArgb(250, red, green, blue))
                    g.FillEllipse(base_brush, base_rect)
                    base_brush.Color = Color.FromArgb(190, 250, 250, 250)
                    g.FillEllipse(base_brush, base_rect)
                    'cumclouds.AddEllipse(base_rect)
                End If
                'If Settings.Outlines = True Then
                '    Dim stainedglasspen2 As New Pen(Color.LightGray, 1)
                '    g.DrawPath(stainedglasspen2, cumclouds)
                'End If
                Dim cutoff_rect As New Rectangle(0, y - 12, Image.Width, 200) 'exclude area just below bottom
                g.SetClip(cutoff_rect, Drawing2D.CombineMode.Exclude)

                For k As Integer = 2 To 0 Step -1  ' 3 decks of 5 subclouds each, starting with top
                    Dim nc As Integer = 10
                    If k = 0 Then nc = 15
                    For i As Integer = 1 To nc 'little clouds making up a deck
                        x1 = x + yrand(width1 / 2) 'beginning points
                        If k = 0 Then x1 = randh(x - 40, x + width1 - 20)
                        y1 = y + y / 3 * k
                        If y1 > y Then y1 = y 'for lower clouds in bottom cloud deck 
                        xwidth = randh(width1 * 0.3, width1 * 0.5)
                        If k = 0 Then xwidth = randh(width1 * 0.1, width1 * 0.2)
                        If xwidth < 2 Then xwidth = 2
                        yheight = xwidth * randh(3, 13) / 10
                        Dim rect As New Rectangle(x1, y1, xwidth, -yheight)

                        Dim c As Integer = 30
                        Dim Brush0 As New Drawing2D.LinearGradientBrush(rect, _
                        Color.FromArgb(c, red, green, blue), _
                        Color.FromArgb(255, 255, 255, 250), _
                        Drawing2D.LinearGradientMode.Vertical)

                        g.FillEllipse(Brush0, rect)
                        'cumclouds.AddEllipse(rect)


                    Next i
                    g.ResetClip()

                Next k

            Next j
            '  makeskyfluffcover(sendx, sendy, width1, -height1)
        Next n

    End Sub
    Private Sub rivertrees(ByVal x, ByVal y, ByVal width, ByVal height)

        height = height * randh(40, 100) / 100
        Dim rect As New Rectangle(x, y, width, height)

        'SET UP PATH TO INCLUDE SEVERAL ARCS AROUND THE EDGE OF THE RECTANGLE
        Dim n, arcbegin, arclength As Integer
        Dim xt, yt, w, startx, starty, rectwid, recthgt, strx, stry As Single
        Dim arcs_path As New Drawing2D.GraphicsPath
        w = width / 3

        'MAKE eight ARCS WITHIN THE AREA OF THE RECTANGLE
        Dim r As Integer = yrand(8)
        For n = 2 To 9 Step 1
            xt = yrand(width / 20)
            yt = yrand(height / 20)
            rectwid = width / randh(2, 3)
            If rectwid < 1 Then Return
            recthgt = height / randh(2, 3)
            arcbegin = 90
            arclength = randh(120, 140)

            If n = 1 Or n = 7 Or n = 8 Then startx = x + randsign() * yrand(xt) : arcbegin = randh(180, 190)
            If n = 3 Or n = 4 Or n = 5 Then startx = x + 2 * w + yrand(xt) : arcbegin = 270
            If n = 2 Or n = 6 Then startx = x + width * 0.3 + randsign() * xt
            If n = 1 Or n = 2 Or n = 3 Then starty = y - (xt) * 2
            If n = 5 Or n = 6 Or n = 7 Then starty = y + height * 0.55
            If n = 4 Or n = 8 Then starty = y + height * 0.2 + yrand(yt)
            If n = 1 Then strx = startx : stry = starty
            If n = 2 Then strx = startx : stry = starty : arcbegin = yrand(360)
            If n = 9 Then startx = strx : starty = stry
            If n = 10 Then startx = strx : starty = stry
            If n = 6 Then arcbegin = 0

            If n <> r Then arcs_path.AddArc(startx, starty, rectwid, recthgt, arcbegin, arclength)
            'g.DrawArc(Pens.Black, startx, starty, rectwid, recthgt, arcbegin, arclength)

        Next n
        g.SmoothingMode = Drawing2D.SmoothingMode.HighQuality
        g.SetClip(arcs_path)
        paintdualcolors(arcs_path.GetBounds.X, arcs_path.GetBounds.Y, arcs_path.GetBounds.Width, _
        arcs_path.GetBounds.Height, 1, 1)
        Dim stainpen As New Pen(Color.Black, 3)
        If Settings.Outlines = True Then g.DrawPath(stainpen, arcs_path)
        g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias

        g.ResetClip()

    End Sub
    Private Sub landshape(ByVal typ, ByVal begy, ByVal numdiv, ByVal yvar, ByVal grade, ByVal curv, ByVal distance)
        'ref from make__nice___painting
        Dim n, begx As Integer
        Dim div As Single
        Dim x(10), y(10)
        Dim landpath As New Drawing2D.GraphicsPath
        'begx = -randh(20, 40)
        begx = randh(-20, -40)
        x(0) = begx : y(0) = begy
        div = (Image.Width - x(0)) / numdiv
        For n = 1 To numdiv
            x(n) = div * n + randsign() * 10
            y(n) = y(n - 1) + grade * yrand(yvar) + curv * yrand(yvar)
            If typ = 1 Then y(n) = y(n) + randsign() * yrand(yvar)
        Next
        For i = numdiv + 1 To 10
            x(i) = x(numdiv)
            y(i) = y(numdiv)
        Next
        Dim land0 As New Point(begx, begy)
        Dim land1 As New Point(x(1), y(1))
        Dim land2 As New Point(x(2), y(2))
        Dim land3 As New Point(x(3), y(3))
        Dim land4 As New Point(x(4), y(4))
        Dim land5 As New Point(x(5), y(5))
        Dim land6 As New Point(x(6), y(6))
        Dim land7 As New Point(x(7), y(7))
        Dim land8 As New Point(x(8), y(8))
        Dim land9 As New Point(x(9), y(9))
        Dim land10 As New Point(x(10), y(10))
        Dim land11 As New Point(Image.Width + 250, y(10) + randsign() * yrand(7))
        Dim land12 As New Point(Image.Width + 250, Image.Height)
        Dim land13 As New Point(-10, Image.Height)
        Dim land14 As New Point(begx, begy)
        Dim land As Point() = {land0, land1, land2, land3, land4, land5, land6, _
        land7, land8, land9, land10, land11, land12, land13, land14}
        ' g.FillClosedCurve(Brushes.Coral, land)
        landpath = New Drawing2D.GraphicsPath
        landpath.AddCurve(land)
        g.SetClip(landpath)
        paintdualcolors(landpath.GetBounds.X, landpath.GetBounds.Y, landpath.GetBounds.Width, _
                landpath.GetBounds.Height, 3, 1)
        g.ResetClip()
        If Settings.Outlines = True Then g.DrawPath(stainedglasspen, landpath)

        ' If Settings.House = True Then
        Dim ax, ay, house As Integer
        house = randh(1, 10)
        ax = x(house)
        ay = y(house) + 25
        ' makebuildings(distance, ax, ay, depth)
        ' End If
        Dim treeheight, tw, tax, tay, h As Integer
        Dim th As Single
        treeheight = 40
        th = pixelheight(treeheight, distance)
        tw = randh(30, 50) / 100 * th
        tax = ax + randsign() * yrand(5)
        tay = ay - th '+ yrand(10)
        ' rivertrees(tax, tay, tw, th)
        h = pixelheight(1, distance)
        ' treetrunks(tax, tay, tw, th, h)
    End Sub

    Private Sub makeskystriateclouds()

        Dim addit, n As Integer
        addit = 11
        Dim x(9), y(9) As Integer
        For n = 1 To yrand(2)
            x(1) = yrand(Image.Width - 100)
            y(1) = randh(0, horizon - 50)
            x(2) = x(1) + randh(100, 600)
            y(2) = y(1) + randsign() * yrand(addit)
            x(3) = x(2) - yrand(10)
            y(3) = y(2) + yrand(addit)
            x(4) = x(3) + yrand(10)
            y(4) = y(3) + yrand(addit)
            x(5) = x(1) - randh(50, 120)     'back again
            y(5) = y(4) + randsign() * yrand(addit)
            x(6) = x(1) + randh(50, 90)
            y(6) = y(5) - (y(5) - y(1)) / 2
            x(7) = x(1)
            y(7) = y(1)
            x(0) = x(1) : y(0) = y(1)
            If n = 2 Then x(3) = x(2) * -1



            Dim point0 As New Point(x(0), y(0)) : Dim point1 As New Point(x(1), y(1))
            Dim point2 As New Point(x(2), y(2)) : Dim point3 As New Point(x(3), y(3))
            Dim point4 As New Point(x(4), y(4)) : Dim point5 As New Point(x(5), y(5))
            Dim point6 As New Point(x(6), y(6)) : Dim point7 As New Point(x(7), y(7))
            Dim point8 As New Point(x(8), y(8)) : Dim point9 As New Point(x(9), y(9))

            Dim curvePoints As Point() = {point0, point1, point2, point3, point4, _
                   point5, point6, point7}

            Settings.Colours(0) = Settings.DominantColor
            If Settings.Scheme > 9 Then    'If CMB scheme
                Dim light, colors As Integer
                Dim season As Integer = Settings.Scheme - 10
                light = 0
                Dim brush As New SolidBrush(Color.Orange)
                colors = c_randh(1, 14)
                brush.Color = ModCMBcolors.getCMBcolor(season, light, colors)
                g.FillClosedCurve(brush, curvePoints)

            Else   'If non CMB scheme
                Dim colsymb, domin As Integer
                Dim tension As Single = 1
                Dim brush As New SolidBrush(Color.Orange)
                Dim scheme As Integer = Settings.Scheme
                Dim colorchoice, colorlevel, grayness As Integer
                If scheme > 4 Then domin = 1 Else domin = 1.5
                colsymb = c_randh(0, scheme * domin)
                If colsymb > Settings.Scheme Then colsymb = 0 'setting dominance through weighting
                colorchoice = Settings.Colours(colsymb)
                grayness = 3
                colorlevel = 9 - n
                brush.Color = modColors.getcolor(colorchoice, grayness, colorlevel)
                g.FillClosedCurve(brush, curvePoints)

            End If
            If Settings.Outlines = True Then
                Dim stainedglasspen1 As New Pen(Color.SlateGray, 1)
                g.DrawClosedCurve(stainedglasspen1, curvePoints)
            End If
            '     'Dim Brush2 As New Drawing2D.LinearGradientBrush(drawlines, Color.FromArgb(20, _
            '(255), (255), (255)), Color.FromArgb(210, 255, 255, 255), _
            ' Drawing2D.LinearGradientMode.Vertical)
            '     g.FillClosedCurve(Brush2, curvePoints)
            '     'g.FillPolygon(brush, curvePoints)
            'Dim skybrush As New Drawing2D.LinearGradientBrush(sky, Color.FromArgb(100, _
            '(0), (0), (255)), _
            'Color.FromArgb(100, 255, 255, 0), _
            'Drawing2D.LinearGradientMode.Vertical)
            'g.FillRectangle(skybrush, sky)

        Next

    End Sub
    Private Sub makefillspace()
        Dim h, addit, xbit, ybit, x(9), y(9), start As Integer
        Dim var As Single
        Dim distant_path As New Drawing2D.GraphicsPath
        Dim accdistant_path As New Drawing2D.GraphicsPath
        xbit = Image.Width / 5
        ybit = (Image.Height - horizon) / 6
        h = horizon + ybit
        addit = 0
        start = -100

        Dim pen As New Pen(Color.LightBlue, 50)
        ' g.DrawLine(pen, 0, horizon, Image.Width, horizon)

        For j = 1 To 3 'do 3 lines across the picture
            addit += 1 'increase size of fluctuation
            For i = 0 To 7 'get 8 points along the width to draw a line
                x(i) = xbit * i + yrand(addit) + start
                var = randsign() * yrand(addit) ' get plus or minus value
                y(i) = h + ybit * j + var
            Next
            x(8) = x(7) : y(8) = Image.Height
            x(9) = 0 : y(9) = Image.Height
            Dim point0 As New Point(x(0), y(0)) : Dim point1 As New Point(x(1), y(1))
            Dim point2 As New Point(x(2), y(2)) : Dim point3 As New Point(x(3), y(3))
            Dim point4 As New Point(x(4), y(4)) : Dim point5 As New Point(x(5), y(5))
            Dim point6 As New Point(x(6), y(6)) : Dim point7 As New Point(x(7), y(7))
            Dim point8 As New Point(x(8), y(8)) : Dim point9 As New Point(x(9), y(9))
            Dim curvePoints As Point() = {point0, point1, point2, point3, point4, _
                   point5, point6, point7, point8, point9}
            distant_path = New Drawing2D.GraphicsPath
            distant_path.AddCurve(curvePoints)
            accdistant_path.AddCurve(curvePoints)

            g.SetClip(distant_path)
            paintdualcolors(distant_path.GetBounds.X, distant_path.GetBounds.Y, distant_path.GetBounds.Width, _
                       distant_path.GetBounds.Height, 2, 1)

            Dim a As Integer
            If Settings.Winter = True Then a = 240 Else a = 100
            Dim haze_brush As New SolidBrush(Color.FromArgb(a, 240, 240, 255))
            g.FillPath(haze_brush, accdistant_path)
            g.ResetClip()
            If Settings.Outlines = True Then g.DrawPath(stainedglasspen, distant_path)
        Next j


    End Sub

    Private Sub makedistantmountains()
        Dim mtptsx(32), mtptsy(32)
        Dim mtnpath As New Drawing2D.GraphicsPath
        Dim k, n, line_extent, mtndir, mtns, startx, starty As Integer
        line_extent = 4 ' extent limits of individual line lengths
        Do
            mtns += 1

            If mtns > 1 Then
                mtptsx(0) = randh(mtptsx(0), mtptsx(0) + 80)
            Else
                mtptsx(0) = randh(Image.Width / 2.5, Image.Width - 200)
            End If
            startx = mtptsx(0)

            If mtns > 1 Then
                mtptsy(0) = randh(mtptsy(0), mtptsy(0) + 30)
            Else
                mtptsy(0) = horizon + randh(4, 7)
            End If

            starty = mtptsy(0)

            j = 1 : k = 13
            mtndir = -1
            For n = 1 To 2

                For i = j To k
                    mtptsx(i) = mtptsx(i - 1) + randh(3, line_extent + 3)
                    mtptsy(i) = mtptsy(i - 1) + randh(0, line_extent) * mtndir
                    ' If mtptsy(i) > mtptsy(0) Then mtptsy(i) = mtptsy(0)
                Next i
                If n = 1 Then
                    mtptsx(14) = mtptsx(13) + line_extent + 4
                    mtptsy(14) = mtptsy(13) - 2
                    mtptsx(15) = mtptsx(14) + randh(40, 70)
                    mtptsy(15) = mtptsy(14) - 5
                    mtptsx(16) = mtptsx(14) + randh(80, 250)
                    mtptsy(16) = mtptsy(15) + 2
                End If

                'change direction of mountainsidex
                mtndir = 1
                j = 17 : k = 30
            Next n
            line_extent *= 1.6

            Dim point0 As New Point(mtptsx(0), mtptsy(0))
            Dim point1 As New Point(mtptsx(1), mtptsy(1))
            Dim point2 As New Point(mtptsx(2), mtptsy(2))
            Dim point3 As New Point(mtptsx(3), mtptsy(3))
            Dim point4 As New Point(mtptsx(4), mtptsy(4))
            Dim point5 As New Point(mtptsx(5), mtptsy(5))
            Dim point6 As New Point(mtptsx(6), mtptsy(6))
            Dim point7 As New Point(mtptsx(7), mtptsy(7))
            Dim point8 As New Point(mtptsx(8), mtptsy(8))
            Dim point9 As New Point(mtptsx(9), mtptsy(9))
            Dim point10 As New Point(mtptsx(10), mtptsy(10))
            Dim point11 As New Point(mtptsx(11), mtptsy(11))
            Dim point12 As New Point(mtptsx(12), mtptsy(12))
            Dim point13 As New Point(mtptsx(13), mtptsy(13))
            Dim point14 As New Point(mtptsx(14), mtptsy(14))
            Dim point15 As New Point(mtptsx(15), mtptsy(15))
            Dim point16 As New Point(mtptsx(16), mtptsy(16))
            Dim point17 As New Point(mtptsx(17), mtptsy(17))
            Dim point18 As New Point(mtptsx(18), mtptsy(18))
            Dim point19 As New Point(mtptsx(19), mtptsy(19))
            Dim point20 As New Point(mtptsx(20), mtptsy(20))
            Dim point21 As New Point(mtptsx(21), mtptsy(21))
            Dim point22 As New Point(mtptsx(22), mtptsy(22))
            Dim point23 As New Point(mtptsx(23), mtptsy(23))
            Dim point24 As New Point(mtptsx(24), mtptsy(24))
            Dim point25 As New Point(mtptsx(25), mtptsy(25))
            Dim point26 As New Point(mtptsx(26), mtptsy(26))
            Dim point27 As New Point(mtptsx(27), mtptsy(27))
            Dim point28 As New Point(mtptsx(28), mtptsy(28))
            Dim point29 As New Point(mtptsx(29), mtptsy(29))
            Dim point30 As New Point(mtptsx(30), mtptsy(30))
            Dim point31 As New Point(mtptsx(30), starty)
            Dim point32 = New Point(startx, starty)
            Dim curvepoints As Point() = {point0, point1, point2, point3, point4, _
            point5, point6, point7, point8, point9, point10, point11, point12, _
            point13, point14, point15, point16, point17, point18, point19, _
            point20, point21, point22, point23, point24, point25, point26, point27, _
            point28, point29, point30, point31}
            mtnpath = New Drawing2D.GraphicsPath
            mtnpath.AddClosedCurve(curvepoints, 0.3F)
            g.SetClip(mtnpath)
            Dim brush As New SolidBrush(findcolor(2, 0))
            g.FillPath(brush, mtnpath)
            g.ResetClip()
            If Settings.Outlines = True Then g.DrawPath(stainedglasspen, mtnpath)
            If mtptsx(17) > Image.Width Then Exit Do


        Loop

        'Dim mtnbrush As New SolidBrush(Color.Orange)
        'g.FillPath(mtnbrush, mtnpath)
    End Sub
    Private Sub makeadistantlake()
        Dim whiteline As New Pen(Color.FromArgb(255, 255, 255, 255))
        ' g.DrawLine(whiteline, 0, 200, 600, 199) '' ------------------------------LINEAR COLOR GRADIENTS
        Dim x, y, x1, y1 As Integer
        x = 0
        y = horizon + randh(30, 100)
        x1 = Image.Width
        y1 = y + yrand(100)

        dualcolorblend(x, y, x1, y1, 9, 1)
        'Dim lake As New Rectangle(x, y, x1, y1)
        'Dim lBrush As New Drawing2D.LinearGradientBrush(lake, Color.FromArgb(255, _
        'yrand(255), yrand(255), yrand(255)), _
        'Color.FromArgb(255, yrand(255), yrand(255), yrand(255)), _
        'Drawing2D.LinearGradientMode.Vertical)
        'g.FillRectangle(lBrush, lake)
        '---------------------------------------V--------------LAYING IN WATER
        'LAKE:
        '   Back lake line, smooth or rough surface, reflections, front line
        '   Edge around lake

        'Dim rect2 As New Rectangle(0, 200, 600, 30)
        'g.FillRectangle(New SolidBrush(Color.FromArgb(200, 20, 100, 250)), rect2)
        'Dim rect3 As New Rectangle(0, 250, 600, 50)
        'g.FillRectangle(New SolidBrush(Color.FromArgb(200, 20, 20, 250)), rect3)
        ' Dim rect1 As New Rectangle(0, 200, 576, 60)
        ' g.FillRectangle(New SolidBrush(Color.FromArgb(200, 30, 250, 170)), rect1)

        '-------------------------------------------------------adding land below 
        'Dim bottrect As New Rectangle(0, 260, 576, 260)
        'g.FillRectangle(New SolidBrush(Color.Gold), bottrect)

    End Sub


    Private Sub makebezierspears()
        '-------------------------------------------------------BEZIER CURVES
        Dim x0, x1, x2, x3, y0, y1, y2, y3 As Integer
        Dim spear As Drawing2D.GraphicsPath
        Dim brushy As New SolidBrush(Color.BurlyWood)
        For i As Integer = 1 To 6

            x0 = yrand(Image.Width)
            y0 = Image.Height

            For n As Integer = 1 To 5
                spear = New Drawing2D.GraphicsPath
                x1 = x0 + randh(2, 5)
                y1 = Image.Height

                x2 = x0 + randsign() * yrand(80)
                y2 = randh(300, 400)
                x3 = x1 + (x2 - x0) + yrand(5)
                y3 = y2

                spear.AddBezier(New Point(x1, y1), New Point(x0, y0), _
                New Point(x2, y2), New Point(x3, y3))

                g.FillPath(brushy, spear)
            Next
        Next
    End Sub

    Private Sub maketrunkpaths()

        Dim brushy As New SolidBrush(Color.Firebrick)

        Dim x(3), y(3) As Integer
        Dim trunk As Drawing2D.GraphicsPath

        For n As Integer = 1 To 3
            trunk = New Drawing2D.GraphicsPath
            x(0) = yrand(Image.Width)
            y(0) = 0

            x(1) = x(0) + randh(10, 20)
            y(1) = 0

            x(2) = x(0) + randsign() * yrand(100)
            y(2) = Image.Height

            x(3) = x(2) + (x(1) - x(0)) + yrand(5)
            y(3) = Image.Height

            Dim point0 As New Point(x(0), y(0))
            Dim point1 As New Point(x(1), y(1))
            Dim point2 As New Point(x(2), y(2))
            Dim point3 As New Point(x(3), y(3))

            Dim curvePoints As Point() = {point0, point1, point2, point3}
            Dim tension As Single = 3.0
            'g.FillClosedCurve(mistybrush, curvePoints)

            trunk.AddLine(point0, point1)          'FOR A TREE
            trunk.AddLine(point3, point2)
            g.FillPath(brushy, trunk)


            '-----------------------------------------SETTING A CLIP (FOR ABOVE REGION)
            'g.SetClip(trunk)
            'g.SetClip(trunk, Drawing2D.CombineMode.Exclude)
            'Dim rpen As New Pen(Color.Black, 4)
            'Dim tryout As New Rectangle(10, 113, 400, 100)
            'g.DrawRectangle(rpen, tryout)
            'g.ResetClip()
        Next
        Return
        '-----------------------------------------SETTING and testing A CLIP (FOR ABOVE REGION)
        'g.SetClip(trunk2)
        'g.SetClip(trunk)
        'Dim rpen As New Pen(Color.Black, 4)
        'Dim tryout As New Rectangle(10, 113, 400, 100)
        'g.DrawRectangle(rpen, tryout)
        'g.drawing2d.ExcludeClip(trunk2) '---------doesn't work
        'g.ResetClip()

    End Sub

    Private Sub evergreen()

        ' -----------------------------------------Attemping Bezier combo's as leaves
        Dim peni As New Pen(Color.Khaki, 1)
        Dim bezi As New Drawing2D.GraphicsPath()
        Dim bezi2 As New Drawing2D.GraphicsPath()
        Dim brush5 As New SolidBrush(Color.Green)
        Dim i As Integer = 0
        Dim j As Integer = 0
        Dim w As Integer = 160
        Dim cx2 As Integer = 60
        Dim cy2 As Integer = 70


        For j = 1 To 2
            For i = 1 To 3

                Dim x1 As Integer = yrand(w) + cx2
                Dim x2 As Integer = yrand(w) + cx2
                Dim x3 As Integer = yrand(w) + cx2
                Dim x4 As Integer = yrand(w) + cx2
                Dim y1 As Integer = yrand(w) + cy2
                Dim y2 As Integer = yrand(w) + cy2
                Dim y3 As Integer = yrand(w) + cy2
                Dim y4 As Integer = yrand(w) + cy2

                If j = 1 Then
                    bezi.AddBezier(New Point(x1, y1), New Point(x2, y2), _
            New Point(x3, y3), New Point(x4, y4))
                    g.FillPath(brush5, bezi)
                    g.DrawBezier(New Pen(Color.Khaki, 2), New Point(x1, y1), New Point(x2, y2), _
                    New Point(x3, y3), New Point(x4, y4))

                    bezi.Flatten()
                Else
                    bezi2.AddBezier(New Point(x1, y1), New Point(x2, y2), _
                                New Point(x3, y3), New Point(x4, y4))
                    g.FillPath(brush5, bezi2)
                    g.DrawBezier(New Pen(Color.Moccasin, 2), New Point(x1, y1), New Point(x2, y2), _
                     New Point(x3, y3), New Point(x4, y4))

                    'bezi2.Flatten()
                End If
            Next
            g.DrawPath(peni, bezi)
            cy2 = 220
        Next
        g.DrawPath(peni, bezi2)
    End Sub

    Private Sub deciduous()


        Dim wd, w, h, ht, x, y, n As Integer
        Dim foliage_x(12), foliage_y(12)

        x = 400
        y = 60
        w = 100
        h = 160
        wd = w / 9
        ht = h / 3
        Dim rect As New Rectangle(x, y, w, h)
        g.DrawRectangle(Pens.Plum, rect)
        For n = 0 To 9
            foliage_x(n) = x + n * wd
            foliage_y(n) = y + yrand(ht)
        Next
        Dim point0 As New Point(foliage_x(0), foliage_y(0))
        Dim point1 As New Point(foliage_x(1), foliage_y(1))
        Dim point2 As New Point(foliage_x(2), foliage_y(2))
        Dim point3 As New Point(foliage_x(3), foliage_y(3))
        Dim point4 As New Point(foliage_x(4), foliage_y(4))
        Dim point5 As New Point(foliage_x(5), foliage_y(5))
        Dim point6 As New Point(foliage_x(6), foliage_y(6))
        Dim point7 As New Point(foliage_x(7), foliage_y(7))
        Dim point8 As New Point(foliage_x(8), foliage_y(8))
        Dim point9 As New Point(foliage_x(9), foliage_y(9))
        Dim point10 As New Point(foliage_x(7), y + ht * 2)
        Dim point11 As New Point(x + w / 2, y + randh(2 * ht, h))
        Dim point12 As New Point(x, ht * 2)



        Dim leaves As Point() = {point0, point1, point2, point3, point4, point5, point6, _
       point7, point8, point9, point10, point11, point12}
        g.FillClosedCurve(Brushes.LightGreen, leaves, 0.2)
        ' g.DrawLines(Pens.Black, leaves)


    End Sub

    Private Sub logo()

        'make arcs
        Dim leaf_path As New Drawing2D.GraphicsPath
        Dim brush4 As New SolidBrush(Color.ForestGreen)

        Dim x, y, width, height, arcstart, arc_end As Integer

        For i = 1 To 4
            x = Image.Width / 4 + yrand(150)
            y = Image.Height / 7 + yrand(100)
            width = randh(30, 400)
            height = randh(30, 300)
            arcstart = yrand(yrand(360))
            arc_end = yrand(yrand(180))

            leaf_path.AddArc(x, y, width, height, arcstart, arc_end)

        Next i

        brush4.Color = findcolor(2, 0)
        g.FillPath(brush4, leaf_path)

    End Sub

    Private Sub makeellipsetrees()
        Dim pntx, pnty, branch_angle As Single
        Dim path As New Drawing2D.GraphicsPath

        For i As Integer = 0 To 10
            Dim j As Integer
            Dim height As Integer = 200
            Dim startingx As Integer = 200 + i * 35
            Dim startingy As Integer = 200
            Dim startx As Integer = startingx
            Dim starty As Integer = startingy
            Dim width As Integer = 8

            pntx = startingx + yrand(width + 20)
            pnty = startingy + yrand(height / 3)
            Dim pt0 As New Point(pntx, pnty)
            Dim pt1 As New Point(pntx + yrand(30), pnty + yrand(30))
            Dim pt2 As New Point(pntx + randh(10, 40), pnty + randh(10, 40))
            Dim gap As Point() = {pt0, pt1, pt2}
            Dim brush As New SolidBrush(Color.Empty)
            brush.Color = findcolor(1, 1)
            ''g.SetClip(path, Drawing2D.CombineMode.Exclude)

            ' g.FillPath(brush, path)

            For j = 0 To 3
                Dim rect As New Rectangle(startingx, startingy, width, -height)
                Dim brush1 As New SolidBrush(Color.Empty)
                brush.Color = findcolor(1, 0)
                'g.FillEllipse(brush, rect)
                startingx -= 10


                branch_angle = randh(-10, 10)
                path.AddEllipse(rect)
                Rotate(startx + startx / 2, starty + height, branch_angle)
                'Rotate(xbegin1 + xwidth1 / 2, ybegin1 + yheight1 / 2, branch_angle)
                ' g.FillPie(brush1, rect, 100, 180)
                ' paintdualcolors(startingx, startingy, width, height, 5, 2)

                RotateBack(startx + startx / 2, starty + height, branch_angle)
                ' g.FillPolygon(brush, gap)

                width = randh(20, 60)
                ' width = width + randh(10, 25)
                height = height * (0.9)
            Next
        Next



    End Sub
    Private Sub makelake()
        '' -------------------------------------------------LINEAR COLOR GRADIENTS
        If Settings.Scheme > 9 Then
            Dim rect5 As New Rectangle(50, 30, 200, 200) ' a cerulean blue
            Dim lBrush As New Drawing2D.LinearGradientBrush(rect5, Color.FromArgb(230, _
           yrand(255), yrand(255), yrand(255)), _
            Color.FromArgb(100, yrand(255), yrand(255), yrand(255)), _
            Drawing2D.LinearGradientMode.BackwardDiagonal)
            g.FillRectangle(lBrush, rect5)
        End If
    End Sub
    Private Sub makemiddleground()
        'MIDGROUND:
        '   road
        '   river,or creek, 
        '   Hill arc orientation
        '   fields
        '   cliffs, tree areas

        'STRUCTURES
        '   fences, barns, posts, flowered areas, stone walls
        'NEAR MIDGROUND
        '   stream details - turbulence, rocks, reflections, curvature, sweep
        '   waterfall - size, top, bottom 
        '       walls and rockwork at sides, bottom
        '   trees, deciduous and pine
        '   structures

    End Sub
    Private Sub makeriver__________()

    End Sub

    Private Sub makeriveraswinding()
        Dim a, b, sparkley1, sparkley2, sparklex1, sparklex2, rx, strleftx(20), strrighty(20), strrightx(20), strlefty(20), _
              streamx(4), startx(4), starty(4), riverwidth, streamy(4), divHeight As Integer
        Dim rbeachx(20), rbeachy(20), lbeachx(20), lbeachy(20)
        Dim level, levelkey, h, lv, sidesway, addonsw As Single
        Dim river_path As New Drawing2D.GraphicsPath
        Dim riverbed_path As New Drawing2D.GraphicsPath
        Dim beachkey As Integer = randh(5, 12)
        riverwidth = randh(1, 200)

        oceanpoint = 0
        'total number of divisions below horizon = 6
        divHeight = (Image.Height - horizon) / 6
        'determine approximate no. divisions left below beginning point of stream
        startx(3) = randh(Image.Width * 0.4, Image.Width * 0.7)
        'starty(3) = horizon + randh(10, 30)
        starty(3) = horizon + randh(35, 60)
        Dim ystart As Single = starty(3)

        'restore beach color if ocean not wanted
        If oceanpoint < 1 Then
            paintdualcolors(0, ystart, Image.Width, Image.Height, 8, 1)
        End If

        level = 7 - ((starty(3) - horizon + 1) / (Image.Height - horizon))
        lv = (7 - level) * 0.6
        h = lv ^ 2 '             height/width factor for perceived distance
        addonsw = h * riverwidth
        startx(4) = startx(3) - addonsw
        starty(4) = starty(3)
        levelkey = 0.25

        For j As Integer = 20 To 0 Step -1
            level -= levelkey
            h = ((7 - level) * 0.6) ^ 2
            addonsw = riverwidth * h
            rx = yrand(80)
            sidesway = randsign() * rx

            streamx(1) = startx(4)
            streamy(1) = starty(4)
            streamx(2) = startx(3)
            streamy(2) = starty(3)
            streamx(3) = streamx(2) + sidesway
            streamy(3) = streamy(2) + h * divHeight * levelkey
            streamx(4) = streamx(3) - addonsw
            If streamx(4) = streamx(3) Then streamx(4) -= 1
            streamy(4) = streamy(3)
            i += 1

            For n As Integer = 3 To 4
                startx(n) = streamx(n) : starty(n) = streamy(n)
            Next n

            strrightx(j) = streamx(3)
            strrighty(j) = streamy(3)
            strleftx(j) = streamx(4)
            strlefty(j) = streamy(4)

            ' add beach points into memory
            rbeachx(j) = strrightx(j) + beachkey * h
            rbeachy(j) = streamy(3)
            lbeachx(j) = strleftx(j) - beachkey * h
            lbeachy(j) = streamy(4)

        Next j

        For riverwidth2 As Integer = 1 To 2
            'follow river edge down on left side then up on right side for best computer line curvature
            Dim point0 As New Point(strleftx(0), strlefty(0))
            Dim point1 As New Point(strleftx(1), strlefty(1))
            Dim point2 As New Point(strleftx(2), strlefty(2))
            Dim point3 As New Point(strleftx(3), strlefty(3))
            Dim point4 As New Point(strleftx(4), strlefty(4))
            Dim point5 As New Point(strleftx(5), strlefty(5))
            Dim point6 As New Point(strleftx(6), strlefty(6))
            Dim point7 As New Point(strleftx(7), strlefty(7))
            Dim point8 As New Point(strleftx(8), strlefty(8))
            Dim point9 As New Point(strleftx(9), strlefty(9))
            Dim point10 As New Point(strleftx(10), strlefty(10))
            Dim point11 As New Point(strleftx(11), strlefty(11))
            Dim point12 As New Point(strleftx(12), strlefty(12))
            Dim point13 As New Point(strleftx(13), strlefty(13))
            Dim point14 As New Point(strleftx(14), strlefty(14))
            Dim point15 As New Point(strleftx(15), strlefty(15))
            Dim point16 As New Point(strleftx(16), strlefty(16))
            Dim point17 As New Point(strleftx(17), strlefty(17))
            Dim point18 As New Point(strleftx(18), strlefty(18))
            Dim point19 As New Point(strleftx(19), strlefty(19))
            Dim point20 As New Point(strleftx(20), strlefty(20))

            Dim point21 As New Point(strrightx(0), strrighty(0))
            Dim point22 As New Point(strrightx(1), strrighty(1))
            Dim point23 As New Point(strrightx(2), strrighty(2))
            Dim point24 As New Point(strrightx(3), strrighty(3))
            Dim point25 As New Point(strrightx(4), strrighty(4))
            Dim point26 As New Point(strrightx(5), strrighty(5))
            Dim point27 As New Point(strrightx(6), strrighty(6))
            Dim point28 As New Point(strrightx(7), strrighty(7))
            Dim point29 As New Point(strrightx(8), strrighty(8))
            Dim point30 As New Point(strrightx(9), strrighty(9))
            Dim point31 As New Point(strrightx(10), strrighty(10))
            Dim point32 As New Point(strrightx(11), strrighty(11))
            Dim point33 As New Point(strrightx(12), strrighty(12))
            Dim point34 As New Point(strrightx(13), strrighty(13))
            Dim point35 As New Point(strrightx(14), strrighty(14))
            Dim point36 As New Point(strrightx(15), strrighty(15))
            Dim point37 As New Point(strrightx(16), strrighty(16))
            Dim point38 As New Point(strrightx(17), strrighty(17))
            Dim point39 As New Point(strrightx(18), strrighty(18))
            Dim point40 As New Point(strrightx(19), strrighty(19))
            Dim point41 As New Point(strrightx(20), strrighty(20))

            Dim rivercurvePoints As Point() = {point20, point19, point18, point17, point16, _
            point15, point14, point13, point12, point11, point10, point9, point8, _
            point7, point6, point5, point4, point3, point2, point1, point0, _
            point21, point22, point23, point24, point25, point26, point27, point28, _
            point29, point30, point31, point32, point33, point34, point35, point36, _
            point37, point38, point39, point40, point41}
            river_path = New Drawing2D.GraphicsPath
            ' beach_path
            river_path.AddCurve(rivercurvePoints, 0.3)       'river_path includes 42 input points
            If riverwidth2 = 1 Then
                riverbed_path = New Drawing2D.GraphicsPath
                riverbed_path.AddCurve(rivercurvePoints)
                g.SetClip(river_path)
                paintdualcolors(0, ystart, Image.Width, Image.Height - ystart, 9, 1)
                g.ResetClip()

                If Settings.Outlines = True Then
                    stainedglasspen.Alignment = Drawing2D.PenAlignment.Outset
                    g.DrawPath(stainedglasspen, river_path)
                End If

                ' set up for adding the beach or treeline alongside the stream, by replacing points
                For n As Integer = 0 To 20
                    strrightx(n) = rbeachx(n)
                    strleftx(n) = lbeachx(n)
                Next
            Else
                '   set stream up for mountainsides extending from either side
                streampts = river_path.PathPoints() 'streampts includes 123 output curve points
                riverpts = river_path.PathPoints()  'both are used

            End If
        Next riverwidth2

        '               make sparkles on water
        g.SetClip(riverbed_path)
        head = streampts(0).y
        Dim pen As New Pen(Color.White, 1) 'needs to be sky color
        For p As Integer = 1 To 5
            For r As Integer = 1 To 70
                sparklex1 = randh(30, 500)
                a = head : b = head + 20 * p
                sparkley1 = randh(a, b)
                sparklex2 = sparklex1 + randh(2, 5)
                sparkley2 = sparkley1
                g.DrawLine(pen, sparklex1, sparkley1, sparklex2, sparkley2)
            Next
        Next p
        g.ResetClip()
        g.SetClip(river_path, Drawing2D.CombineMode.Exclude)

    End Sub
    Private Sub makerivermountains(ByVal rise)

        Dim n, z, v, vz, levelstartx, levelendx As Integer
        Dim initialx(8), initialy(8), xincr, ave As Single
        Dim hillcount_L, hillcount_R As Integer
        Dim acchill_path As New Drawing2D.GraphicsPath
        initialx(2) = 1 : initialy(2) = 1
        Dim riser As Single
        Dim direction As String = ""
        Dim q As Integer = 1
        For side As Integer = 1 To 2    'going down left side of river first, then down right 
            levelstartx = 0
            n = 1
            v = streampts(n).x : z = streampts(n + 1).x : vz = streampts(n - 1).x  ' take an average, test direction of river at source
            ave = (v + z) / 2
            If side = 1 Then        'goal, to have mountain go from cove to cove
                If ave < vz Then     'if starting from a point rather than a cove
                    Do While ave <= vz And n < 61
                        n += 1          'continue to center of cove
                        v = streampts(n).x : z = streampts(n + 1).x : vz = streampts(n - 1).x
                        If v = z Then z -= 1
                        ave = (v + z) / 2
                    Loop
                    levelendx = n
                    xincr = (streampts(levelstartx).x * 0.1428)   '/7
                    direction = "Right"
                End If
            End If
            If side = 2 Then        'coming down the right side, do reverse
                If ave > vz Then
                    Do While ave >= vz And n < 61
                        n += 1          'continue to center of cove
                        v = streampts(n).x : z = streampts(n + 1).x : vz = streampts(n - 1).x
                        If v = z Then z += 1
                        ave = (v + z) / 2
                    Loop
                    levelendx = n
                    xincr = (streampts(levelstartx).x * 0.1428)
                    direction = "Left"
                End If
            End If

Mainbody:   Do While streampts(n).y <= Image.Height + 10 And n < 61 ' find width of river inlet - left.  
                If direction <> "" Then GoTo processpoints

                xincr = (streampts(levelstartx).x * 0.1428)   '/7  - setting distance between x points
                If xincr < 50 Then xincr = 50

                If side = 1 Then
                    Do While ave >= vz And n < 61
                        n += 1          'continue to center of cove
                        v = streampts(n).x : z = streampts(n + 1).x : vz = streampts(n - 1).x
                        If v = z Then z += 1
                        ave = (v + z) / 2
                    Loop
                    Do While ave <= vz And n < 61
                        n += 1          'continue to center of cove
                        v = streampts(n).x : z = streampts(n + 1).x : vz = streampts(n - 1).x
                        If v = z Then z += 1
                        ave = (v + z) / 2
                    Loop
                    levelendx = n
                End If

                If side = 2 Then
                    Do While ave <= vz And n < 61
                        n += 1          'continue to center of cove
                        v = streampts(n).x : z = streampts(n + 1).x : vz = streampts(n - 1).x
                        If v = z Then z -= 1
                        ave = (v + z) / 2
                    Loop
                    Do While ave >= vz And n < 61
                        n += 1          'continue to center of cove
                        v = streampts(n).x : z = streampts(n + 1).x : vz = streampts(n - 1).x
                        If v = z Then z += 1
                        ave = (v + z) / 2
                    Loop
                    levelendx = n
                End If

processpoints:
                direction = ""
                riser = rise        'presently a minimum of 10 points
                Dim hill_path As New Drawing2D.GraphicsPath
                ' starting points for the intersecting mountain shape
                initialx(0) = streampts(levelstartx).x : initialy(0) = streampts(levelstartx).y
                Dim xrev As Integer = -10       'extend beyond the edges of the form
                If side = 2 Then xrev = Image.Width + 10
                For w As Integer = 1 To 7       ' q changes its direction, depending on the side
                    initialx(w) = streampts(levelstartx).x - (q * (xincr * w - randsign() * randh(2, 14)))
                    initialy(w) = initialy(w - 1) - randh(5, riser)
                    riser += randh(2, rise) ' the key to general terrain of river painting - level or mountainous
                Next
                Dim point0 As New Point(initialx(0), initialy(0))
                Dim point1 As New Point(initialx(1), initialy(1))
                Dim point2 As New Point(initialx(2), initialy(2))
                Dim point3 As New Point(initialx(3), initialy(3))
                Dim point4 As New Point(initialx(4), initialy(4))
                Dim point5 As New Point(initialx(5), initialy(5))
                Dim point6 As New Point(initialx(6), initialy(6))
                Dim point7 As New Point(initialx(7), initialy(7))
                If initialx(7) < 0 Then xrev = initialx(7)
                Dim point8 As New Point(xrev, initialy(7) - 5) 'Finish to outside of form
                Dim point9 As New Point(xrev, streampts(levelendx).y + 10) ' Drop down to level of target end
                Dim point10 As New Point(streampts(levelendx).x, streampts(levelendx).y) ' Return to target end
                Dim r(18) As Single

                Dim spread As Integer = levelendx - levelstartx
                Dim k As Single = spread / 18

                Dim ext As Integer = q * -2 'To bring the edge of the mountain tighter to the river
                For i = 1 To 17
                    r(i) = levelendx - k * i  ' filling in points for more accurate line
                Next
                Dim point11 As New Point(streampts(r(1)).x + ext, streampts(r(1)).y)
                Dim point12 As New Point(streampts(r(2)).x + ext, streampts(r(2)).y)
                Dim point13 As New Point(streampts(r(4)).x + ext, streampts(r(4)).y)
                Dim point15 As New Point(streampts(r(3)).x + ext, streampts(r(3)).y)
                Dim point14 As New Point(streampts(r(5)).x + ext, streampts(r(5)).y)
                Dim point16 As New Point(streampts(r(6)).x + ext, streampts(r(6)).y)
                Dim point17 As New Point(streampts(r(7)).x + ext, streampts(r(7)).y)
                Dim point18 As New Point(streampts(r(8)).x + ext, streampts(r(8)).y)
                Dim point19 As New Point(streampts(r(9)).x + ext, streampts(r(9)).y)
                Dim point20 As New Point(streampts(r(10)).x + ext, streampts(r(10)).y)
                Dim point21 As New Point(streampts(r(11)).x + ext, streampts(r(11)).y)
                Dim point22 As New Point(streampts(r(12)).x + ext, streampts(r(12)).y)
                Dim point23 As New Point(streampts(r(13)).x + ext, streampts(r(13)).y)
                Dim point24 As New Point(streampts(r(14)).x + ext, streampts(r(14)).y)
                Dim point25 As New Point(streampts(r(15)).x + ext, streampts(r(15)).y)
                Dim point26 As New Point(streampts(r(16)).x + ext, streampts(r(16)).y)
                Dim point27 As New Point(streampts(r(17)).x + ext, streampts(r(17)).y)
                Dim curvepoints3 As Point() = {point0, point1, point2, point3, point4, point5, _
                point6, point7, point8, point9, point10, point11, point12, point13, point14, point15, _
                point16, point17, point18, point19, point20, point21, point22, point23, point24, point25, _
                point26, point27}

                If side = 1 Then hillcount_L += 1 Else hillcount_R += 1
                hill_path.AddCurve(curvepoints3, 0.4F)

                Dim hx, hy As Integer       'place trees on crests of mountains/hills
                Dim hillpath
                hillpath = hill_path.PathPoints
                Dim width, height, starty As Integer
                Dim level, h, lv As Single
                Dim e As Integer = 0
                If side = 1 Then
                    Do While hillpath(e).x > 0
                        hx = hillpath(e).x + yrand(4)
                        hy = hillpath(e).y + 20
                        starty = hy - 20
                        If e = 0 Then
                            level = 7 - ((starty - horizon + 1) / (Image.Height - horizon))
                            lv = (7 - level) * 0.6
                            h = lv ^ 2 '             height/width factor for perceived distance
                            If h > 0.42 Then Exit Do
                            'ElseIf hillpath(e - 1).x - hillpath(e).x < 10 Or hillpath(e).x - hillpath(e + 1).x < 10 Then
                            ' GoTo loop1
                        End If
                        height = randh(550, 600) * h
                        ' If height > 100 Then Exit Do
                        width = height * randh(2, 4) / 10
                        If e = 0 Then e = 5 : GoTo loop1
                        If randsign() > 0 Then
                            rivertrees(hx, hy, width, height)
                        Else

                            Dim hilltrees_rect As New Rectangle(hx, hy, width, -height)
                            Dim beep
                            beep = findcolor(2, 0)
                            Dim tree_brush As New SolidBrush(Color.FromArgb(255, beep.R, beep.G, beep.B))
                            g.FillEllipse(tree_brush, hilltrees_rect)
                        End If

loop1:                  e += 1
                    Loop
                End If

                acchill_path.AddCurve(curvepoints3)     'accumulates the hillshapes
                g.SetClip(hill_path)
                paintdualcolors(hill_path.GetBounds.X, hill_path.GetBounds.Y, hill_path.GetBounds.Width, _
                hill_path.GetBounds.Height, 5, 1)

                Dim a As Integer
                If Settings.Winter = True Then a = 240 Else a = 100
                Dim haze_brush As New SolidBrush(Color.FromArgb(a, 240, 240, 255))
                If Settings.Mountains = False Then
                    g.FillPath(haze_brush, acchill_path)
                End If

                g.ResetClip()
                If Settings.Outlines = True Then g.DrawPath(stainedglasspen, hill_path)

                levelstartx = levelendx     'new start
            Loop
            q = -1
            For i As Integer = 0 To 63
                streampts(i) = streampts(123 - i)   'put other side of river at front of index
            Next

        Next side
        'makerivertrees()
    End Sub

    Private Sub Rotate(ByVal x As Integer, ByVal y As Integer, ByVal a As Single)
        g.TranslateTransform(x, y)
        g.RotateTransform(a)
        g.TranslateTransform(-x, -y)
    End Sub

    Private Sub RotateBack(ByVal x As Integer, ByVal y As Integer, ByVal a As Single)
        g.TranslateTransform(x, y)
        g.RotateTransform(-a)
        g.TranslateTransform(-x, -y)
    End Sub

    Private Sub makerivertrees()

        Dim h, level, xbegin1, ybegin1, ybase, ybase1, ybase2, xwidth1, yheight1 As Single
        Dim beg, en, st, q, ht As Integer
        Dim j As Integer
        'ht = randh(160 * 3, 270 * 3)     'basic tree height
        ht = randh(100, 200) * 3
        Dim trees_path As New Drawing2D.GraphicsPath

        level = (riverpts(0).y - horizon + 1) / (Image.Height - horizon)
        h = (level * 0.6) ^ 2

        ybase1 = riverpts(0).y + randh(0, 100) * h ' left side
        ybase2 = riverpts(0).y + randh(0, 100) * h ' right side
        Do
            ybase = Math.Min(ybase1, ybase2) ' pick furthest back side

            If ybase = ybase1 Then
                beg = 1 : en = 61 : st = 1 : q = -1
            Else
                beg = 122 : en = 62 : st = -1 : q = 1
            End If

            level = (ybase - horizon + 1) / (Image.Height - horizon)
            h = (level * 0.6) ^ 2

size:
            yheight1 = (ht + yrand(79)) * h
            Dim Con As Single
            Con = (randh(20, 40) / 10)
            xwidth1 = yheight1 / Con
            If xwidth1 < 3 Then GoTo loops
            If yheight1 > 360 Then Exit Do

            ybegin1 = ybase - yheight1 - 5

            For j = beg To en Step st
                If riverpts(j).y >= ybase Then
                    xbegin1 = riverpts(j - st).x + (ybase - riverpts(j - st).y) _
                                         * (riverpts(j).x - riverpts(j - st).x) _
                                         / (riverpts(j).y - riverpts(j - st).y)
                    xbegin1 = xbegin1 + q * xwidth1 + q * randh(0, 10)
                    Exit For
                End If
            Next
            If yrand(10) > 3 Then
                rivertrees(xbegin1, ybegin1, xwidth1, yheight1)
            Else

                Dim skyhole, ra, rb As Integer
                Dim rect As New Rectangle(xbegin1, ybegin1, xwidth1, yheight1)
                If Settings.Outlines = True And xwidth1 < 6 Then GoTo loops
                skyhole = randh(1, 6)
                Select Case skyhole
                    Case Is = 1     'left gap
                        ra = 230 : rb = 340
                    Case Is = 2     'right gap
                        ra = 335 : rb = 340
                    Case Is = 3     'bottom out
                        ra = 135 : rb = 270
                    Case Is = 4     'top split
                        ra = 280 : rb = 350
                    Case Is >= 5    'no change
                        ra = 280 : rb = 360
                End Select
                'treeangle = randh(-14, 14)

                '  This will rotate the following objects by "treeangle" degrees.
                '  It will rotate the objects around the x and y values 
                '  xbegin1 + xwidth1/2, ybegin1 + yheight1 rotates the object around the bottom-middle of the object.
                '  xbegin1 + xwidth1/2, ybegin1 + yheight1/2 rotates the object around the middle-middle of the object.

                'Rotate(xbegin1 + xwidth1 / 2, ybegin1 + yheight1, treeangle)
                Dim brush1 As New SolidBrush(Color.Empty)
                brush1.Color = findcolor(2, 0)     '2 represents river trees, for darker colors

                g.FillPie(brush1, rect, ra, rb)
                g.SetClip(rect)
                paintdualcolors(xbegin1, ybegin1, xwidth1, yheight1, 3, 2)
                g.ResetClip()

                If Settings.Outlines = True Then
                    g.DrawPie(stainedglasspen, rect, ra, rb)
                    g.DrawEllipse(stainedglasspen, rect)
                End If
                ' RotateBack(xbegin1 + xwidth1 / 2, ybegin1 + yheight1, treeangle)
            End If
            treetrunks(xbegin1, xwidth1, ybegin1, yheight1, h)

loops:      If ybase = ybase1 Then
                ybase1 = ybase1 + (randh(0, 4) ^ 4) * h
            Else
                ybase2 = ybase2 + (randh(0, 4) ^ 4) * h
            End If
        Loop Until False

    End Sub
    Private Sub treetrunks(ByVal xbegin1, ByVal xwidth1, ByVal ybegin1, ByVal yheight1, ByVal h)

        Dim brush As New SolidBrush(Color.DarkGray)
        Dim xtrunkbottom1, xtrunkwidth, ytrunkbottom1, ytrunkbottom2, xtrunktop, ytrunktop As Single
        For m As Integer = 1 To 2 ' do two trunks per tree
            xtrunkbottom1 = xbegin1 + xwidth1 / 2
            ytrunkbottom1 = ybegin1 - 3 + yheight1 + 60 * h
            xtrunkwidth = xtrunkbottom1 + 15 * h
            ytrunkbottom2 = ytrunkbottom1

            xtrunktop = xtrunkbottom1 + randsign() * yrand(6)
            ytrunktop = randh(ytrunkbottom1 - yheight1 * 0.8, ytrunkbottom1 - yheight1 * 1.2)

            Dim point21 As New Point(xtrunkbottom1, ytrunkbottom1)
            Dim point22 As New Point(xtrunkwidth, ytrunkbottom2)
            Dim point23 As New Point(xtrunktop, ytrunktop)
            Dim curvepoints1 As Point() = {point21, point22, point23, point21}

            'PLACE A BUSH at the bottom of the trunk, now that the coordinates are known
InsertBush:  '  makerivertreebush(xtrunkbottom1, ytrunkbottom1 + 6 * h, h, s)

Finishtrunks:
            Dim trunk_path = New Drawing2D.GraphicsPath
            '  Rotate(xtrunkbottom1, ytrunkbottom1, treeangle) 'same treeangle used for main tree body
            trunk_path.AddCurve(curvepoints1, 0.1F)
            Dim w As Single = trunk_path.getbounds.width
            If trunk_path.GetBounds.Width < 2 Or trunk_path.getbounds.height < 1 Then Return
            g.SetClip(trunk_path)

            If yrand(100) > 80 Then g.FillPath(Brushes.Black, trunk_path) Else g.FillPath(Brushes.DarkGray, trunk_path)
            'paintdualcolors(trunk_path.GetBounds.X, trunk_path.GetBounds.Y, _
            'trunk_path.GetBounds.Width, trunk_path.GetBounds.Height, 4, 1)
            Return



            '' PUT BLACK BLOTCHES ON TREE
            'Dim pen1 As New Pen(Color.Black, 1)
            'For r As Integer = 1 To 5
            '    Dim x, y, x1, y1 As Integer
            '    x = randh(trunk_path.getbounds.x - 4, trunk_path.getbounds.width + 4)
            '    y = randh(trunk_path.getbounds.y, trunk_path.getbounds.y - trunk_path.getbounds.height)
            '    x1 = x + randh(1, 2)
            '    y1 = y
            '    g.DrawLine(pen1, x, y, x1, y1)
            'Next
            '' If Settings.Outlines = True Then g.DrawPath(stainedglasspen, trunk_path)
        Next
        g.ResetClip()


    End Sub
    Private Sub makerivertreebranch2()
        ''Place a branch or two on tree, using rotated ellipses
        ''first get proper branch size according to distance
        'Dim xbranchwidth, ybranchtop As Integer
        'Dim branch_path As New Drawing2D.GraphicsPath
        'Dim branch_angle As Integer = randh(-30, 30)

        'xbranchwidth = (randh(50, 100)) * h
        'ybranchtop = (randh(100, 150)) * h
        'branch_path.AddEllipse(xtrunkbottom1, yheight1, xbranchwidth, ybranchtop)
        'If branch_path.GetBounds.Width < 1 Then GoTo loops

        'Rotate(xtrunkbottom1, (ytrunkbottom1 - ytrunktop) / 2, branch_angle)
        'RotateBack(xtrunkbottom1, (ytrunkbottom1 - ytrunktop) / 2, branch_angle)
        'paintdualcolors(xtrunkbottom1, ytrunktop, xtrunkwidth, -ybranchtop, 3, 2)
    End Sub
    Public Function findcolor(ByVal lightness, ByVal grayness) '  simple color selection
        'lightness determines lightness of color, and ranges from 0 to 2 for CMB.light, converted to Reg colors
        'it is predetermined from the referring code
CMB:
        If Settings.Scheme > 9 Then
            Dim season As Integer = Settings.Scheme - 10  'changing from 11, 12, 13, etc
            Dim light1 As Integer = c_randh(0, 2) 'three general levels of tone but use first two
            Dim colors1 As Integer = c_randh(1, 14)       '14 colors in each light1 level
            findcolor = ModCMBcolors.getCMBcolor(season, lightness, colors1)
        Else
NonCMB:     Dim colsymb, domin As Integer
            Dim colorchoice As Integer
            If Settings.Scheme > 0 Then domin = 1 Else domin = 2
            colsymb = c_randh(0, Settings.Scheme * domin)
            If colsymb > Settings.Scheme Then colsymb = 0 'setting dominance through weighting
            colorchoice = Settings.Colours(colsymb)
            Dim gray As Integer = c_randh(1, 3)  'don't get too dark (level 3)
            If grayness = 1 Then gray = 3
            Dim light As Integer = (4 - (lightness + 1)) * 3 - 2
            Dim intensity As Integer = randh(light, light + 2)
            findcolor = modColors.getcolor(colorchoice, gray, intensity)

        End If

    End Function
    Private Sub makerivertreebush(ByVal startx, ByVal starty, ByVal h, ByVal s)

        'to make a bush
        Dim x0, x1, x2, x3, y0, y1, y2, y3, outer, inner As Integer
        Dim bush_path As New Drawing2D.GraphicsPath
        'Dim brushy As New SolidBrush(Color.BurlyWood)
        If s = 2 Then outer = 150 : inner = 100 Else outer = 100 : inner = 150
        x1 = startx - randh(90, outer) * h
        y1 = starty + randh(20, 35) * h
        x2 = startx '- randh(30, 45) * h
        y2 = starty - randh(30, 45) * h
        x3 = startx + randh(90, inner) * h
        y3 = y1 ' - randh(30, 45) * h
        x0 = startx '+ randh(30, 45) * h
        y0 = y1 '+ randh(30, 45) * h


        bush_path.AddBezier(New Point(x1, y1), New Point(x2, y2), _
        New Point(x3, y3), New Point(x0, y0))
        g.SetClip(bush_path)
        If bush_path.GetBounds.Height < 1 Or bush_path.GetBounds.Width < 1 Then g.ResetClip() : Return
        paintdualcolors(bush_path.GetBounds.X, bush_path.GetBounds.Y, bush_path.GetBounds.Width, _
        bush_path.GetBounds.Height, 3, 1)
        'g.FillPath(brushy, bush_path)
        g.ResetClip()
        If Settings.Outlines = True Then g.DrawPath(stainedglasspen, bush_path)


    End Sub
    Private Sub paintdualcolors(ByVal beginx, ByVal beginy, ByVal xwidth, ByVal yheight, ByVal special, ByVal type_fill)
        If Settings.Scheme > 9 Then
            dualCMBblend(beginx, beginy, xwidth, yheight, special, type_fill)
        Else
            dualcolorblend(beginx, beginy, xwidth, yheight, special, type_fill)
        End If
    End Sub

    Private Sub dualCMBblend(ByVal beginx, ByVal beginy, ByVal xwidth, ByVal yheight, ByVal special, ByVal type_fill)
        'type_fill is 1 for rectangle, 2 for ellipse

        'SPECIAL is: 
        '9 - river, 
        '8 - beach,
        '7 - ocean
        '6 -
        '5 - rivermountains
        '4 -
        '3 - bush
        '2 - distant mountains
        '1 -

        Dim light1, light2, colors1, colors2, season As Integer
        Dim color1, color2
        Dim area As New Rectangle(beginx, beginy, xwidth, yheight)
        season = Settings.Scheme - 10  'CMB color seasons from 0 to 3
River:
        If special = 9 Then  ' cue for focus on river color
            Dim color_river As Integer = c_randh(1, 5)
            Select Case color_river
                Case 1
                    If season = 0 Then light1 = 0 : colors1 = 8 : light2 = 2 : colors2 = 11
                    If season = 1 Then light1 = 0 : colors1 = 4 : light2 = 2 : colors2 = 2
                    If season = 2 Then light1 = 0 : colors1 = 6 : light2 = 2 : colors2 = 4
                    If season = 3 Then light1 = 0 : colors1 = 4 : light2 = 2 : colors2 = 1
                Case 2
                    If season = 0 Then light1 = 0 : colors1 = 11 : light2 = 2 : colors2 = 9
                    If season = 1 Then light1 = 0 : colors1 = 8 : light2 = 2 : colors2 = 1
                    If season = 2 Then light1 = 0 : colors1 = 7 : light2 = 2 : colors2 = 6
                    If season = 3 Then light1 = 0 : colors1 = 5 : light2 = 2 : colors2 = 6
                Case 3
                    If season = 0 Then light1 = 0 : colors1 = 13 : light2 = 2 : colors2 = 10
                    If season = 1 Then light1 = 0 : colors1 = 9 : light2 = 2 : colors2 = 3
                    If season = 2 Then light1 = 0 : colors1 = 10 : light2 = 2 : colors2 = 14
                    If season = 3 Then light1 = 0 : colors1 = 8 : light2 = 2 : colors2 = 7
                Case 4
                    If season = 0 Then light1 = 1 : colors1 = 3 : light2 = 2 : colors2 = 1
                    If season = 1 Then light1 = 0 : colors1 = 10 : light2 = 2 : colors2 = 7
                    If season = 2 Then light1 = 1 : colors1 = 6 : light2 = 2 : colors2 = 9
                    If season = 3 Then light1 = 0 : colors1 = 10 : light2 = 2 : colors2 = 11
                Case 5
                    If season = 0 Then light1 = 1 : colors1 = 10 : light2 = 2 : colors2 = 4
                    If season = 1 Then light1 = 0 : colors1 = 13 : light2 = 2 : colors2 = 10
                    If season = 2 Then light1 = 1 : colors1 = 12 : light2 = 2 : colors2 = 1
                    If season = 3 Then light1 = 0 : colors1 = 14 : light2 = 2 : colors2 = 4
            End Select
            color1 = ModCMBcolors.getCMBcolor(season, light1, colors1)
            color2 = ModCMBcolors.getCMBcolor(season, light2, colors2)

            Dim Brush0 As New Drawing2D.LinearGradientBrush(area, _
                      Color.FromArgb(color1.a, color1.r, color1.g, color1.b), _
                      Color.FromArgb(color2.a, color2.r, color2.g, color2.b), _
                      Drawing2D.LinearGradientMode.Vertical)
            g.FillRectangle(Brush0, area)

            Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
                    Color.FromArgb(200, 255, 255, 255), _
                    Color.FromArgb(15, 25, 25, 25), _
                    Drawing2D.LinearGradientMode.Vertical)
            For n As Integer = 1 To 1
                g.FillRectangle(Brush3, area)
            Next n
        End If

        'Paint Beach
        If special = 8 Then
            Dim color_beach As Integer = c_randh(1, 4)
            Select Case color_beach
                Case 1
                    If season = 0 Then light1 = 0 : colors1 = 2
                    If season = 1 Then light1 = 0 : colors1 = 12
                    If season = 2 Then light1 = 0 : colors1 = 1
                    If season = 3 Then light1 = 0 : colors1 = 2
                Case 2
                    If season = 0 Then light1 = 0 : colors1 = 6
                    If season = 1 Then light1 = 1 : colors1 = 1
                    If season = 2 Then light1 = 0 : colors1 = 3
                    If season = 3 Then light1 = 0 : colors1 = 12
                Case 3
                    If season = 0 Then light1 = 1 : colors1 = 5
                    If season = 1 Then light1 = 1 : colors1 = 9
                    If season = 2 Then light1 = 0 : colors1 = 13
                    If season = 3 Then light1 = 1 : colors1 = 6
                Case 4
                    If season = 0 Then light1 = 1 : colors1 = 7
                    If season = 1 Then light1 = 1 : colors1 = 10
                    If season = 2 Then light1 = 0 : colors1 = 14
                    If season = 3 Then light1 = 1 : colors1 = 8
            End Select
            Dim brush1 As New SolidBrush(Color.Orange)
            brush1.Color = ModCMBcolors.getCMBcolor(season, light1, colors1)
            g.FillRectangle(brush1, area)

            Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
            Color.FromArgb(130, 255, 255, 255), _
            Color.FromArgb(15, 15, 15, 15), _
            Drawing2D.LinearGradientMode.Vertical)
            For n As Integer = 1 To 2
                g.FillRectangle(Brush3, area)
            Next n
        End If

        'Paint Ocean
        If special = 7 Then
            Dim color_ocean As Integer = c_randh(1, 2)
            Select Case color_ocean
                Case 1
                    If season = 0 Then light1 = 1 : colors1 = 14
                    If season = 1 Then light1 = 1 : colors1 = 3
                    If season = 2 Then light1 = 1 : colors1 = 3
                    If season = 3 Then light1 = 1 : colors1 = 10
                Case 2
                    If season = 0 Then light1 = 2 : colors1 = 7
                    If season = 1 Then light1 = 2 : colors1 = 6
                    If season = 2 Then light1 = 1 : colors1 = 6
                    If season = 3 Then light1 = 2 : colors1 = 6

            End Select
            Dim brush1 As New SolidBrush(Color.Orange)
            brush1.Color = ModCMBcolors.getCMBcolor(season, light1, colors1)
            g.FillRectangle(brush1, area)

            Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
            Color.FromArgb(5, 255, 255, 255), _
            Color.FromArgb(150, 255, 255, 255), _
            Drawing2D.LinearGradientMode.Vertical)
            For n As Integer = 1 To 4
                g.FillRectangle(Brush3, area)
            Next n


        End If

OtherCMB:
        If special < 7 Then
            light1 = c_randh(0, 2)
            colors1 = c_randh(1, 14)
            Dim brush As New SolidBrush(Color.Orange)
            brush.Color = ModCMBcolors.getCMBcolor(season, 2, colors1)

            Dim Brush2 As New Drawing2D.LinearGradientBrush(area, _
            Color.FromArgb(100, (255), (255), (255)), _
            Color.FromArgb(200, 5, 5, 100), _
            Drawing2D.LinearGradientMode.Vertical)

            If type_fill = 1 Then       'rectangle
                g.FillRectangle(brush, area)
                g.FillRectangle(Brush2, area)
            End If
            If type_fill = 2 Then       'ellipse
                If special <> 3 Then
                    g.FillEllipse(brush, area)
                End If
                g.FillEllipse(Brush2, area)
            End If
        End If

endsub:
    End Sub
    Private Sub dualcolorblend(ByVal beginx, ByVal beginy, ByVal xwidth, ByVal yheight, ByVal special, ByVal type_fill)

        Dim colorintensity As Integer
        Dim area As New Rectangle(beginx, beginy, xwidth, yheight)
        Dim colsymb, colsymb2, domin As Integer
        Dim scheme As Integer = Settings.Scheme
        Dim colorchoice, colorchoice2 As Integer

RiverNonCMB:
        If special = 9 Then        ' river focus
            colorchoice = c_randh(12, 14)
            colorchoice2 = colorchoice
            Do While colorchoice2 = colorchoice
                colorchoice2 = c_randh(12, 14)
            Loop
            Dim brush1 As New Drawing2D.LinearGradientBrush(area, _
                     modColors.getcolor(colorchoice, 1, 7), _
                     modColors.getcolor(colorchoice2, 1, 1), _
                     Drawing2D.LinearGradientMode.Vertical)
            g.FillRectangle(brush1, area)

            'Dim Brush3 As New Drawing2D.LinearGradientBrush(area, Color.FromArgb(240, _
            '(255), (255), (255)), Color.FromArgb(0, 255, 255, 255), _
            ' Drawing2D.LinearGradientMode.Vertical)

            'g.FillRectangle(Brush3, area)
        End If

OtherNonCMB:
        If scheme > 0 Then domin = 1 Else domin = 2
        colsymb = c_randh(0, scheme * domin)
        If colsymb > Settings.Scheme Then colsymb = 0 'setting dominance through weighting
        colorchoice = Settings.Colours(colsymb)
        colsymb2 = colsymb
        If scheme = 0 Then GoTo choice
        If scheme > 0 Or special <> 3 Then      'make sure second color is different from first
            Do While colsymb2 = colsymb
                colsymb2 = c_randh(0, scheme)
            Loop
        End If
choice: colorchoice2 = Settings.Colours(colsymb2)
        If special = 3 Or special = 8 Then         'river trees 
            colorintensity = c_randh(5, 8)
            Dim Brush3 As New Drawing2D.LinearGradientBrush(area, Color.FromArgb(0, _
            (0), (0), (0)), Color.FromArgb(210, 10, 10, 10), _
             Drawing2D.LinearGradientMode.Vertical)
            g.FillEllipse(Brush3, area)
            Return
        End If
        If special = 5 Then         'river mountains
            colorintensity = c_randh(1, 3)
        End If
        'color.FromArgb(0,255,255,255), 
        Dim brush2 As New Drawing2D.LinearGradientBrush(area, _
             modColors.getcolor(colorchoice, 1, colorintensity), _
            modColors.getcolor(colorchoice2, 2, colorintensity), _
             Drawing2D.LinearGradientMode.Vertical)
        If type_fill = 1 Then
            g.FillRectangle(brush2, area)
        End If
        If type_fill = 2 Then
            g.FillEllipse(brush2, area)
        End If

    End Sub


    Private Sub makepathpoints()
        Dim x, y As Integer
        For n As Integer = 1 To 123
            x = riverpts(n).x - 2 : y = riverpts(n).y - 2
            Dim pen As New Pen(Color.Black)
            g.DrawEllipse(pen, x, y, 5, 5)
        Next

    End Sub
    Private Sub makeevergreentree()

    End Sub

    Private Sub CMBcolordisplay()
        Dim x, y, width, height As Integer
        x = 10
        y = 20
        width = 30
        height = 30

        For i As Integer = 0 To 3
            For j As Integer = 0 To 2
                For k As Integer = 1 To 14
                    Dim box As New Rectangle(x, y, 20, 20)
                    Dim paintbrush As New SolidBrush(Color.Black)
                    paintbrush.Color = ModCMBcolors.getCMBcolor(i, j, k)
                    g.FillRectangle(paintbrush, box)
                    x += 27
                Next k
                x = 10
                y += 22
            Next j
            x = 10
            y += 30
        Next i
        'If n = 1 Then endit = 14
        'If n = 2 Then endit = 14 : start = 1
        x = 10
        ' Next
    End Sub
    Private Sub colordisplayreg()

        Dim x, y, width, height As Integer
        x = 10
        y = 20
        width = 30
        height = 30

        For i As Integer = 14 To 18
            For j As Integer = 1 To 3
                For k As Integer = 1 To 9
                    Dim box As New Rectangle(x, y, 20, 20)
                    Dim paintbrush As New SolidBrush(Color.Black)
                    paintbrush.Color = modColors.getcolor(i, j, k)
                    g.FillRectangle(paintbrush, box)
                    x += 27
                Next k
                x = 10
                y += 22
            Next j
            x = 10
            y += 30
        Next i
    End Sub
    Private Sub makeskyfluffclouds()
        Dim x, y As Integer

        Dim fluffwidth As Integer = 300
        Dim fluffheight As Integer = 25
        ''Fill an ellipse setting CenterColor and SurroundColors.
        For n As Integer = 1 To 5
            x = yrand(Image.Width) : y = randh(Image.Height * 0.2, Image.Height * 0.3)

            Dim rect_pts() As Point = {New Point(x, y), New Point(x + fluffwidth, y), _
               New Point(x + fluffwidth, y + fluffheight), New Point(x, y + fluffheight)}

            Dim ellipse_path As New Drawing2D.GraphicsPath()
            Dim path_brush As New Drawing2D.PathGradientBrush(rect_pts)
            ellipse_path.AddEllipse(x, y, fluffwidth, fluffheight)

            path_brush = New Drawing2D.PathGradientBrush(ellipse_path)
            path_brush.CenterColor = Color.White
            path_brush.SurroundColors = New Color() {Color.Empty}

            g.FillEllipse(path_brush, x, y, fluffwidth, fluffheight)
            ellipse_path.Reset()
        Next n
    End Sub
    Private Sub makeskyfluffcover(ByVal x, ByVal y, ByVal wid, ByVal hgt)

        Dim fluffwidth As Integer = randh(200, 250)
        Dim fluffheight As Integer = randh(25, 40)
        ''Fill an ellipse setting CenterColor and SurroundColors.

        Dim rect_pts() As Point = {New Point(x, y), New Point(x + fluffwidth, y), _
           New Point(x + fluffwidth, y + fluffheight), New Point(x, y + fluffheight)}

        Dim ellipse_path As New Drawing2D.GraphicsPath()
        Dim path_brush As New Drawing2D.PathGradientBrush(rect_pts)
        ellipse_path.AddEllipse(x, y, fluffwidth, fluffheight)

        path_brush = New Drawing2D.PathGradientBrush(ellipse_path)
        path_brush.CenterColor = Color.White
        path_brush.SurroundColors = New Color() {Color.Empty}

        g.FillEllipse(path_brush, x, y, fluffwidth, fluffheight)
        ellipse_path.Reset()



    End Sub
    Private Sub makeskyevening()
        Dim a As Integer = 0
        'Dim rect As New Rectangle(0, 0, Image.Width, Image.Height)
        'Dim rec_brush As New SolidBrush(Color.Blue)

        'g.FillRectangle(rec_brush, rect)

        Dim rect_pts() As Point = {New Point(-a, -a), New Point(Image.Width + a, -a), _
         New Point(Image.Width + a, Image.Height + a), New Point(-a, Image.Height + a)}

        Dim ellipse_path As New Drawing2D.GraphicsPath()
        Dim path_brush As New Drawing2D.PathGradientBrush(rect_pts)
        ellipse_path.AddEllipse(-a, -a, Image.Width + a, Image.Height + a)
        path_brush = New Drawing2D.PathGradientBrush(ellipse_path)

        path_brush.CenterColor = findcolor(2, 0)
        path_brush.SurroundColors = New Color() {Color.Empty}
        g.FillEllipse(path_brush, -a, -a, Image.Width + a, Image.Height + a)
        ' Dim rec_brush As New SolidBrush(Color.path_brush.surroundcolors)

        ' ellipse_path.Reset()


    End Sub


    Private Sub makemidpines()
        Dim brush As New SolidBrush(Color.Green)
        Dim a, b, c, plx, ply, anchorx, anchory, pc, pd, tipx, bleftx, brightx As Integer
        Dim ra, rb, rc, rd, branchx, branchy As Single
        Dim trunk, branch As New Drawing2D.GraphicsPath
        a = Image.Width - 100
        b = Image.Height - 150 : c = Image.Height - 250
        anchorx = randh(0, a) 'anchor the tree area
        anchory = randh(b, c)

        For n As Integer = 1 To 7
            a = anchorx + randh(7, 20)
            'b = Image.Height - 200
            c = anchory - yrand(10)
            plx = randh(0, a) 'place the branch
            ply = randh(b, c)
variable:   pc = randh(70, 110)  ' extent of rectangle (ellipse)
            pd = randh(20, 50)
            Dim rect As New Rectangle(plx, ply, pc, pd)

            For i = 1 To 3
                ra = randh(335, 365)        'points for pie shapes - by angles
                rb = randh(10, 35)
                rc = randh(170, 185)
                rd = randh(15, 35)
                g.FillPie(brush, rect, ra, rb)

                rect.Y += yrand(20)
                rect.X += yrand(10)
                g.FillPie(brush, rect, rc, rd)
            Next

            trunk = New Drawing2D.GraphicsPath
            branch = New Drawing2D.GraphicsPath
            tipx = plx + pc / 2 - 15
            bleftx = (tipx + randsign() * yrand(10))
            brightx = bleftx + 3
            Dim point0 As New Point(tipx, ply)
            Dim point1 As New Point(bleftx, ply + 150)
            Dim point2 As New Point(brightx, ply + 150)
            Dim curvepoints As Point() = {point0, point1, point2}
            trunk.AddClosedCurve(curvepoints)
            g.FillPath(Brushes.Black, trunk)

            Dim trunkpts
            trunkpts = trunk.PathPoints 'finding trunk points for branches
            Dim tkx, tky As Single
            For t As Integer = 0 To 1
                tkx = yrand(trunkpts(8).x - trunkpts(6).x)
                tky = yrand(trunkpts(8).y - trunkpts(6).y) - 5
                branchx = trunkpts(6).x + tkx
                branchy = trunkpts(6).y + tky
                Dim point3 As New Point(branchx, branchy)
                Dim rs As Integer = randsign()
                Dim rsy As Integer = randsign()
                Dim vr As Single = randh(5, 12)
                Dim vry As Single = randh(5, 12)
                Dim branch2x, branch2y, branch3x, branch3y As Single
                branch2x = branchx + rs * vr
                branch2y = branchy + rsy * vr
                Dim point4 As New Point(branch2x, branch2y)
                branch3x = branch2x + rs * vr
                branch3y = branch2y
                Dim point5 As New Point(branch3x, branch3y)
                Dim curvepoints2 As Point() = {point3, point4, point5}
                branch.AddCurve(curvepoints2)
                g.DrawCurve(Pens.Black, curvepoints2)
            Next
            'Dim branchh
            'branchh = branch.PathPoints       
        Next

    End Sub

    'Private Sub makecloseupwater()

    'End Sub
    'Private Sub makeoceanwaves()

    'End Sub
    'Private Sub makebeachfront()

    'End Sub
    'Private Sub ptsarray()
    '    Dim rect As New Rectangle(20, 20, 200, 100)
    '    Dim ptsarray As Single()() = {New Single() {1, 0, 0, 0, 0}, New Single() {0, 1, 0, 0, 0}, _
    '    New Single() {0, 0, 1, 0, 0}, New Single() {0, 0, 0, 0.5F, 0}, New Single() {0, 0, 0, 0, 1}}

    '    'Dim clrMatrix As New   - nothing works form here on in
    '    'Dim imgAttributes As New imageattributes()



    'End Sub
    'Private Sub makeroadway()
    '    'ROADWAY    
    '    '   Hills to traverse, up and down
    '    '   direction 
    '    '   edges
    'End Sub
    Private Sub makemist()

        Dim rect5 As New Rectangle(0, 0, Image.Width, Image.Height)

        If randsign() = 1 Then
            Dim lBrush2 As New Drawing2D.LinearGradientBrush(rect5, _
            Color.FromArgb(255, (220), (220), (250)), _
            Color.FromArgb(30, 220, 220, 255), _
            Drawing2D.LinearGradientMode.Vertical)
            g.FillRectangle(lBrush2, rect5)
        Else
            Dim lBrush2 As New Drawing2D.LinearGradientBrush(rect5, _
                   Color.FromArgb(30, 220, 220, 200), _
                   Color.FromArgb(255, (220), (220), (200)), _
                   Drawing2D.LinearGradientMode.Vertical)
            g.FillRectangle(lBrush2, rect5)
        End If


    End Sub
    Public Function randsign()
        If FObjectRandom.NextDouble < 0.5 Then Return -1 Else Return 1
    End Function

    Public Function yrand(ByVal num1 As Integer)
        Return FObjectRandom.NextDouble * num1 + 1
    End Function
    Public Function randh(ByVal low As Integer, ByVal high As Integer)
        randh = Int(((high - low) + 1) * FObjectRandom.NextDouble + low)
    End Function
    Public Function c_randh(ByVal low As Integer, ByVal high As Integer)
        c_randh = Int(((high - low) + 1) * FColorRandom.NextDouble + low)
    End Function

    Private Sub trial_distanceformula()
        Dim x, y, y1, y2, p, n As Integer
        Dim v, d, h, height1, width1 As Single

        v = 2      'distance from eye to aperture, a constant during the painting
        d = 0       'distance from eye to object, in feet
        h = 0       'real height of object, in feet
        y1 = 0      'vertical placement of bottom of object, in pixels  -supplied

        y2 = 0      'calculated placment of top of object, in pixels 
        p = 800     'number of pixels per foot of height (given about 75 pixels per inch)

        ' formula: y2=y1-(v*h*p/d)
        Dim brush As New SolidBrush(Color.Red)

        'in terms of an ellipse/rectangle
        v = 2
        h = 75 'known height in feet
        Dim dist_path As New Drawing2D.GraphicsPath
        '  For w As Integer = 1 To 2
        'For n = 300 To 190 ' to calculate further distances first
        d = 500
        ' y2 = 250 + 40 / n
        height1 = v * h * p / d
        width1 = height1 / 20
        'x = n * 10
        'y = 100 - n
        x = 300 : y = 50

        Dim rect As New Rectangle(x, y, width1, height1)
        g.FillRectangle(brush, rect)
        g.DrawRectangle(Pens.Black, rect)
        If n > 16 Then dist_path.AddRectangle(rect)
        ' Next n
        g.FillPath(Brushes.Red, dist_path)
        '  q = -1
        '  brush.Color = Color.Aquamarine
        '  Next w




        ''in terms of a cloud
        'p = 800   'pixels per foot
        'v = 1       'from eye to aperture
        'h = 500 'assumed height of cloud
        'q = 1
        'd = 1
        '' For w As Integer = 1 To 2
        '' For d = 1000 To 21000 Step 5000  'to calculate further distances first
        'y2 = 250 + 40 / n

        Dim rect1 As New Rectangle(0, horizon, Image.Width, Image.Height - horizon)
        ' g.FillRectangle(Brushes.BlueViolet, rect1)

        'For n = 0 To 4
        '    p = 800
        '    h = 50
        '    For j As Integer = 1 To 5 - n
        '        r1 = horizon - 40 * n
        '        r2 = r1 - 40
        '        x = yrand(Image.Width)
        '        y = randh(r2, r1)
        '        d = 50000 / (horizon - y)
        '        height1 = v * h * p / d
        '        width1 = height1 * 1.4
        '        rect = New Rectangle(x, y, width1, -height1)
        '        brush.Color = Color.FromArgb(255, 255, 255, 255)
        '        g.FillEllipse(brush, rect)
        '        'g.DrawEllipse(Pens.Black, rect)
        '    Next j
        'Next n



    End Sub
    Public Function pixelheight(ByVal height, ByVal distance)
        Dim y, v As Single
        v = 1
        y = (v * height * 800) / distance
        Return y
    End Function

    Private Sub makerivertreebranch(ByVal xbegin1, ByVal ybegin1, ByVal h)
        Dim branch_angle As Integer
        Dim xwidth1 As Integer = randh(5, 10) * h
        Dim yheight1 As Integer = randh(20, 30) * h
        Dim rect As New Rectangle(xbegin1, ybegin1, xwidth1, yheight1)
        Dim branch_path As New Drawing2D.GraphicsPath

        '  This will rotate the following objects by "treeangle" degrees.
        '  It will rotate the objects around the x and y values 
        'xbegin1 + xwidth1/2, ybegin1 + yheight1 rotates the object around the bottom-middle of the object.
        'xbegin1 + xwidth1/2, ybegin1 + yheight1/2 rotates the object around the middle-middle of the object.

        branch_angle = randh(-30, 30)
        'branch_path.AddEllipse(rect)
        paintdualcolors(xbegin1, ybegin1, xwidth1, yheight1, 5, 2)
        'Rotate(xbegin1 + xwidth1 / 2, ybegin1 + yheight1, branch_angle)
        Rotate(xbegin1 + xwidth1 / 2, ybegin1 + yheight1 / 2, branch_angle)
        ' g.FillPie(brush1, rect, ra, rb)
        RotateBack(xbegin1 + xwidth1 / 2, ybegin1 + yheight1, branch_angle)

    End Sub
    Private Sub unusedleftovers()

        'NonCMB:
        '        Else
        '            'If regular color scheme
        '            'Dim area As New Rectangle(beginx, beginy, xwidth, yheight)
        '            Dim colsymb, colsymb2, domin As Integer
        '            Dim scheme As Integer = Settings.Scheme
        '            Dim colorchoice, colorchoice2 As Integer
        'RiverNonCMB:
        '            If special = 9 Then        ' river focus
        '                colorchoice = c_randh(12, 14)
        '                colorchoice2 = colorchoice
        '                Do While colorchoice2 = colorchoice
        '                    colorchoice2 = c_randh(12, 14)
        '                Loop
        '                Dim brush1 As New Drawing2D.LinearGradientBrush(area, _
        '                         modColors.getcolor(colorchoice, 1, 7), _
        '                         modColors.getcolor(colorchoice2, 1, 1), _
        '                         Drawing2D.LinearGradientMode.Vertical)
        '                g.FillRectangle(brush1, area)
        '                'Dim Brush3 As New Drawing2D.LinearGradientBrush(area, Color.FromArgb(240, _
        '                '(255), (255), (255)), Color.FromArgb(0, 255, 255, 255), _
        '                ' Drawing2D.LinearGradientMode.Vertical)

        '                'g.FillRectangle(Brush3, area)
        '            Else

        '                If scheme > 0 Then domin = 1 Else domin = 2
        '                colsymb = c_randh(0, scheme * domin)
        '                If colsymb > Settings.Scheme Then colsymb = 0 'setting dominance through weighting
        '                colorchoice = Settings.Colours(colsymb)
        '                colsymb2 = colsymb
        '                If scheme = 0 Then GoTo choice
        '                If scheme > 0 Or special <> 3 Then      'make sure second color is different from first
        '                    Do While colsymb2 = colsymb
        '                        colsymb2 = c_randh(0, scheme)
        '                    Loop
        '                End If
        '        colorchoice2 = Settings.Colours(colsymb2)
        '                If special = 3 Then         'river trees 
        '                    colorintensity = c_randh(5, 8)
        '                    Dim Brush3 As New Drawing2D.LinearGradientBrush(area, Color.FromArgb(0, _
        '                    (0), (0), (0)), Color.FromArgb(210, 10, 10, 10), _
        '                     Drawing2D.LinearGradientMode.Vertical)
        '                    g.FillEllipse(Brush3, area)
        '                    Return
        '                End If
        '                If special = 5 Then         'river mountains
        '                    colorintensity = c_randh(1, 3)
        '                End If
        '                'color.FromArgb(0,255,255,255), 
        '                Dim brush2 As New Drawing2D.LinearGradientBrush(area, _
        '                     modColors.getcolor(colorchoice, 1, colorintensity), _
        '                    modColors.getcolor(colorchoice2, 2, colorintensity), _
        '                     Drawing2D.LinearGradientMode.Vertical)
        '                If type_fill = 1 Then
        '                    g.FillRectangle(brush2, area)
        '                End If
        '                If type_fill = 2 Then
        '                    g.FillEllipse(brush2, area)
        '                End If
        '            End If
        '        End If


        '' graydientoverlay
        'Dim midintensity As Integer
        'Dim cs As Integer = 0
        'Dim darkness As Integer = randsign()
        'If darkness = 1 Then cs = 255
        'Dim less_intensity As Single = randh(10, 40)
        'Dim more_intensity As Single = randh(170, 220)
        'If darkness = 1 Then midintensity = less_intensity : less_intensity = more_intensity : more_intensity = midintensity
        'Dim tonal_brush As New Drawing2D.LinearGradientBrush(area, _
        'Color.FromArgb(less_intensity, cs, cs, cs), _
        'Color.FromArgb(more_intensity, cs, cs, cs), _
        ' Drawing2D.LinearGradientMode.Vertical)
        'If type_fill = 1 Then
        '    g.FillRectangle(tonal_brush, area)
        'Else
        '    g.FillEllipse(tonal_brush, area)
        'End If
        'If riverpts(45).x - riverpts(20).x < 0 Then
        '    beg = 1 : en = 61 : st = 1 : q = -1
        '    s = 1 'Determine which side to start on first (so back ones don't superimpose on front)
        'Else
        '    beg = 122 : en = 62 : st = -1 : q = 1
        '    s = 2
        'End If


        'ybegin1 = riverpts(0).y

        '            For i As Integer = beg To en Step st
        'part2:          part2 += 1
        '                'level = 7 - (riverpts(i).y - horizon + 1) / (Height - horizon)
        '                'h = (7 - level) ^ 2
        '                level = (riverpts(i).y - horizon + 1) / (Image.Height - horizon)
        '                h = (level * 0.6) ^ 2
        '                yheight1 = (ht - 6 + yrand(20)) * h
        '                If yheight1 > 300 Then Exit For
        '                xwidth1 = (ht / 3 + yrand(20)) * h    'width of tree
        '                xbegin1 = riverpts(i).x + q * xwidth1 + q * randh(0, 10)
        '                'ybegin1 = riverpts(i).y - yheight1 - 5
    End Sub
    Private Sub trialtrees()
        Dim x, y, x1, y1, width, height, wid, hgt As Integer
        x = 100 : y = 100
        ' height  
        width = 100 : height = 300
        Dim rect As New Rectangle(x, y, width, height)
        g.FillRectangle(Brushes.BurlyWood, rect)
        Dim path As New Drawing2D.GraphicsPath
        path.AddRectangle(rect)
        Dim rects As New Rectangle
        Dim brush As New SolidBrush(Color.Green)

        For n As Integer = 1 To 20
            x1 = yrand(width) + x
            y1 = yrand(height) + y
            wid = yrand(width / 2)
            hgt = yrand(height / 2)
            rects = New Rectangle(x1, y1, wid, hgt)
            g.FillEllipse(brush, rects)

        Next


    End Sub
    Private Sub makebuildings(ByVal distance, ByVal Ax, ByVal Ay, ByVal depth)

        Dim wallheight, wallheight_y, peakht, peakface, frontwidth, _
         wl, wh, dd, ff, xtend, sidewidth, eaves, By As Single
        Dim Bx As Integer
cabin:  'lengths in feet
        wallheight = randh(10, 12)
        frontwidth = randh(25, 42)
        sidewidth = randh(20, 40)
        wl = wallheight * 0.4 : wh = wallheight * 0.9
        peakht = randh(wl, wh)
        xtend = randh(2.5, 4)
        eaves = 0.5
        peakface = 1 ' to side, not to front, which is  2 - to be used to select which way house sits

        Bx = Ax
        ' Now convert lengths to new length and pixel placement according to distance

        wh = pixelheight(wallheight, distance)   ' By
        wallheight_y = Ay - wh
        By = wallheight_y
        ff = pixelheight(frontwidth, distance)     ' ff
        ' frontwidth_x = Ax + fw

        dd = pixelheight(sidewidth, distance)       ' dd
        'sidewidth_x = Ax - sw

        peakht = pixelheight(peakht, distance)      'just the peak height above roof

        xtend = pixelheight(xtend, distance)        'roof extension
        basicbuilding(peakface, peakht, Ax, Ay, Bx, By, ff, dd, xtend, distance, frontwidth, sidewidth, depth)

    End Sub

    Private Sub basicbuilding(ByVal peakface, ByVal peakheight, ByVal Ax, ByVal Ay, ByVal Bx, _
    ByVal By, ByVal ff, ByVal dd, ByVal xtend, ByVal distance, ByVal frontwidth, ByVal sidewidth, ByVal depth)
        Dim LVP, RVP, LVPM, RVPM, side As Integer
        Dim Cx, Cy, Dx, Dy, Ex, Ey, Fx, Fy, Gx, Gy, Hx, Hy, Ix, Iy, Jx, Jy, _
                     uvpx, uvpy, Ictrx, Ictry, Jctrx, Jctry, Imidx, Imidy, rtcornerx, rtcornery, afx, afy, _
                    cornerx, cornery, lcornerx, lcornery, IBx, IBy, BEx, BEy, fax, fay, _
                    dax, day, uvp2x, uvp2y, peak1x, peak1y, peak2x, peak2y, vbcornerx, _
                    vbcornery, EBx, EBy, peakhtx, peakhty, Lhtx, Lhty, roofmx, roofmy As Single
       
        If Ay = horizon Then Ay += 1
        If By = horizon Then By += 1


        'LEFT AND RIGHT VANISHINGS POINTS
        'Dim lvangle, lvpy, rvpy, deepx, deepy As Integer
        Dim t As Integer = 0

        If t = 0 Then
            If Ax <= Image.Width / 2 Then
                LVP = randh(Ax - 300, Ax - dd / 4)
                RVP = LVP + 1200
            Else
                RVP = randh(Ax + ff, depth)
                RVP = randh(Ax + ff / 4, Ax + 300)
                LVP = RVP - 1200
            End If

            ''LEFT AND RIGHT MEASURING POINTS

            LVPM = LVP + (Ax - LVP) * 0.55  'left measuring point
            RVPM = Ax + (RVP - Ax) * 0.45   'right measuring point
        Else
            Dy = horizon + 15 * (Ay - By)
            Dim ang As Integer = 0 ' or specify the first angle for vanishing point
            vanishingpoints(Ax, Dy, ang, LVP, RVP, RVPM, LVPM)
        End If

        side = 1
        buildbox(Ax, Ay, Bx, By, dd, LVP, RVP, RVPM, LVPM, side, Cx, Cy, Dx, Dy, sidecolor) 'and get C&D
        Dim sidecolor1 = sidecolor

        side = 2
        buildbox(Ax, Ay, Bx, By, ff, LVP, RVP, RVPM, LVPM, side, Ex, Ey, Fx, Fy, sidecolor) ' and get E&F
        Dim sidecolor2 = sidecolor

        Dim brush As New SolidBrush(Color.Black)
        intersection(RVP, horizon, Cx, Cy, LVP, horizon, Ex, Ey, Gx, Gy) 'get point G

        'do flat roof
        If By > horizon Then joinfour(Cx, Cy, Gx, Gy, Ex, Ey, Bx, By)

        'do peaked roof
        intersection(RVP, horizon, Dx, Dy, LVP, horizon, Fx, Fy, Hx, Hy) 'get point H
        If peakface = 1 Then 'place peak on narrower side of building
            'for extended roof -   ' Two letters together mean extension point
            intersection(Bx, By, Dx, Dy, Ax, Ay, Cx, Cy, Ictrx, Ictry) 'get point Ictr (front center)
            intersection(Gx, Gy, Fx, Fy, Ex, Ey, Hx, Hy, Jctrx, Jctry)  'get point Jctr (back center)
            intersection(Ictrx, Ictry, Ictrx, 0, Cx, Cy, Bx, By, Imidx, Imidy)  'get point Imid
            Iy = Imidy - peakheight : Ix = Imidx
            If Iy = Cy Then Iy += 0.1
            intersection(Ix, Iy, RVP, horizon, Jctrx, Jctry, Jctrx, 0, Jx, Jy)   'get point J, back peak
            intersection(LVPM, horizon, Ax + ff + xtend, Ay, Ax, Ay, Fx, Fy, afx, afy) 'extens.point,Bott.Rt
            intersection(afx, afy, afx, 0, Bx, By, Ex, Ey, BEx, BEy) 'BExy extension
            intersection(Bx, By, Ix, Iy, Ex, Ey, Jx, Jy, uvpx, uvpy) 'get upper vanishing point
            intersection(uvpx, uvpy, BEx, BEy, Ix, Iy, Jx, Jy, peak2x, peak2y) 'get IJxy extension
            intersection(LVPM, horizon, Ax - xtend, Ay, Ax, Ay, Fx, Fy, fax, fay) 'get extend point on AF line
            intersection(RVPM, horizon, Ax + xtend, Ay, Ax, Ay, Dx, Dy, dax, day) 'get extend point on AD line
            intersection(dax, day, dax, 0, Ix, Iy, Bx, By, IBx, IBy) ' get IBxy point (from fa extended point
            intersection(fax, fay, fax, 0, Ex, Ey, Bx, By, EBx, EBy) 'get CBxy point (from da extended point)
            intersection(uvpx, uvpy, EBx, EBy, Jx, Jy, Ix, Iy, peak1x, peak1y) 'get front peak point
            intersection(RVP, horizon, IBx, IBy, peak1x, peak1y, EBx, EBy, cornerx, cornery) ' get front corner
            intersection(RVP, horizon, cornerx, cornery, peak2x, peak2y, BEx, BEy, rtcornerx, rtcornery) ' get rtcorner
            intersection(Cx, Cy, Ix, Iy, Gx, Gy, Jx, Jy, uvp2x, uvp2y)   'other vanishing point lower)
            intersection(LVP, horizon, cornerx, cornery, uvp2x, uvp2y, peak1x, peak1y, lcornerx, lcornery) 'left corner
            intersection(RVP, horizon, lcornerx, lcornery, LVP, horizon, rtcornerx, rtcornery, vbcornerx, vbcornery) 'verybackcorner
            intersection(Ax, Ay, Bx, By, peak1x, peak1y, peak2x, peak2y, peakhtx, peakhty) 'get peak height
            intersection(Cx, Cy, Dx, Dy, peak1x, peak1y, lcornerx, lcornery, Lhtx, Lhty) ' get left gable height
            intersection(cornerx, cornery, cornerx, 0, peak1x, peak1y, peak2x, peak2y, roofmx, roofmy) 'get roofm point

            'do eaves
            Dim maineavex, maineavey, rteavex, rteavey, pkeavex, pkeavey, leavex, leavey, vbeavex, _
            vbeavey, pk2eavex, pk2eavey, epointx, epointy As Single
            Dim eave As Single = pixelheight(0.7, distance)
            maineavex = cornerx
            maineavey = cornery + eave
            intersection(RVP, horizon, maineavex, maineavey, rtcornerx, rtcornery, rtcornerx, 0, rteavex, rteavey) 'right eave point
            intersection(uvpx, uvpy, maineavex, maineavey, peak1x, peak1y, peak1x, 0, pkeavex, pkeavey) 'get peak eave point
            intersection(uvp2x, uvp2y, pkeavex, pkeavey, lcornerx, lcornery, lcornerx, 0, leavex, leavey) 'get left eave point
            intersection(RVP, horizon, leavex, leavey, vbcornerx, vbcornery, vbcornerx, 0, vbeavex, vbeavey) 'get very back eave point
            intersection(uvpx, uvpy, rteavex, rteavey, peak2x, peak2y, peak2x, 0, pk2eavex, pk2eavey) 'get peak2 eave point
            'g.DrawLine(Pens.Black, maineavex, maineavey, cornerx, cornery)

            'BUILDING INSTRUCTIONS

            ' 1. do left eave first
            joinfour(lcornerx, lcornery, leavex, leavey, vbeavex, vbeavey, vbcornerx, vbcornery)
            ' 2. then back roof
            joinfour(lcornerx, lcornery, vbcornerx, vbcornery, peak2x, peak2y, peak1x, peak1y)

            '4. redo left side of bldg, if needed
            Dim roofptx, roofpty As Single
            intersection(Cx, Cy, Dx, Dy, lcornerx, lcornery, peak1x, peak1y, roofptx, roofpty) ' get roofpt

            If Cy > roofpty Or cornery > Ey Then
                buildpeakside(Ax, Ay, Bx, By, Ix, Iy, Cx, Cy, Dx, Dy)
            End If

            '5. do front side peaked, stretched roof
            intersection(peak2x, peak2y, peak2x, 0, peak1x, peak1y, cornerx, cornery, epointx, epointy) 'point below/above jy

            If peak2y < epointy Then ' under normal conditions (roof comes down over front wall)

                '6. redo right side of bldg - with door, windows
                buildnonpeakside(Ax, Ay, Bx, By, Ex, Ey, Fx, Fy)
                basicdoor_windows(Ax, Ay, By, Fx, Fy, Dx, Dy, dd, ff, RVP, LVP, RVPM, LVPM, distance, frontwidth, sidewidth)

                '7. build right peaked, stretched roof
                joinfour(peak2x, peak2y, rtcornerx, rtcornery, cornerx, cornery, peak1x, peak1y)

            Else 'do roof first if perspective shows it hanging out above walls and dipping down at back

                '6. do back right eave
                joinfour(peak2x, peak2y, pk2eavex, pk2eavey, rteavex, rteavey, rtcornerx, rtcornery)

                '7. do roof
                joinfour(peak2x, peak2y, rtcornerx, rtcornery, cornerx, cornery, peak1x, peak1y)

                '8. rebuild left side - to cover sinking back side of roof
                buildpeakside(Ax, Ay, Bx, By, Ix, Iy, Cx, Cy, Dx, Dy)

                '9. build right side - with door, windows
                buildnonpeakside(Ax, Ay, Bx, By, Ex, Ey, Fx, Fy)
                basicdoor_windows(Ax, Ay, By, Fx, Fy, Dx, Dy, dd, ff, RVP, LVP, RVPM, LVPM, distance, frontwidth, sidewidth)
                '7. do roof
                'joinfour(peak2x, peak2y, rtcornerx, rtcornery, cornerx, cornery, peak1x, peak1y)


            End If
            'Build chimney 
            chimney(distance, Ix, Iy, Jx, Jy, LVP, LVPM, RVP, RVPM, uvpx, uvpy, Ex, Ey, Bx, By)

            'Build vent pipe
            Dim pipelen, pl, pipex, pipey, pipewid, pw, croofx, croofy, edgeptx, edgepty As Single
            pipelen = 1.5 'in feet
            pipewid = 0.3
            pl = pixelheight(pipelen, distance)
            pw = pixelheight(pipewid, distance)
            intersection(peak1x, peak1y, rtcornerx, rtcornery, peak2x, peak2y, cornerx, cornery, croofx, croofy) 'roof center
            pipex = croofx
            pipey = croofy
            intersection(croofx, croofy, croofx, 0, peak1x, peak1y, cornerx, cornery, edgeptx, edgepty) 'edgept alternate
            If pipey > edgepty Then pipey = edgepty

            g.FillRectangle(New SolidBrush(Color.Black), pipex, pipey - pl, pw, pl)

            ' front and peak eaves
            joinfour(cornerx, cornery, maineavex, maineavey, rteavex, rteavey, rtcornerx, rtcornery)
            joinfour(cornerx, cornery, maineavex, maineavey, pkeavex, pkeavey, peak1x, peak1y)
            joinfour(pkeavex, pkeavey, peak1x, peak1y, lcornerx, lcornery, leavex, leavey)
            'If Dy > Ay Then joinfour(Dx, Dy, Ax, Ay, Fx, Fy, Hx, Hy)

        End If



        ''roof edges
        'Dim rf As Integer = (Ay - By) / 20
        'Dim endpen As New Pen(Color.White, rf)
        'endpen.Color = findcolor(1, 1)
        'If Settings.Outlines = True Then endpen.Color = Color.Black
        'g.DrawLine(endpen, cornerx, cornery, rtcornerx, rtcornery)
        'g.DrawLine(endpen, cornerx, cornery, peak1x, peak1y)
        'g.DrawLine(endpen, lcornerx, lcornery, peak1x, peak1y)



        ''FOR RIGHT FACING PEAK -  ff shorter than dd
        'Dim fctrx, fctry, bctrx,s bctry, bcx, bcy, rcornerx, rcornery As Single
        'intersection(Bx, By, Fx, Fy, Ax, Ay, Ex, Ey, fctrx, fctry) 'get front center point
        'intersection(Gx, Gy, Dx, Dy, Cx, Cy, Hx, Hy, bctrx, bctry)  'get back center point
        'intersection(fctrx, fctry, fctrx, 0, Ex, Ey, Bx, By, Imidx, Imidy)  'get front top mid point
        'Iy = Imidy - peakhgt : Ix = Imidx
        'intersection(Ix, Iy, LVP, horizon, bctrx, bctry, bctrx, 0, Jx, Jy)   'get point J, back peak
        ''for extended roof - 1/8th length of AB  
        'Dim p As Single = 1 / 6
        'xtend = (Ay - By) * p
        'intersection(RVPM, horizon, Ax - dd - xtend, Ay, Ax, Ay, Dx, Dy, adx, ady) 'extens.point,Bott.lft
        'intersection(adx, ady, adx, 0, Bx, By, Cx, Cy, bcx, bcy) 'Bcxy extension
        'intersection(Bx, By, Ix, Iy, Cx, Cy, Jx, Jy, uvpx, uvpy) 'get upper vanishing point
        'intersection(uvpx, uvpy, bcx, bcy, Ix, Iy, Jx, Jy, IJx, IJy) 'IJxy extension
        'intersection(Ix, Iy, Bx, By, Ax - xtend, Ay, Bx - xtend, By, IBx, IBy) 'get IBxy
        'intersection(Imidx + xtend, Imidy, Imidx + xtend, 0, Jx, Jy, Ix, Iy, JIx, JIy) 'get JIxy
        'peak1x = JIx : peak1y = JIy : peak2x = IJx : peak2y = IJy
        'intersection(uvpx, uvpy, peak1x, peak1y, LVP, horizon, IBx, IBy, cornerx, cornery) 'front roof corner
        'intersection(LVP, horizon, cornerx, cornery, uvpx, uvpy, peak2x, peak2y, lcornerx, lcornery) 'lcornerxy
        'intersection(Ex, Ey, Ix, Iy, Gx, Gy, Jx, Jy, uvp2x, uvp2y)   'other upper vanishing point
        'intersection(RVP, horizon, cornerx, cornery, uvp2x, uvp2y, peak1x, peak1y, rcornerx, rcornery) 'rt corner
        'intersection(LVP, horizon, rcornerx, rcornery, RVP, horizon, lcornerx, lcornery, bcornerx, bcornery)   'backcorner
        'intersection(RVP, horizon, rcornerx, rcornery, rcornerx, rcornery, bcornerx, bcornery, vbcornerx, vbcornery)   'verybackcorner

        ''  
        ''back side peaked, stretched roof
        'joinfour(bcornerx, bcornery, rcornerx, rcornery, peak1x, peak1y, peak2x, peak2y)

        ''left side of building
        'buildpeakside(Ax, Ay, Bx, By, Ix, Iy, Cx, Cy, Dx, Dy)

        ''right side of building
        'buildnonpeakside(Ax, Ay, Bx, By, Ex, Ey, Fx, Fy)

        ''front side peaked,stretched,roof
        'joinfour(peak1x, peak1y, cornerx, cornery, lcornerx, lcornery, peak2x, peak2y)

        ''redo left side if necessary
        'If cornery > peak2y Then buildpeakside(Ax, Ay, Bx, By, Ix, Iy, Cx, Cy, Dx, Dy)

        ''right side of building
        'buildnonpeakside(Ax, Ay, Bx, By, Ex, Ey, Fx, Fy)

        ''roof edges
        'Dim endspen As New Pen(Color.White, 2)
        'endspen.Color = findcolor(0, 1)
        'If Settings.Outlines = True Then endspen.Color = Color.Black
        'g.DrawLine(endspen, peak1x, peak1y, rcornerx, rcornery)
        'g.DrawLine(endspen, cornerx, cornery, peak1x, peak1y)
        'g.DrawLine(endspen, lcornerx, lcornery, cornerx, cornery)


        ''g.DrawLine(Pens.Black, Bx, By, Ix, Iy)


    End Sub

    Private Sub joinfour(ByVal x0, ByVal y0, ByVal x1, ByVal y1, ByVal x2, ByVal y2, ByVal x3, ByVal y3)
        Dim pt0 As New Point(x0, y0)
        Dim pt1 As New Point(x1, y1)
        Dim pt2 As New Point(x2, y2)
        Dim pt3 As New Point(x3, y3)
        Dim quart As Point() = {pt0, pt1, pt2, pt3}
        Dim brush As New SolidBrush(Color.Empty)
        brush.Color = findcolor(1, 1)
        g.FillPolygon(brush, quart)
        If Settings.Outlines = True Then g.DrawPolygon(stainedglasspen, quart)

    End Sub
    Private Sub buildbox(ByVal Ax, ByVal Ay, ByVal Bx, ByVal By, ByVal dd, ByVal LVP, ByVal RVP, _
    ByVal RVPM, ByVal LVPM, ByVal side, ByRef Cx, ByRef Cy, ByRef Dx, ByRef Dy, ByRef sidecolor)

        Dim x0, y0, x1, y1, x2, y2, x3, y3, x4, y4, M12, M34, B12, B34 As Single
        ' 
        'line1
        If side = 1 Then
            x1 = RVPM : y1 = horizon
            x2 = Ax - dd : y2 = Ay
        Else
            x1 = LVPM : y1 = horizon
            x2 = Ax + dd : y2 = Ay
        End If

        'line2
        If side = 1 Then
            x3 = LVP : y3 = horizon
            x4 = Ax : y4 = Ay
        Else
            x3 = RVP : y3 = horizon
            x4 = Ax : y4 = Ay
        End If

        M12 = (y2 - y1) / (x2 - x1)
        M34 = (y4 - y3) / (x4 - x3)
        B12 = y1 - (x1 * M12)
        B34 = y3 - (x3 * M34)

        x0 = -1 * (B12 - B34) / (M12 - M34)
        y0 = M12 * x0 + B12
        Dx = x0
        Dy = y0

        'get C point
        x4 = Bx
        y4 = By

        M34 = (y4 - y3) / (x4 - x3)
        B34 = y3 - (x3 * M34)

        x0 = Dx
        y0 = M34 * x0 + B34
        Cx = x0
        Cy = y0

        Dim pnt1 As New Point(Ax, Ay)
        Dim pnt2 As New Point(Bx, By)
        Dim pnt3 As New Point(Cx, Cy)
        Dim pnt4 As New Point(Dx, Dy)
        Dim wall As Point() = {pnt1, pnt2, pnt3, pnt4}
        If side = 1 Then
            sidecolor = findcolor(1, 1)
            Dim brush1 As New SolidBrush(sidecolor) : g.FillPolygon(brush1, wall)
        Else
            sidecolor = findcolor(1, 1)
            Dim brush1 As New SolidBrush(sidecolor) : g.FillPolygon(brush1, wall)
        End If
        If Settings.Outlines = True Then g.DrawPolygon(stainedglasspen, wall)


    End Sub

    Private Sub buildpeakside(ByVal ax, ByVal ay, ByVal bx, ByVal by, ByVal cx, ByVal cy, ByVal dx, ByVal dy, ByVal ex, ByVal ey)
        Dim pt1 As New Point(ax, ay)
        Dim pt2 As New Point(bx, by)
        Dim pt3 As New Point(cx, cy)
        Dim pt4 As New Point(dx, dy)
        Dim pt5 As New Point(ex, ey)
        Dim wall As Point() = {pt1, pt2, pt3, pt4, pt5}
        Dim brush As New SolidBrush(Color.Empty)
        brush.Color = findcolor(1, 1)
        g.FillPolygon(brush, wall)
        If Settings.Outlines = True Then g.DrawPolygon(stainedglasspen, wall)

    End Sub
    Private Sub buildnonpeakside(ByVal ax, ByVal ay, ByVal bx, ByVal by, ByVal cx, ByVal cy, ByVal dx, _
    ByVal dy)
        Dim pt1 As New Point(ax, ay)
        Dim pt2 As New Point(bx, by)
        Dim pt3 As New Point(cx, cy)
        Dim pt4 As New Point(dx, dy)
        Dim wall As Point() = {pt1, pt2, pt3, pt4}
        Dim brush As New SolidBrush(Color.Empty)
        brush.Color = findcolor(2, 1)
        g.FillPolygon(brush, wall)
        If Settings.Outlines = True Then g.DrawPolygon(stainedglasspen, wall)


    End Sub


    Private Sub intersection(ByVal x1, ByVal y1, ByVal x2, ByVal y2, ByVal x3, ByVal y3, _
    ByVal x4, ByVal y4, ByRef X0, ByRef y0)

        Dim B12, B34, M12, M34 As Single

        If x2 = x1 Then
            X0 = x2
            M34 = (y4 - y3) / (x4 - x3)
            B34 = y3 - (x3 * M34)
            y0 = M34 * X0 + B34
            Exit Sub
        End If

        If x4 = x3 Then
            X0 = x4
            M12 = (y2 - y1) / (x2 - x1)
            B12 = y1 - (x1 * M12)
            y0 = M12 * X0 + B12
            Exit Sub
        End If

        M12 = (y2 - y1) / (x2 - x1)
        M34 = (y4 - y3) / (x4 - x3)
        B12 = y1 - (x1 * M12)
        B34 = y3 - (x3 * M34)

        X0 = -1 * (B12 - B34) / (M12 - M34)
        y0 = M12 * X0 + B12


    End Sub
    Private Sub basicdoor_windows(ByVal Ax, ByVal Ay, ByVal By, ByVal Fx, ByVal Fy, ByVal Dx, ByVal Dy, ByVal dd, ByVal ff, ByVal rvp, _
    ByVal lvp, ByVal rvpm, ByVal lvpm, ByVal distance, ByVal frontwidth, ByVal sidewidth)
        Dim doorstart, doorx, doory, s, L, ds, _
        dh, dw, toprtx, toprty, doorsill, doorox, dooroy As Single
        Dim doorht, side, doorwidth As Integer

        'choose which side to put door on
        '   If randsign(2) > 0 Then
        side = 2
        s = ff * 0.2 : L = ff * 0.7
        doorstart = randh(s, L)
        doorwidth = 4
        dw = pixelheight(doorwidth, distance)
        intersection(lvpm, horizon, Ax + doorstart, Ay, Ax, Ay, Fx, Fy, doorx, doory) ' get door start point
        intersection(lvpm, horizon, Ax + doorstart + dw, Ay, Ax, Ay, Fx, Fy, doorox, dooroy) ' get opp. side of door
        doorht = 7
        dh = pixelheight(doorht, distance)
        doorsill = 0.4
        ds = pixelheight(doorsill, distance)
        intersection(rvp, horizon, doorx, doory - dh, doorox, dooroy, doorox, 0, toprtx, toprty) 'get top right point of door
        joinfour(doorx, doory - ds, doorx, doory - dh, toprtx, toprty, doorox, dooroy - ds)

        'DOOR JAMB
        Dim jamwidth, jwid, topjwx, topjwy, rbotjx, rbotjy, lbotjx, lbotjy, rtopjx, rtopjy As Single
        jamwidth = 0.3
        jwid = pixelheight(jamwidth, distance)
        topjwx = toprtx - jwid  'starters
        topjwy = toprty
        intersection(lvp, horizon, doorox, dooroy - ds, topjwx, topjwy, toprtx, 0, rbotjx, rbotjy) 'get bottom right jamb point 
        intersection(rvp, horizon, rbotjx, rbotjy, doorx, doory - ds, doorx, 0, lbotjx, lbotjy) 'get bottom left point
        intersection(rvp, horizon, toprtx, toprty, rbotjx, rbotjy, rbotjx, 0, rtopjx, rtopjy) 'get top right point
        joinfour(rtopjx, rtopjy, toprtx, toprty, doorox, dooroy - ds, rbotjx, rbotjy)
        joinfour(doorox, dooroy - ds, rbotjx, rbotjy, lbotjx, lbotjy, doorx, doory - ds)

        'DOOR HANDLE
        Dim handlex, handley, inset, it, handsize, hz As Single
        inset = 0.8
        handsize = 0.25
        hz = pixelheight(handsize, distance)
        it = pixelheight(inset, distance)
        handley = dooroy - ds - (((dooroy - ds) - toprty) * 0.5)
        handlex = doorox - it
        Dim pen As New Pen(Color.Empty)
        pen.Color = findcolor(2, 2)
        g.DrawEllipse(pen, handlex, handley, hz, hz)

        'FRONT WINDOW
        'get widest side of door
        If doorx - Ax > Fx - doorox Then
            Dim closd, cl, wtoprx, wtopry, wtoplx, wtoply, wwx, wwy, vvx, vvy, mdlx, mdly, mdrx, mdry As Single
            'Choose 4 feet from door to start
            closd = 4
            cl = pixelheight(closd, distance)
            intersection(lvpm, horizon, Ax + doorstart - cl, Ay, Ax, Ay, Fx, Fy, wwx, wwy) 'get projection
            intersection(rvp, horizon, doorx, doory - dh, wwx, wwy, wwx, 0, wtoprx, wtopry) 'top right point
            intersection(lvpm, horizon, Ax + cl, Ay, Ax, Ay, Fx, Fy, vvx, vvy) 'get lower left proj.
            intersection(rvp, horizon, doorx, doory - dh, vvx, vvy, vvx, 0, wtoplx, wtoply) 'get top left point
            intersection(rvp, horizon, doorx, doory - dh / 3, vvx, vvy, vvx, 0, mdlx, mdly) 'get lower left point
            intersection(rvp, horizon, mdlx, mdly, wwx, wwy, wwx, 0, mdrx, mdry) 'get lower right point
            joinfour(wtoprx, wtopry, wtoplx, wtoply, mdlx, mdly, mdrx, mdry)

            'FRONT WINDOW CASING (OUTSIDE)
            Dim sillwidth, sill, topsx, topsy, botrx, botry, botlx, botly, toprx, topry As Single
            sillwidth = 0.3
            sill = pixelheight(sillwidth, distance)
            topsx = wtoprx - sill   'starters
            topsy = wtopry    '          "
            intersection(lvp, horizon, mdrx, mdry, topsx, topsy, topsx, 0, botrx, botry) 'get bottom right point
            intersection(rvp, horizon, mdlx, mdly, wtoplx, wtoply, wtoplx, 0, botlx, botly) 'get bottom left point
            intersection(botrx, botry, botrx, 0, wtoprx, wtopry, wtoplx, wtoply, toprx, topry)      'get top right point
            joinfour(wtoprx, wtopry, toprx, topry, botrx, botry, mdrx, mdry) 'paint right side
            joinfour(mdlx, mdly, botlx, botly, botrx, botry, mdrx, mdry)    'paint bottom

            'CROSS PIECES IN FRONT WINDOW
            Dim midx, midy, transomht, tr, lbarx, lbary, rbarx, rbary, centx, centy, botcentx, botcenty As Single
            transomht = 2
            tr = pixelheight(transomht, distance)
            lbarx = botlx
            lbary = botly - tr
            intersection(rvp, horizon, lbarx, lbary, botrx, botry, botrx, 0, rbarx, rbary) 'get right bar point
            intersection(wtoplx - sill, wtoply, botrx, botry, toprx, topry, botlx - sill, botly, midx, midy) 'center point of window
            intersection(midx, midy, midx, 0, lbarx, lbary, rbarx, rbary, centx, centy) 'center bar point
            intersection(midx, midy, midx, 0, botlx, botly, botrx, botry, botcentx, botcenty) 'bottom center point
            g.DrawLine(Pens.LightGray, toprx, topry, botrx, botry)
            g.DrawLine(Pens.LightGray, botrx, botry, botlx, botly)
            g.DrawLine(Pens.LightGray, lbarx, lbary, rbarx, rbary) 'crossbar
            g.DrawLine(Pens.LightGray, centx, centy, botcentx, botcenty) 'vertical bar
        End If

        'BACK WINDOW
        Dim bh, wh, ww, LLx, LLy, WLLx, WLLy, WULx, WULy, Wrx, Wry, WURx, WURy, WLRx, WLRy As Single
        bh = pixelheight(3.0, distance) 'beginning height
        ww = pixelheight(3.2, distance) 'width
        wh = pixelheight(5.0, distance) 'height
        intersection(rvpm, horizon, Ax - 0.7 * dd, Ay, Ax, Ay, Dx, Dy, LLx, LLy) 'get helper point for left side of window
        intersection(lvp, horizon, Ax, Ay - bh, LLx, LLy, LLx, 0, WLLx, WLLy) 'get lower left windowpoint
        intersection(lvp, horizon, WLLx, WLLy - wh, WLLx, WLLy, WLLx, 0, WULx, WULy) ' get upper left windowpoint
        intersection(rvpm, horizon, Ax - 0.7 * dd + ww, Ay, Ax, Ay, Dx, Dy, Wrx, Wry) 'get helper point for right side of window
        intersection(lvp, horizon, WULx, WULy, Wrx, Wry, Wrx, 0, WURx, WURy) 'get upper right point 
        intersection(lvp, horizon, WLLx, WLLy, WURx, WURy, WURx, 0, WLRx, WLRy) 'get lower right point
        joinfour(WLLx, WLLy, WLRx, WLRy, WURx, WURy, WULx, WULy)

        'BACK WINDOW CASING
        Dim sULx, sULy, sLLx, sLLy, sLRx, sLRy, sillwid, sil As Single
        sillwid = 0.3
        sil = pixelheight(sillwid, distance)
        intersection(rvp, horizon, WLLx, WLLy, WULx + sil, WULy, WULx + sil, 0, sLLx, sLLy) 'get lowerleft sill point
        intersection(lvp, horizon, WULx, WULy, sLLx, sLLy, sLLx, 0, sULx, sULy) 'get upperleft point
        intersection(lvp, horizon, sLLx, sLLy, WLRx, WLRy, WLRx, 0, sLRx, sLRy) 'get lowerright point
        joinfour(WULx, WULy, sULx, sULy, sLLx, sLLy, WLLx, WLLy)
        joinfour(WLLx, WLLy, sLLx, sLLy, sLRx, sLRy, WLRx, WLRy)

        'BACK WINDOW CROSS BARS
        Dim ctrx, ctry, UMpx, UMpy, BMpx, BMpy, LMpx, LMpy, RMpx, RMpy As Single
        intersection(sLLx, sLLy, WURx + sil, WURy, WLRx, WLRy, sULx, sULy, ctrx, ctry) 'get center of window
        intersection(lvp, horizon, WURx, WURy, ctrx, ctry, ctrx, 0, UMpx, UMpy) 'get upper middle point
        intersection(lvp, horizon, sLLx, sLLy, UMpx, UMpy, ctrx, ctry, BMpx, BMpy) 'get bottom middle point
        intersection(lvp, horizon, ctrx, ctry, sLLx, sLLy, sULx, sULy, LMpx, LMpy) 'get left middle point
        intersection(lvp, horizon, ctrx, ctry, WLRx, WLRy, WURx, WURy, RMpx, RMpy) 'get right middle point
        g.DrawLine(Pens.DarkGray, sULx, sULy, sLLx, sLLy)

        g.DrawLine(Pens.LightGray, UMpx, UMpy, BMpx, BMpy) 'vertical center bar
        g.DrawLine(Pens.LightGray, LMpx, LMpy, RMpx, RMpy) 'horizontal center bar
        g.DrawLine(Pens.LightGray, sULx, sULy, sLLx, sLLy) 'left vertical bar
        g.DrawLine(Pens.LightGray, sLLx, sLLy, sLRx, sLRy) 'bottom horizontal bar

        'BACK GAS METER
        Dim gasx, gasy, meter, mt, gasconnectx, gasconnecty, groundx, groundy As Single
        meter = 0.6
        mt = pixelheight(meter, distance)
        gasx = sLRx + 0.8 * (Ax - sLRx)
        gasy = Ay - 0.3 * (Ay - By) ' sLRy - 3

        g.FillEllipse(Brushes.Black, gasx, gasy, mt, mt)
        g.FillEllipse(Brushes.DarkGray, gasx - 1, gasy, mt, mt)
        gasconnectx = gasx + mt / 2
        gasconnecty = gasy + mt
        intersection(gasconnectx, gasconnecty, gasconnectx, 0, Ax, Ay, Dx, Dy, groundx, groundy) 'get ground point
        g.DrawLine(New Pen(Color.DarkSlateGray, 0.1), gasconnectx, gasconnecty, groundx, groundy)

    End Sub
    Private Sub chimney(ByVal distance, ByVal Ix, ByVal Iy, ByVal Jx, ByVal Jy, ByVal lvp, _
    ByVal lvpm, ByVal rvp, ByVal rvpm, ByVal uvpx, ByVal uvpy, ByVal Ex, ByVal Ey, ByVal Bx, ByVal By)
        Dim Len, Wid, Hgt As Integer
        Dim clx, cwx, chy, bgptx, bgpty, toplx, toply, botlx, botly, botrx, othx, othy, botry, toprx, topry, edgex, _
        edgey, roofx, roofy, botx, backptx, backpty, boty, topx, topy As Single
        Len = 2.0
        Wid = 1.1
        Hgt = randh(3, 5)
        clx = pixelheight(Len, distance)
        cwx = pixelheight(Wid, distance)
        chy = pixelheight(Hgt, distance)

        'find BEGINNING POINTS

        ' bgptx = randh(Ix + clx * 2.0, Jx - clx * 2.0) 'allow for chimney size
        ' bgpty = Iy - (Iy - Jy) * (bgptx - Ix) / (Jx - Ix)
        'simplified
        bgptx = Ix + (Jx - Ix) * 0.6 '+ randsign() * ((Jx - Ix) / 4)
        bgpty = (Iy - Jy) / 2
        'FIND BEGINNING Y POINT
        intersection(rvp, horizon, Ix, Iy, bgptx, bgpty, bgptx, 0, roofx, roofy) 'get roof point

        If Bx < bgptx Then  'IF USING FRONT ROOF EDGE
            intersection(rvp, horizon, Bx, By, bgptx, bgpty, bgptx, 0, edgex, edgey) 'get edge point
        Else
            intersection(uvpx, uvpy, Bx, By, bgptx, bgpty, bgptx, 0, edgex, edgey)
        End If
        botx = roofx
        boty = edgey - (edgey - roofy) * 0.6
        If boty > edgey Then boty = edgey
        topx = roofx
        topy = boty - chy


        intersection(botx - cwx, boty, botx - cwx, 0, uvpx, uvpy, botx, boty, botlx, botly) 'bottom left point
        intersection(lvp, horizon, topx, topy, botlx, botly, botlx, 0, toplx, toply) 'top left point
        intersection(botx + clx, boty, botx + clx, 0, rvp, horizon, botx, boty, botrx, botry) 'get bottom right point   
        intersection(uvpx, uvpy, Bx, By, botrx, botry, botrx, 0, othx, othy) 'other bottom right point
        If botry > othy Then botry = othy
        intersection(rvp, horizon, topx, topy, botrx, botry, botrx, 0, toprx, topry) 'get top right point
        intersection(lvp, horizon, toprx, topry, rvp, horizon, toplx, toply, backptx, backpty) 'get back point of chim.
        If topry > othy Then topry = othy

        joinfour(toplx, toply, topx, topy, toprx, topry, backptx, backpty) ' top hole
        joinfour(toprx, topry, topx, topy, botx, boty, botrx, botry)    'right side
        joinfour(toplx, toply, topx, topy, botx, boty, botlx, botly)    'left side

    End Sub

End Class
