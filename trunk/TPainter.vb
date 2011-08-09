Imports ColorWorkshop.modColors, ColorWorkshop.modCMBcolors

Public Class TPainter
    Private bottom As Integer
    Private g As Graphics = Nothing
    Private FImage As Image = Nothing
    Private FSettings As TSettings = Nothing
    Private cmbseason As Integer
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
    Dim rooftest As String = " " : Dim subject As String = " " : Dim rowhousetype As String = " " : Dim rht As String = ""
    Dim doorx, doorox, wtoplx, wtoprx, fdepthx, fdepthy, Bx, By, Cx, Cy, Dx, Dy, Ex, Ey, Fx, Fy, ff, Ix, Iy, _
    newAx, newAy, roofpty, lastmountainy As Single
    Dim stainedglasspen As New Pen(Color.Black, 1)
    Dim outlinepen As New Pen(Color.Black, 2)
    Dim colormem, skycolor, membasey, mem_x, mem_y As Integer
    Dim aerial, color14, M_color, M_gray, M_light, Mred, Mgreen, Mblue, tone, zAy, zFy, found, _
    LVP, RVP, RVPM, LVPM, distance, Ax, Ay, multib, gap As Integer
    Dim rooflttone, roofdktone, roofltcolor, roofdkcolor, chimneylttone, chimneyltcolor, _
    chimneydktone, chimneydkcolor, wallslttone, wallsltcolor, wallsdktone, wallsdkcolor, _
    decklttone, deckdktone, deckltcolor, deckdkcolor, rd, endhs As Integer
    Dim bgmtn As Integer = 215 'for background mountains
    Dim roadlx(8), roadly(8), roadrx(8), roadry(8), polebotx(8), poleboty(8), dist(8)

    Property Image() As Image
        Get
            Return FImage
        End Get
        Set(ByVal value As Image)
            FImage = value

            g = Graphics.FromImage(Image)
            ' g.SmoothingMode = Drawing2D.SmoothingMode.HighQuality
            g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias
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

    Protected Overrides Sub Finalize()
        MyBase.Finalize()
    End Sub

    Sub Make____Nice____Painting()

        FObjectRandom = New Random(Settings.ObjectSeed)
        FColorRandom = New Random(Settings.ColorSeed)
        aerial = yrand(100) 'what level of aerial perspective will exist

SingleEvents:
        If Settings.Cmb = True Then CMBcolordisplay() : Exit Sub
        If Settings.TintsShades = True Then colordisplayreg() : Exit Sub
        If Settings.Colorcode = True Then If FormSettings.scheme = 0 Then colorcode() : Exit Sub

ThePainting:

Horizon:
        If Settings.bigsky = True Then
            horizon = randh(Image.Height / 2, Image.Height * 0.75)
        Else
            horizon = randh(50, Image.Height * 0.5)
        End If

VanishingPts:
        Dim vp As Integer = randh(400, 700)
        RVP = vp
        LVP = RVP - 1200

TrialsAndTribulations:
        '__________________________________________
project1: If Settings.project1 = True Then
            Dim x0, y0, wid0, hgt0, botwidth, n As Integer
            x0 = 0 : y0 = 100 : wid0 = 125 : hgt0 = 250 : botwidth = wid0 / 10
            For n = 1 To 6
                rivertrees(x0, y0, wid0, hgt0)
                newtreetrunks(x0 + wid0 / 1.7, y0 + hgt0, botwidth, hgt0)
                x0 += 160
            Next
            Exit Sub
        End If

project2:
        If Settings.Project2 = True Then
            'colortestlarge()
            deciduous()
            Exit Sub
        End If

project3:
        If Settings.Project3 = True Then
            'testdistance()
            golf()

            Exit Sub
        End If

project4:
        If Settings.Project4 = True Then
            Flower()
            Exit Sub
        End If
        '________________________________________

        If Settings.Flowers = True Then

        End If

BeginningofLandscapePainting:
Sky:
        makeskycolor()
        If Settings.Clear = True Or Settings.Overcast = True Then
            If yrand(10) > 4 Then makeskymistyclouds()
            If yrand(10) > 6 Then makeskystriateclouds()
            If Settings.Overcast = True Then makeskyovercast()
        End If

        If Settings.Clouds = True Then
            If yrand(10) > 3 Then makeskymistyclouds()
            If yrand(10) > 3 Then makeskystriateclouds()
            If yrand(10) > 3 Then makeskyfluffclouds()
            If yrand(10) > 2 Then makeskycumulusclouds()
        End If

BackgroundMountains:
        If Settings.Mountainous = True Then
            makehorizonmountains()
        End If

Sea_or_land:
        If Settings.Sea = True Then
            makeocean()
            makeskyfluffclouds()
        Else
            makeland(1)
        End If
        If Settings.Mist = True Then makemist(5)

DistantMountains:
        If Settings.Mountainous = True Then makedistantmountains()
        If Settings.Mist = True Then makemist(5)

MajorMountain:
        If Settings.Mountainous = True Then
            'nothing here yet
        End If

RollingHills:
        If Settings.Hills = True And Settings.River = False Then rollinghill()

Prairie:
        If Settings.Flat = True And Settings.River = False Then

        End If

Localscenes:
        If Settings.Home = True Then
            homes()
            Exit Sub
        End If

        If Settings.Farm = True Then
            'nothing here yet
        End If
River:
        If Settings.River = True Or Settings.Stream = True Then
            makeriveraswinding()
            If Settings.Mountainous = True Then
                makerivermountains(randh(17, 30))   'mountainous
            ElseIf Settings.Flat = True Then
                makerivermountains(0)               'prairie
            Else : makerivermountains(randh(6, 13)) 'rolling hills
            End If



            Dim ifn As Integer = yrand(100)
            If ifn > 30 And ifn < 50 Then makerivertrees()
            If Settings.Mist = True Then makemist(5)
            If ifn > 70 Then lonepine() : lonepine()
            If ifn < 20 Then birchstand()
            Dim gt As Integer = randh(200, 600)
            If ifn > 20 And ifn < 30 Then
                If gt > 580 Then
                    bush(gt, Image.Height, 50)
                End If
            End If
        End If


    End Sub
    Private Sub golf()
        Dim holedistance, totwindstrength, windstrength, hole_elevation, distance_w_elev, _
        rough_lie, spin, clubpower, windlieclub As Integer
        Dim roughclub, windfactor, factor1, Ffactor, difference, fdifference As Single
        Dim clubselection As String = "5i"
        Dim winddirection As String = "into"
        Dim swingselection As String = "chip"
        Dim fullclubselection As String
        Dim holeID As String
        Dim a, b As String
        b = "_________________________________________________________________________________"

Enterdetails:

        'Enter the following details
        holeID = "Kiawah 7"
        hole_elevation = 7      'in feet
        holedistance = 29      'in yards
        totwindstrength = 0    'in mph. 
        windstrength = 0       'Remember how you got it for formula.
        winddirection = "into"    '(i means facing into the wind, "w"(with)means the wind is coming from behind you.)
        clubselection = "PW"    'select details from below
        swingselection = "pitch" ' out of full, pitch, punch, chip
        clubpower = 37
        spin = 0    '(out of 3, more spin = 3)
        rough_lie = 0

        a = "____________________________________________________________________________________"

        'ClubPower:  
        '        Club   Full    Punch   Pitch   Chip    Flop
        '        D      235     205
        '        3WD    222     206
        '        3H     202     190
        '        3i     200     185     85
        '        4i     187     170     69
        '        5i     175     155     60      41
        '        6i     160     143     56      36
        '        7i     145     128     50      31
        '        8i     130     115     45      27
        '        9i     120     107     41      24  
        '        PW     110     95      37      20
        '        SW     70      59      22      11      24
        '        60W    55      45      15      7       18


        fullclubselection = clubselection + "   " + CStr(clubpower) + " "

EFFECTS_ON_HOLE_DISTANCE:

Elevation_Effects:
        'usually measured in feet but is also seen in inches.  The importance of small distances remains to be seen.
        'I presently do not know how to make use of this factor in determining how much power a ball should be hit with.
        'therefore I deal with theories and trials.
        'maybe use angle from ball to hole and multiply
        Dim s_elevation_factor As String = "D=D+or-elev"

        If hole_elevation > 0 Then distance_w_elev = holedistance + hole_elevation
        If hole_elevation < 0.1 Then distance_w_elev = holedistance - hole_elevation

EFFECTS_ON_CLUB_DISTANCE:

RoughLie_Effects:  'the rough_lie factor reduces the distance of the club

        Dim s_roughfactor As String = "1st no.%"
        roughclub = clubpower - rough_lie 'lie = either 10,20,30,40 etc, the first number of the lie.

Wind_Effects:
        'determining windfactor and the new yardage capacity or clubpower of the club, winddegrees will be used later.
        'will likely be providing more accurate estimates of wind with an against me, but presently just taking 75% of share to affect the ball flight.

        Dim s_windportion As String = CStr(windstrength) + "/" + CStr(totwindstrength)

        If winddirection = "with" Then windfactor = (0.005 * windstrength) + 1
        If winddirection = "into" Then windfactor = (100 - windstrength) / 100

        windlieclub = roughclub * windfactor

SpinEffects:
        'waiting to find out what these effects might on the flight distance of the golf ball

Difference:  'between modified club potential and modified distance to hole:

        difference = windlieclub - distance_w_elev

Factors:  'to work with to help to eliminate the difference between demand and capability
        'can play with this.  Working with the difference alone did not help in the beginning

        factor1 = 0.5
        fdifference = difference * factor1

        Ffactor = fdifference / windlieclub

        Ffactor = Ffactor * 10 'brings it to tenths of a centimeter
        'what portion of power bar to stop at, thereby reducing 

Convert_to_Strings:

        Dim s_factor1 As String = CStr(factor1)
        Dim s_ffactor As String = CStr(Ffactor)
        Dim s_holeelevation As String = CStr(hole_elevation)
        Dim s_holedistance As String = CStr(holedistance)
        Dim s_windstrength As String = CStr(windstrength)
        Dim s_winddirection As String = CStr(winddirection)
        Dim s_clubselection As String = CStr(clubselection)
        Dim s_fullclubselection As String = CStr(fullclubselection)
        Dim s_rough_lie As String = CStr(rough_lie)
        Dim s_clubpower As String = CStr(clubpower)
        Dim s_roughclub As String = CStr(roughclub)
        Dim s_windlieclub As String = CStr(windlieclub)
        Dim s_spin As String = CStr(spin)
        Dim s_swingselection As String = CStr(swingselection)
        Dim s_difference As String = CStr(difference)
        Dim s_distance_w_elev As String = CStr(distance_w_elev)

Finalsummary:
        Dim s_endclubsummary As String = "No comment"
        'the club along with its reduced power swing, or a statement that a lower numbered club be used.
        If Ffactor >= -0.0 Then
            s_endclubsummary = s_swingselection + "  " + clubselection + "  " + s_ffactor
        ElseIf Ffactor < 0 Then
            s_endclubsummary = "Choose a higher club this hole"
        End If

        Dim para00 As String = "Hole Challenge:------------------" + holeID
        Dim para01 As String = "Club Considered:--------------------------------------------------------  " + s_fullclubselection
        Dim para1a As String = "Swing selection:------------------------------------------------  " + s_swingselection
        Dim para02 As String = "Distance to hole:-------------------------------------------------------------------------  " + s_holedistance
        Dim para03 As String = "Hole elevation:---------------------------------------------------  " + s_holeelevation
        Dim para04 As String = "Elevation factor applied to distance-------------  " + s_elevation_factor
        Dim para05 As String = "New distance from elevation factor--------------------------------------------------  " + s_distance_w_elev
        Dim para06 As String = "Rough/Sand lie:-------------------------------------------------  " + s_rough_lie
        Dim para07 As String = "Rough or Sand factor:------------------------------  " + s_roughfactor
        Dim para08 As String = "Changed club from Rough/Sand:------------------------------------------  " + s_roughclub
        Dim para09 As String = "Wind direction used:-------------------------------------------  " + s_winddirection
        Dim para10 As String = "Wind strength (portion used out of total):----------------  " + s_windportion
        Dim para11 As String = "Changed club from wind:-----------------------------------------------------  " + s_windlieclub
        Dim para12 As String = "Difference bet. final club and hole distance:---------------------  " + s_difference
        Dim para13 As String = "Factor1 to manipulate the difference-------------  " + s_factor1
        Dim para14 As String = "Factor2: Final difference/Final club,*10--------------  " + s_ffactor
        Dim para15 As String = "Recommendations re club:-----------------------------------------------  " + s_endclubsummary
        Dim para16 As String = "Make certain hole marker is adjusted for wind effects and green approach to the hole"
        Dim para17 As String = "Record results below of golf stroke, showing cut or slice and landing and ending results:"


        Dim aFont As New System.Drawing.Font(" Arial", 10, FontStyle.Bold)
        g.DrawString(para00, aFont, Brushes.Black, 55, 29)
        g.DrawString(para01, aFont, Brushes.Black, 75, 50)
        g.DrawString(para1a, aFont, Brushes.Black, 75, 71)
        g.DrawString(para02, aFont, Brushes.Black, 75, 92)
        g.DrawString(para03, aFont, Brushes.Black, 75, 113)
        g.DrawString(para04, aFont, Brushes.Black, 75, 134)
        g.DrawString(para05, aFont, Brushes.Black, 75, 155)
        g.DrawString(para06, aFont, Brushes.Black, 75, 176)
        g.DrawString(para07, aFont, Brushes.Black, 75, 197)
        g.DrawString(para08, aFont, Brushes.Black, 75, 218)
        g.DrawString(para09, aFont, Brushes.Black, 75, 239)
        g.DrawString(para10, aFont, Brushes.Black, 75, 260)
        g.DrawString(para11, aFont, Brushes.Black, 75, 281)
        g.DrawString(para12, aFont, Brushes.Black, 75, 302)
        g.DrawString(para13, aFont, Brushes.Black, 75, 323)
        g.DrawString(para14, aFont, Brushes.Black, 75, 344)
        g.DrawString(para15, aFont, Brushes.Black, 75, 365)
        g.DrawString(para16, aFont, Brushes.Black, 75, 410)
        g.DrawString(para17, aFont, Brushes.Black, 75, 431)


    End Sub
    Private Sub testdistance() '        using simple right angle style - or railway track
        'width = (lessht / landht) * bottomwidth
        g.DrawLine(Pens.Green, 0, horizon, Image.Width, horizon)
        Dim x0, beginy, incY, i, j, riverwidth, sway, maxsway As Integer
        Dim bottomwidth, y0, lessht, landht As Integer
        Dim treehgt, p_treeht, p_treewth, x0t, y0t, x, botwidth As Integer
        Dim rwidth, dfactor, rivpointL_x(21), rivpointL_y(21), rivpointR_x(21), rivpointR_y(21), strleftx(21), strlefty(21), strrightx(21), strrighty(21) As Single
        Dim box1 As New Rectangle
        Dim box2 As New Rectangle
        Dim river_path As New Drawing2D.GraphicsPath
        horizon = 290
        beginy = horizon + yrand(3) + 1
        y0 = beginy
        x0 = randh(Image.Width * 0.1, Image.Width * 0.4)
        bottomwidth = 700 'pixels
        riverwidth = 70 'feet across the river
        landht = Image.Height - horizon
        incY = landht / 40
        dfactor = bottomwidth / landht
        maxsway = 20 'rwidth / 8

        For i = 0 To 20
            lessht = y0 - horizon '                        
            rwidth = lessht * dfactor
            sway = rwidth * ((randh(-maxsway, maxsway) / 60)) * ((20 - j) / 20)
            x0 += sway
            strleftx(i) = x0
            strlefty(i) = y0
            strrightx(i) = x0 + rwidth '+ sway
            strrighty(i) = y0
            y0 += incY + i * 2
        Next i
        'For riverwidth2 As Integer = 1 To 2
        '
        Dim point0 As New Point(strleftx(0), strlefty(0))
        Dim point1 As New Point(strleftx(0) - yrand(5), strlefty(1))
        Dim point2 As New Point(strleftx(2), strlefty(2))
        Dim point3 As New Point(strleftx(2) - yrand(5), strlefty(3))
        Dim point4 As New Point(strleftx(4), strlefty(4))
        Dim point5 As New Point(strleftx(4) - yrand(5), strlefty(5))
        Dim point6 As New Point(strleftx(6), strlefty(6))
        Dim point7 As New Point(strleftx(6) - yrand(6), strlefty(7))
        Dim point8 As New Point(strleftx(8), strlefty(8))
        Dim point9 As New Point(strleftx(8) - yrand(8), strlefty(9))
        Dim point10 As New Point(strleftx(10), strlefty(10))
        Dim point11 As New Point(strleftx(10) - yrand(10), strlefty(11))
        Dim point12 As New Point(strleftx(12), strlefty(12))
        Dim point13 As New Point(strleftx(12) - yrand(12), strlefty(13))
        Dim point14 As New Point(strleftx(14), strlefty(14))
        Dim point15 As New Point(strleftx(14) - yrand(14), strlefty(15))
        Dim point16 As New Point(strleftx(16), strlefty(16))
        Dim point17 As New Point(strleftx(16) - yrand(16), strlefty(17))
        Dim point18 As New Point(strleftx(18), strlefty(18))
        Dim point19 As New Point(strleftx(18) - yrand(18), strlefty(19))
        Dim point20 As New Point(strleftx(20), strlefty(20))

        Dim point21 As New Point(strrightx(0), strrighty(0))
        Dim point22 As New Point(strrightx(1), strrighty(1))
        Dim point23 As New Point(strrightx(1) + yrand(5), strrighty(2))
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

        ' follow river edge down on left side then up on right side for best computer line curvature

        'Dim rivercurvePoints As Point() = {point0, point1, point2, point3, point4, _
        'point5, point6, point7, point8, point9, point10, point11, point12, _
        'point13, point14, point15, point16, point17, point18, point19, point20, _
        'point41, point40, point39, point38, point37, point36, point35, point34, _
        'point33, point32, point31, point30, point29, point28, point27, point26, _
        'point25, point24, point23, point22, point21}

        Dim rivercurvePoints As Point() = {point0, point2, point4, _
                point6, point8, point10, point12, _
                point14, point16, point18, point20, _
                point41, point39, point37, point35, _
                point33, point31, point29, point27, _
                point25, point23, point21}

        river_path = New Drawing2D.GraphicsPath
        river_path.AddCurve(rivercurvePoints, 0.2)
        ' beach_path
        'height = 10
        'box1 = New Rectangle(x0, y0, rwidth, height)
        'g.FillRectangle(Brushes.Blue, box1)
        g.SetClip(river_path)
        Dim fpht As Single = river_path.GetBounds.Height
        colorblend(river_path.GetBounds.X, river_path.GetBounds.Y, _
        river_path.GetBounds.Width, river_path.GetBounds.Height, "river", 0)
        g.ResetClip()
        'For i = 0 To 20
        '    box1 = New Rectangle(strleftx(i), strlefty(i), 5, 5)
        '    box2 = New Rectangle(strrightx(i), strrighty(i), 5, 5)
        '    g.FillRectangle(Brushes.Red, box1)
        '    g.FillRectangle(Brushes.Red, box2)
        'Next
        '       place rivertrees:
        For i = 0 To 20
            If i = 10 Or i = 5 Or i = 15 Then
                For x = 1 To 2
                    treehgt = 60 'ft
                    p_treeht = (treehgt / riverwidth) * (lessht * dfactor)
                    p_treewth = p_treeht * 0.4
                    x0t = x0 - p_treewth : If x = 2 Then x0t = x0 + rwidth + p_treewth
                    y0t = y0 - p_treeht
                    botwidth = p_treewth / 10
                    ' rivertrees(x0t, y0t, p_treewth, p_treeht)
                    ' newtreetrunks(x0 + wid0 / 1.7, y0 + hgt0, botwidth, hgt0)
                    ' newtreetrunks(x0t + p_treewth / 1.7, y0t + p_treeht, botwidth, p_treeht)
                Next

                g.DrawRectangle(Pens.Red, box1)
            End If
            y0 += incY
        Next
        ' y = (v * height * 800) / distance

    End Sub

    Private Sub colorsummary()
        'Declare colour palette used by the painting
        'Declare colour palette used by the painting
        Dim colorsummary As String = ""
        colorsummary = FormSettings.colorscheme & " "
        If Mid(colorsummary, 1, 3) <> "CMB" Then
            For i = 0 To 6
                colorsummary = colorsummary & colorname(FormSettings.colours(i))
                If FormSettings.colours(i + 1) = 0 Then GoTo colorsum
                If i < 6 Then
                    If FormSettings.colours(i + 1) > 0 Then
                        colorsummary = colorsummary & ", "
                    End If
                End If
            Next
        End If
colorsum:
        Dim aFont As New System.Drawing.Font(" Arial", 8, FontStyle.Italic)
        g.DrawString(colorsummary, aFont, Brushes.Black, 8, 8)
        ' g.DrawString(CStr(yrand(3)), aFont, Brushes.Black, 8, 18)
    End Sub
    Private Sub makehomescene() ' presently a project
        ellipseondark()
        'makeskyfluffcover(300, 100, 100, 80)
        Lombardy()
        'deciduous()
        ''setupbuildingcolors()
        ''rowhousetype = "single"
        ''placebuilding("single")

        'xmastree(30, 400, 70)

    End Sub
    Private Sub homes()
        If yrand(10) > 5 Then 'single building
            setupbuildingcolors()
            rowhousetype = "single"
            placebuilding("single")

        Else ' inlne rowhousing
            Dim van, r, ht As Integer
            r = randsign()
            van = randh(5, 50) * r

            For multib = 0 To 7
                ht = randh(0, 2)
                If ht = 0 Then
                    rht = "withdoorstep"
                ElseIf ht = 1 Then
                    rht = "withlowdeck"
                ElseIf ht = 2 Then
                    rht = "withoutdeckorstep"
                End If
                rowhousetype = rht

                RVP = RVP + van
                setupbuildingcolors()
                placebuilding("inline")
            Next
            roadfill()
            pole()

        End If
    End Sub
    Private Sub farm()

    End Sub
    Private Sub xmastreeforest()

        Dim x0 As Integer
        Dim y0, b As Single
        horizon = randh(100, 250)
        makeskycolor()
        makedistantmountains()
        Dim n As Integer
        y0 = horizon
        Dim rect As New Rectangle(x0, y0, Image.Width, 100)
        Dim dense As New SolidBrush(Color.Empty)
        dense.Color = ModCMBcolors.getCMBcolor(3, 2, 7)
        Mred = dense.Color.R : Mgreen = dense.Color.G : Mblue = dense.Color.B
        makeland(1) '0 means lighter colors

        b = 600 'distance
        y0 = y0 + 10

        Do While b > 20
            For n = 1 To b / 10
                x0 = yrand(Image.Width)
                xmastree(x0, y0, b)
            Next
            b -= 20
            y0 = y0 + 0.005 * y0
            If Settings.Winter = True Then If b > 20 Then makemist(3)
            If Settings.Mist = True Then If b > 20 Then makemist(1)
        Loop
        'bush(yrand(Image.Width), 350, 15)
        If Settings.Winter = True Then  'for snow falling
            Dim sfw, ellip, sfh As Integer
            Dim rec As New Rectangle
            For ellip = 1 To 300
                y0 = yrand(Image.Height)
                sfw = randh(2, 4) : sfh = randh(2, 4)
                rec = New Rectangle(yrand(Image.Width), y0, sfw, sfh)
                g.FillEllipse(Brushes.White, rec)
            Next

            horizon = randh(100, 250)
            makeskycolor()
            makedistantmountains()
            y0 = horizon
            rect = New Rectangle(x0, y0, Image.Width, 100)
            dense = New SolidBrush(Color.Empty)
            dense.Color = ModCMBcolors.getCMBcolor(3, 2, 7)
            Mred = dense.Color.R : Mgreen = dense.Color.G : Mblue = dense.Color.B
            makeland(1) '0 means lighter colors

            b = 600 'distance
            y0 = y0 + 10

            Do While b > 20
                For n = 1 To b / 10
                    x0 = yrand(Image.Width)
                    xmastree(x0, y0, b)
                Next
                b -= 20
                y0 = y0 + 0.005 * y0
                If Settings.Winter = True Then If b > 20 Then makemist(3)
                If Settings.Mist = True Then If b > 20 Then makemist(1)
            Loop
        End If
    End Sub
    Private Sub black()
        Dim area As New Rectangle(0, 0, Image.Width, Image.Height)
        Dim brush As New SolidBrush(Color.Black)
        g.FillRectangle(brush, area)

    End Sub
    Private Sub tulipbuds()
        Dim n, red, green, blue, colsymb As Integer
        Dim recpath As New Drawing2D.GraphicsPath
        Dim ellpath As New Drawing2D.GraphicsPath
        Dim brushfl As New SolidBrush(Color.Empty)
        recpath.AddBezier(New Point(100, 70), New Point(240, 60), New Point(75, 225), New Point(93, 70))
        black()
        If FormSettings.scheme > 9 Then
            findcolor(3)
            red = Mred : green = Mgreen : red = Mred
        Else
            colsymb = randh(0, FormSettings.scheme)
            M_color = FormSettings.colours(colsymb)
            brushfl.Color = modColors.getcolor(M_color, 1, 5)
            red = brushfl.Color.R : green = brushfl.Color.G : blue = brushfl.Color.B
        End If
        For n = 1 To 12
            '  ellpath = recpath.Clone
            g.TranslateTransform(randh(-1, 80), randh(-10, 10))
            brushfl.Color = Color.FromArgb(red, green, blue)
            g.FillPath(brushfl, recpath)
            ' inflate(15, 15)
            green += randh(-30, 30)
            If green > 255 Then green = 255
            If green < 0 Then green = 0
            red += randh(-30, 30)
            If red > 255 Then red = 255
            If red < 0 Then red = 0
            blue += randh(-30, 30)
            If blue > 255 Then blue = 255
            If blue < 0 Then blue = 0
        Next


    End Sub

    Private Sub Flower()

        Dim x0, y0, width, height, rotatex, rotatey, colsymb, m_color, a, red, green, blue As Integer
        x0 = Image.Width / 3
        y0 = Image.Height * 0.3
        width = 130
        height = width * 0.4
        Dim brush As New SolidBrush(Color.Fuchsia)
        If FormSettings.scheme > 9 Then
            findcolor(3)
            red = Mred : green = Mgreen : red = Mred
        Else
            colsymb = randh(0, FormSettings.scheme)
            m_color = FormSettings.colours(colsymb)
            brush.Color = modColors.getcolor(m_color, 1, 5)
            red = brush.Color.R : green = brush.Color.G : blue = brush.Color.B
        End If
        Dim flpath As New Drawing2D.GraphicsPath

        Dim rect As New Rectangle
        rect = New Rectangle(x0, y0, width, height)
        Dim endpt As Integer = 40
        Dim adegrees As Integer = 39
        For a = 1 To endpt
            rotatex = x0 + width * 1.2
            rotatey = y0 + (height / 1.8)
            'flpath.AddEllipse(x0, y0, width, height)
            ' g.SetClip(flpath)
            brush.Color = Color.FromArgb(red, green, blue)
            g.FillEllipse(brush, rect)
            'Dim blendbrushv As New Drawing2D.LinearGradientBrush(area, _
            '          Color.FromArgb(n, uppercolor.r, uppercolor.g, uppercolor.b), _
            '          Color.FromArgb(255, lowercolor.R, lowercolor.G, lowercolor.b), _
            '                  Drawing2D.LinearGradientMode.Vertical)
            'g.FillRectangle(blendbrushv, area)
            g.ResetClip()
            Rotate(rotatex, rotatey, adegrees)
            If a = 10 Or a = 20 Or a = 30 Then
                ' endpt = endpt + yrand(3)
                x0 += 30
                width -= 50
                height = width * 0.35
                adegrees += yrand(5)
                red += randh(-30, 30)
                If red > 255 Then red = 255
                If red < 0 Then red = 0
                green += randh(-30, 30)
                If green > 255 Then green = 255
                If green < 0 Then green = 0
                blue += randh(-30, 30)
                If blue > 255 Then blue = 255
                If blue < 0 Then blue = 0
            End If
        Next





        'Lombardy()
        'lonepine()
        'g.FillRectangle(Brushes.Chocolate, 200, 250, 5, 5)
        'pinebough(200, 250, 5, 30)
        ' g.DrawLine(Pens.Blue, 0, horizon, Image.Width, horizon)
        ' makemidpines() 'pie shapes, good start

        ' bezierbranches() 'wierd,wonderful
        'For i = 1 To 10
        '    deciduous() 'good start
        'Next
        ' gnarledtree(300, 400, 150)
        'rivertrees(300, 200, 100, 300)
        ' makeskyevening()
        'trial_distanceformula()
        'gas()
        '  If Settings.Fog = True Then makemist()
        ' makeskyfluffclouds()

        'treeheight()
        'treestand()
        '   aspengrove()
        'makebezierspears()
        'makewaterfall()
        'trial()
        ' makepathpoints()
        ' CMBcolordisplay()
        'findgray(3)

    End Sub
    Private Sub setupbuildingcolors()
        If FormSettings.scheme > 9 Then
            setuphousecolorsCMB("walls")
            setuphousecolorsCMB("roof")
            setuphousecolorsCMB("chimney")
            setuphousecolorsCMB("deck")
        Else
            setuphousecolorsREG("walls")
            roofltcolor = wallsltcolor
            Do While roofltcolor = wallsltcolor
                setuphousecolorsREG("roof")
            Loop
            setuphousecolorsREG("chimney")
            setuphousecolorsREG("deck")
        End If
    End Sub
    Private Sub placebuilding(ByVal subject) 'from make___etc.
        Dim cd, de, ef, fg As Integer
        If subject = "single" Then
            Ax = yrand(RVP - 80)
            Ay = randh(horizon + 40, horizon + 150)
            LVPM = LVP + (Ax - LVP) * 0.3  'left measuring point
            RVPM = Ax + (RVP - Ax) * 0.45   'right measuring point
            distance = randh(35, 170)

        End If

        If multib = 0 And subject <> "single" Then 'first building stats
            cd = RVP - 40
            de = RVP - 180
            If RVP < Image.Width Then
                Ax = randh(de, cd)
            Else
                Ax = yrand(de)
            End If
            gap = randh(20, 27)
            ef = horizon + 1 : fg = horizon + 40
            If Settings.Mountainous = True Then ef = lastmountainy : fg = ef + 40
            Ay = randh(ef, fg)
            LVPM = LVP + (Ax - LVP) * 0.45  'left measuring point
            RVPM = Ax + (RVP - Ax) * 0.45   'right measuring point
            intersection(Ax, Ay, RVP, horizon, Ax - gap, Ay, Ax - gap, 0, newAx, newAy) 'prepare for another inline building
            distance = randh(1000, 1600)
        End If

        If subject = "inline" Then
            Ax = newAx
            If Ax < -150 Then

                Exit Sub
            End If

            Ay = newAy
            LVPM = LVP + (Ax - LVP) * 0.55  'left measuring point
            RVPM = Ax + (RVP - Ax) * 0.45   'right measuring point
            intersection(Ax, Ay, RVP, horizon, Ax - gap, Ay, Ax - gap, 0, newAx, newAy) 'prepare for another inline building
            distance *= 0.65
            gap *= 1.8
        End If

        housedesign()

    End Sub

    Private Sub makemultiplebuildings()
        Dim bdist As Integer = randh(50, 200)
        Dim ay, ax, depth, portion, distance As Single
        depth = randh(1000, 3000)
        For ay = horizon - 100 To Image.Height Step 40
            ' ay = horizon + 200
            ax = randh(100, Image.Width - 100)
            portion = ay - horizon
            distance = (1.0 - (portion * 5 / depth)) * 1000
            ' the smaller the depth, the deeper the building
            ' distance = 100
            housedesign()
        Next

    End Sub


    Private Sub makehorizonmountains()
        Dim mtptsx(32), mtptsy(32)
        Dim mtnpath As New Drawing2D.GraphicsPath
        Dim k, line_extent, mtns, startx, starty As Integer
        line_extent = randh(0, 3) 'extent limits of individual line lengths
        For x As Integer = 1 To (randh(3, 6))
            mtns += 1
            mtptsx(0) = randh(-30, Image.Width - 20)
            mtptsy(0) = horizon
            starty = mtptsy(0)

            j = 1 : k = 10

            For i = j To k
                mtptsx(i) = mtptsx(i - 1) + randh(3, line_extent)
                mtptsy(i) = mtptsy(i - 1) - randh(1, line_extent)
            Next i
            j = 11 : k = 20
            For i = j To k
                mtptsx(i) = mtptsx(i - 1) + randh(20, 50)
                mtptsy(i) = mtptsy(i - 1) + randh(2, 10) * randsign()
            Next i
            j = 21 : k = 30
            For i = j To k
                mtptsx(i) = mtptsx(i - 1) + randh(3, line_extent + i / 2)
                mtptsy(i) = mtptsy(i - 1) + randh(3, line_extent)
                If mtptsy(i) > mtptsy(0) Then mtptsy(i) = mtptsy(0)
            Next i
            'line_extent *= randh(7, 14) / 10

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
            Dim point32 As New Point(startx, starty)
            Dim curvepoints As Point() = {point0, point1, point2, point3, point4, _
            point5, point6, point7, point8, point9, point10, point11, point12, _
            point13, point14, point15, point16, point17, point18, point19, _
            point20, point21, point22, point23, point24, point25, point26, point27, _
            point28, point29, point30, point31}
            mtnpath = New Drawing2D.GraphicsPath
            mtnpath.AddClosedCurve(curvepoints, 0.3F)
            Dim subject As String = "bg_mountains"
            g.SetClip(mtnpath)
            colorblend(mtnpath.GetBounds.X, mtnpath.GetBounds.Y, mtnpath.GetBounds.Width, _
                        mtnpath.GetBounds.Height, subject, 0)
            g.ResetClip()
            If Settings.Outlines = True Then g.DrawPath(stainedglasspen, mtnpath)
            '  If mtptsx(17) > Image.Width Then Exit Do


        Next

    End Sub
    Private Sub makeland(ByVal type)

        colorblend(0, horizon - 2, Image.Width, Image.Height, "land", type)
        horizonmist()

    End Sub
    Private Sub horizonmist()
        Dim x1, y1, x2, y2, x3, y3, x4, y4, hgt As Integer
        hgt = 8
        x1 = -50
        y1 = horizon - hgt
        x2 = Image.Width / 2
        y2 = horizon - hgt
        x3 = x2
        y3 = horizon + hgt
        x4 = x1
        y4 = y3
        For n As Integer = 1 To 2
            Dim point1 As New Point(x1, y1) : Dim point2 As New Point(x2, y2)
            Dim point3 As New Point(x3, y3) : Dim point4 As New Point(x4, y4)
            Dim rect_pts As Point() = {point1, point2, point3, point4}
            Dim ellipse_path As New Drawing2D.GraphicsPath()
            Dim path_brush As New Drawing2D.PathGradientBrush(rect_pts)
            ellipse_path.AddEllipse(x1, y1, x2, hgt * 2)
            path_brush = New Drawing2D.PathGradientBrush(ellipse_path)
            path_brush.CenterColor = Color.Gray
            path_brush.SurroundColors = New Color() {Color.Empty}
            g.FillEllipse(path_brush, x1, y1, x2, hgt)
            ellipse_path.Reset()
            x1 = Image.Width / 4
            x2 = Image.Width * 0.67
            x3 = x2
            x4 = x1
        Next
    End Sub
    Private Sub rollinghill()
        'BUILD LANDSHAPE (begy,numdiv,yvar,grade,distance)
        'makefillspace()
        'Exit Sub
        Dim begy, numdiv, yvar, grade, typ, curv, dist As Integer
        begy = horizon + yrand(100)
        Dim i As Integer = randh(1, 2)
        For n As Integer = 0 To i
            dist = 700 - n * 10
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

    End Sub

    Private Sub vanishingpoints(ByVal x1, ByVal y1, ByVal angle1, ByRef LMP, ByRef RMP)

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

        For j = 1 To 2 'do 7 lines across the picture
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
            ' Settings.Colours(0) = Settings.DominantColor
            If FormSettings.scheme > 9 Then
                Dim light, colors As Integer
                Dim cmbseason As Integer = FormSettings.scheme - 10
                light = c_randh(0, 2)
                Dim brush As New SolidBrush(Color.Orange)
                colors = c_randh(1, 14)
                brush.Color = ModCMBcolors.getCMBcolor(cmbseason, light, colors)
                g.FillClosedCurve(brush, curvePoints)
            Else
                Dim colsymb, domin As Integer
                Dim tension As Single = 1
                Dim brush As New SolidBrush(Color.Orange)
                Dim scheme As Integer = FormSettings.scheme
                Dim colorchoice, colorlevel, grayness As Integer
                If FormSettings.scheme > 4 Then domin = 1 Else domin = 2
                colsymb = c_randh(0, scheme * domin)
                If colsymb > FormSettings.scheme Then colsymb = 0 'setting dominance through weighting
                colorchoice = FormSettings.colours(colsymb)
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

        Dim sky As New Rectangle(0, 0, Image.Width, horizon + 5)

CMB:
        If FormSettings.scheme > 9 Then
            Dim cmbseason As Integer = FormSettings.scheme - 10
            Dim light1 As Integer = c_randh(0, 2)
            Dim colors1 As Integer = c_randh(0, 13)

            Dim skybrush As New SolidBrush(Color.Orange)
            skybrush.Color = ModCMBcolors.getCMBcolor(cmbseason, light1, colors1)
            g.FillRectangle(skybrush, sky)

            ' paint light gradient in sky
            Dim Brush3 As New Drawing2D.LinearGradientBrush(sky, _
                    Color.FromArgb(10, 255, 255, 255), _
                    Color.FromArgb(210, 255, 255, 255), _
                    Drawing2D.LinearGradientMode.Vertical)
            Dim k As Integer = 0
            If horizon < Image.Height / 3 Then k = 3 Else k = 2
            For n As Integer = 1 To k
                g.FillRectangle(Brush3, sky)
            Next
        Else
nonCMB:

            Dim colsymb, colsymb2, domin As Integer
            Dim brush As New SolidBrush(Color.Orange)
            Dim scheme As Integer = FormSettings.scheme
            Dim colorchoice, colorchoice2, colorlevel, grayness As Integer
            If FormSettings.scheme > 4 Then domin = 1 Else domin = 2
            colsymb = c_randh(0, scheme * domin)
            colsymb2 = c_randh(0, scheme)
            If colsymb > FormSettings.scheme Then colsymb = 0 'setting dominance through weighting
            colorchoice = FormSettings.colours(colsymb)
            skycolor = colorchoice
            colorchoice2 = FormSettings.colours(colsymb2)
            grayness = 2
            colorlevel = 7

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
    Private Sub makeskyovercast()
        Dim sky As New Rectangle(0, 0, Image.Width, Image.Height)
        Dim brush As New SolidBrush(Color.FromArgb(220, 240, 240, 250))
        g.FillRectangle(brush, sky)

    End Sub
    Private Sub makeocean()
        'paint sea
        Dim subject As String = "sea"
        colorblend(0, horizon - 2, Image.Width, Image.Height - horizon - 2, subject, 0)
        If Settings.Outlines = True Then g.DrawLine(stainedglasspen, 0, horizon, Image.Width, horizon)
        horizonmist()

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
        Dim x, y, x1, y1, h, yheight, xwidth, sendx, sendy As Integer
        Dim p, n, r1, r2 As Integer
        Dim v, d, height1, width1 As Single
        Dim color2
        v = 1
        p = 800 'pixels per foot
        h = 50  'number to represent general thickness of clouds
        color2 = findcolor(2)

        Dim a As Integer = 40
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
                ' If randsign() = 1 Then

                Dim xbase, ybase As Integer 'place flat cloud underneath
                xbase = x - randh(10, 20)
                ybase = y
                Dim ellipse_path As New Drawing2D.GraphicsPath
                ellipse_path.AddEllipse(xbase, ybase, width1 + 20, -width1 / 20)
                g.SetClip(ellipse_path)
                Dim base_brush As New SolidBrush(Color.FromArgb(250, Mred, Mgreen, Mblue))
                g.FillPath(base_brush, ellipse_path)
                base_brush.Color = Color.FromArgb(190, 250, 250, 250)
                g.FillPath(base_brush, ellipse_path)

                ''cumclouds.AddEllipse(base_rect)
                ' End If
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
                        If Settings.Above = True Or Settings.Evening = True Then
                            Dim Brush0 As New Drawing2D.LinearGradientBrush(rect, _
                            Color.FromArgb(c, Mred, Mgreen, Mblue), _
                            Color.FromArgb(255, 255, 255, 250), _
                            Drawing2D.LinearGradientMode.Vertical)
                            g.FillEllipse(Brush0, rect)
                        End If
                        If Settings.left = True Then
                            Dim Brush0 As New Drawing2D.LinearGradientBrush(rect, _
                              Color.FromArgb(c, Mred, Mgreen, Mblue), _
                             Color.FromArgb(255, 255, 255, 250), _
                             Drawing2D.LinearGradientMode.ForwardDiagonal)
                            g.FillEllipse(Brush0, rect)
                        End If
                        If Settings.Right = True Then
                            Dim Brush0 As New Drawing2D.LinearGradientBrush(rect, _
                             Color.FromArgb(c, Mred, Mgreen, Mblue), _
                            Color.FromArgb(255, 255, 255, 250), _
                            Drawing2D.LinearGradientMode.BackwardDiagonal)
                            g.FillEllipse(Brush0, rect)
                        End If
                    Next i
                    g.ResetClip()
                Next k
            Next j
            '  makeskyfluffcover(sendx, sendy, width1, -height1)
        Next n
    End Sub
    Private Sub rivertrees(ByVal x, ByVal y, ByVal width, ByVal height)

         Dim rect As New Rectangle(x, y, width, height)

        'SET UP PATH TO INCLUDE SEVERAL ARCS AROUND THE EDGE OF THE RECTANGLE
        Dim n, arcbegin, arclength As Integer
        Dim xt, yt, w, startx, starty, rectwid, recthgt As Single
        Dim arcs_path As New Drawing2D.GraphicsPath
        Dim rectl(10), rects(10), pointx(10), pointy(10)
        w = width / 3

        'MAKE nine ARCS WITHIN THE AREA OF THE RECTANGLE

        Dim coord(10) As String
        xt = width / 20
        yt = height / 20
        arcbegin = 90
        arclength = randh(90, 150)

        For n = 1 To 8
            rectwid = xt * randh(5, 15)
            recthgt = yt * randh(5, 15)
            If rectwid < 1 Then Return

            If n = 1 Then
                startx = x + (xt * randh(-2, 2)) : starty = y + yt * randh(-2, 2) : arcbegin = randh(90, 190)
            ElseIf n = 2 Then
                startx = x + (xt * 9) + (xt * randh(-2, 2)) : starty = y + (yt * randh(-4, 0)) : arcbegin = randh(150, 270)
            ElseIf n = 3 Then
                startx = x + xt * 19 + (xt * randh(-2, 2)) - (rectwid * 0.5) : starty = y + yt * randh(-2, 2) : arcbegin = randh(240, 300)
            ElseIf n = 4 Then
                startx = x + xt * 18 + (xt * randh(-2, 2)) - (rectwid * 0.5) : starty = y + yt * 6 + yt * (randh(-2, 2)) : arcbegin = randh(240, 300)
            ElseIf n = 5 Then
                startx = x + xt * 18 + (xt * randh(-2, 2)) - (rectwid * 0.5) : starty = y + yt * 14 + yt * (randh(-2, 0)) - 0.5 * recthgt : arcbegin = randh(240, 300)
            ElseIf n = 6 Then
                startx = x + xt * 9 + xt : starty = y + yt * 10 : arcbegin = 0
                recthgt = height / 3 : rectwid = width / 4
            ElseIf n = 7 Then
                startx = x + (xt * randh(-2, 2)) : starty = y + yt * 14 + yt * (randh(-2, 0)) - recthgt * 0.5 : arcbegin = randh(90, 190)
            ElseIf n = 8 Then
                startx = x + (xt * randh(-2, 2)) : starty = y + yt * 6 + yt * randh(-2, 2) : arcbegin = randh(90, 190)
            ElseIf n = 9 Then
                startx = x + (xt * 2) : starty = y + (yt * 6) : arcbegin = randh(240, 300)
            ElseIf n = 10 Then
                startx = x + (xt * 18) : starty = y + (yt * 5) : arcbegin = yrand(360) 'randh(240, 300)
            End If

            arcs_path.AddArc(startx, starty, rectwid, recthgt, arcbegin, arclength)
        Next n
       
        g.SetClip(arcs_path)
        colorblend(arcs_path.GetBounds.X, arcs_path.GetBounds.Y, arcs_path.GetBounds.Width, _
               arcs_path.GetBounds.Height, "river trees", 0)
        g.ResetClip()

        Dim stainpen As New Pen(Color.Black, 1)
        If Settings.Outlines = True Then g.DrawPath(stainpen, arcs_path)
       
    End Sub
    Private Sub landshape(ByVal typ, ByVal begy, ByVal numdiv, ByVal yvar, ByVal grade, ByVal curv, ByVal distance)
        'ref from rollinghill
        Dim n, begx As Integer
        Dim div As Single
        Dim x(10), y(10)
        Dim landpath As New Drawing2D.GraphicsPath
        begx = randh(-100, 0)
        ' begx = randh(-20, -40)
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
        Dim subject As String = "landshape"
        colorblend(landpath.GetBounds.X, landpath.GetBounds.Y, landpath.GetBounds.Width, _
                landpath.GetBounds.Height, subject, 0)
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
            x(2) = x(1) + randh(100, 300)
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
            Dim striate_path As New Drawing2D.GraphicsPath
            striate_path.AddCurve(curvePoints)
            Dim subject As String = "striate"
            g.SetClip(striate_path)
            colorblend(striate_path.GetBounds.X, striate_path.GetBounds.Y, striate_path.GetBounds.Width, _
                        striate_path.GetBounds.Height, subject, 0)
            g.ResetClip()

        Next

    End Sub
    Private Sub jobs_and_goals()
        'Sort out my rolling hills so that
        '   a. They are more realistic.  How do rolling hills look?
        '   b. I am more in control of distance
        '   c. I can place a house on a level/hill
        '   d. They can totally replace a river scene.  Needs more defined background mountains/hills
        '   e. Separate out subroutines for the hill sequences
        'Arrange for all objects to have adequate color coverage
        '   a. Both blends and singles, with additional shading when wanted
        '   b. .. in both CMB and REG
        'Finish setting up a tonalcomp and intensity factor
        'build a coulee around a river, leading to a prairie scene
        'build in color retention (or tone retention) for repeating colors or changing tones
        'finish 'aerial perspective effects
        '
    End Sub
    Private Sub gas()
        'gas cost, US and Canada exchange
        Dim usd, can, can_purchase_rate_usd, usd_gas_rate, usd_purchase_rate_can, can_gas_rate As Double
        Dim can_rate, usd_rate, output As String
        Dim font As New Font("arial", 16)
        Dim n As Integer = 1 ' type 0 if using US to Can, 1 if Canadian to US
        If n = 0 Then            'pay usd per gal.  How much is that in can/liter?
            '1 gal = 3.7 liters
            usd = 2.91 'cost per gallon of gas...changing
            can_purchase_rate_usd = 1.11 ' 1.18 straight across, or 1.20 if bought
            can = usd * can_purchase_rate_usd ' purchase per $1.00 USD
            can_gas_rate = can / 3.7 'liters
            can_rate = Mid(CStr(can_gas_rate), 1, 5)
            output = (CStr(usd) & " US per gallon =  " & can_rate & " Cdn per liter")
        Else
            can = 1.019 'cost per liter of gas ... changing
            usd_purchase_rate_can = 0.9
            usd = can * usd_purchase_rate_can 'usd cost of one liter
            usd_gas_rate = usd * 3.7  'usd cost of one gallon
            usd_rate = Mid(CStr(usd_gas_rate), 1, 5)
            output = (CStr(can) & " Cdn per liter =  " & usd_rate & " USD per gallon")
        End If
        g.DrawString(output, font, Brushes.Black, New RectangleF(Image.Width / 3, Image.Height / 2, 800, 20))

    End Sub
    Private Sub makefillspace()
        Dim h, addit, xbit, ybit, x(9), y(9), start As Integer
        Dim var As Single
        Dim distant_path As New Drawing2D.GraphicsPath
        Dim accdistant_path As New Drawing2D.GraphicsPath
        xbit = Image.Width / 5
        ybit = (Image.Height - horizon) / 6
        h = horizon + ybit
        addit = 5
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
            Dim subject As String = "fillspace"
            colorblend(distant_path.GetBounds.X, distant_path.GetBounds.Y, distant_path.GetBounds.Width, _
                       distant_path.GetBounds.Height, subject, 0)

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
        Dim k, line_extent, mtns, startx, starty As Integer
        line_extent = randh(1, 5)
        Do
            ' extent limits of individual line lengths
            mtns += 1
            If mtns > 1 Then
                mtptsx(0) = randh(mtptsx(0) + 20, mtptsx(0) + 100)
                mtptsy(0) = randh(mtptsy(0), mtptsy(0) + 20)
            Else
                mtptsx(0) = randh(Image.Width / 2.5, Image.Width - 200)
                mtptsy(0) = horizon + randh(4, 7)
            End If
            startx = mtptsx(0)
            starty = mtptsy(0)
            j = 1 : k = 13

            For i = j To k
                mtptsx(i) = mtptsx(i - 1) + randh(3, line_extent + i)
                mtptsy(i) = mtptsy(i - 1) + randh(3, line_extent) * -1
                If i > 1 And i < 3 Then mtptsy(i) -= yrand(10)
            Next i

            mtptsx(14) = mtptsx(13) + line_extent + i
            mtptsy(14) = mtptsy(13) - randh(5, i)
            mtptsx(15) = mtptsx(14) + randh(40, 70)
            mtptsy(15) = mtptsy(14) - randh(5, i)
            mtptsx(16) = mtptsx(14) + randh(80, 250)
            mtptsy(16) = mtptsy(15) - randh(5, i)

            'change direction of mountainside
            j = 17 : k = 30
            For i = j To k
                mtptsx(i) = mtptsx(i - 1) + randh(3, line_extent + i)
                mtptsy(i) = mtptsy(i - 1) + randh(3, line_extent)
                If mtptsy(i) > mtptsy(0) Then mtptsy(i) = mtptsy(0)
            Next i

            line_extent *= 2.2

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
            Dim subject As String = "bg_mountains"
            g.SetClip(mtnpath)
            colorblend(mtnpath.GetBounds.X, mtnpath.GetBounds.Y, mtnpath.GetBounds.Width, _
                        mtnpath.GetBounds.Height, subject, 0)
            g.ResetClip()
            If Settings.Outlines = True Then g.DrawPath(stainedglasspen, mtnpath)
            If mtptsx(17) > Image.Width Then
                lastmountainy = mtptsy(0)
                Exit Do
            End If



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

        dualcolorblend(x, y, x1, y1, "lake", 1)
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

    Private Sub spears()
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
                y2 = randh(y1, y1 - 100)
                x3 = x1 + (x2 - x0) + yrand(5)
                y3 = y2

                spear.AddBezier(New Point(x1, y1), New Point(x0, y0), _
                New Point(x2, y2), New Point(x3, y3))

                g.FillPath(brushy, spear)
                If Settings.Outlines = True Then g.DrawPath(stainedglasspen, spear)
            Next
        Next
    End Sub

    Private Sub birchstand()

        Dim x(8), y(8), nb, n As Integer
        nb = randh(3, 5)
        For n = 1 To nb
            'trunk = New Drawing2D.GraphicsPath
            x(0) = yrand(Image.Width) 'beginning point - upper left
            y(0) = -2
            x(1) = x(0) + randh(25, 36) 'other side of top
            y(1) = -2
            x(2) = x(0) + randsign() * yrand(30) 'bottom left
            y(2) = Image.Height
            x(3) = x(2) + (x(1) - x(0)) + yrand(5) 'bottom right
            y(3) = Image.Height

            Dim point0 As New Point(x(0), y(0))
            Dim point1 As New Point(x(1), y(1))
            Dim point2 As New Point(x(2), y(2))
            Dim point3 As New Point(x(3), y(3))
            Dim point4 As New Point(x(0), y(0))

            Dim trunk As Point() = {point0, point1, point3, point2, point0}
            'Dim tension As Single = 3.0

            Dim trunkpath As New Drawing2D.GraphicsPath
            trunkpath.AddCurve(trunk)

            g.SetClip(trunkpath)

            'setting path dimensions
            Dim beginx, beginy, xwidth, yheight As Integer
            beginx = trunkpath.GetBounds.X
            beginy = trunkpath.GetBounds.Y
            xwidth = trunkpath.GetBounds.Width
            yheight = trunkpath.GetBounds.Height
            Dim area As New Rectangle(beginx, beginy, xwidth, yheight)
            mem_y = beginy ' get memory of this location
            mem_x = beginx
            If FormSettings.scheme < 10 Then
                GoTo regcolorscheme
            End If

cmbcolorscheme:
            cmbseason = FormSettings.scheme - 10 'CMB color seasons from 0 to 3
            Dim color1, tone As Integer
            If cmbseason = 0 Then color1 = 4
            If cmbseason = 1 Then color1 = 2
            If cmbseason = 2 Then color1 = 2
            If cmbseason = 3 Then color1 = 3
            tone = 0

            Dim CMBbrush As New SolidBrush(Color.Empty)
            CMBbrush.Color = ModCMBcolors.getCMBcolor(cmbseason, tone, color1)
            g.FillRectangle(CMBbrush, area)
            GoTo shadeit

regcolorscheme:

            Dim colorno, dom As Integer
            dom = FormSettings.scheme
            colorno = c_randh(0, dom)
            M_color = FormSettings.colours(colorno)
            Dim regbrush As New SolidBrush(Color.Orange)
            regbrush.Color = modColors.getcolor(M_color, 1, 9)
            g.FillRectangle(regbrush, area)

shadeit:
            If Settings.Right = True Then
                Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
                             Color.FromArgb(150, 0, 0, 0), _
                             Color.FromArgb(0, 230, 230, 230), _
                             Drawing2D.LinearGradientMode.Horizontal)
                g.FillRectangle(Brush3, area)
            Else
                Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
                                             Color.FromArgb(0, 230, 230, 230), _
                                             Color.FromArgb(150, 0, 0, 0), _
                                             Drawing2D.LinearGradientMode.Horizontal)
                g.FillRectangle(Brush3, area)
            End If
            'If Settings.Outlines = True Then g.DrawPath(outlinepen, trunkpath)

trunkscars:
            Dim x0, x1, x2, x3, y0, y1, y2, y3, sc As Integer
            For sc = 1 To 10
                x0 = randh(beginx - 20, xwidth + beginx)
                y0 = yrand(Image.Height)
                x2 = x0 + (x(1) - x(0))
                y2 = y0 + yrand(5)
                x1 = x0 + (x2 - x0) * randh(2, 4) / 10
                y1 = y0 - (x2 - x0) * 0.5
                x3 = x1
                y3 = y1 + yrand(3)

                Dim scarpath = New Drawing2D.GraphicsPath
                scarpath.addbezier(New Point(x0, y0), New Point(x1, y1), _
                   New Point(x2, y2), New Point(x3, y3))
                'scarpath.addbezier(New Point(x2, y2), New Point(x3, y3), _
                '             New Point(x0, y0), New Point(x1, y1))
                Dim scarbrush As New SolidBrush(Color.FromArgb(100, 0, 0, 0))
                g.FillPath(scarbrush, scarpath)
            Next
            g.ResetClip()
            If Settings.Outlines = True Then g.DrawPath(outlinepen, trunkpath)
        Next
        If yrand(8) > 0 Then
            spears()
            Dim bl As Integer = yrand(500)
            bushleaves(bl, Image.Height - 30, 20)
        End If

        If yrand(8) > 0 Then
            bushleaves(mem_x, mem_y + 60, 30)
        End If

    End Sub
    Private Sub colortestlarge()
        Dim red, green, blue, x, y, j, k As Integer
        Dim cname As String
        Dim color1

        i = FormSettings.dominantcolor
        cname = modColors.colorname(i)
        Dim Font As New System.Drawing.Font(" Arial", 8, FontStyle.Bold)
        x = 8
        y = 80
        For j = 1 To 3
            For k = 1 To 9
                color1 = modColors.getcolor(i, j, k)
                red = color1.r
                green = color1.g
                blue = color1.b
                Dim brush0 As New SolidBrush(System.Drawing.Color.FromArgb(255, color1))
                g.FillRectangle(brush0, x, y, 80, 80)
                x += 80
            Next
            y += 81
            x = 8
        Next
    End Sub

    Private Sub colorcode()
        Dim i, j, k, x, y, x0, avey, red, green, blue As Integer
        Dim red1, green1, blue1, cname, ave_y As String
        Dim color1
        x0 = 0
        i = FormSettings.dominantcolor
        cname = modColors.colorname(i)
        Dim aFont As New System.Drawing.Font(" Arial", 7, FontStyle.Bold)
        Dim bFont As New System.Drawing.Font(" Arial", 8, FontStyle.Bold)
        For j = 1 To 3
            For k = 1 To 9
                color1 = modColors.getcolor(i, j, k)
                red = color1.r
                green = color1.g
                blue = color1.b
                avey = (red + green + blue) / 3
                ave_y = CStr(avey)
                red1 = CStr(red)
                green1 = CStr(green)
                blue1 = CStr(blue)
                Dim pen As New Pen(Drawing.Color.Blue, 1)

                x = x0 + 30 * k


                Dim brush0 As New SolidBrush(System.Drawing.Color.FromArgb(255, red, green, blue))
                g.FillRectangle(brush0, x, 300, 22, 22)

                g.DrawString(red1, aFont, Brushes.Black, x, 320)
                g.DrawString(green1, aFont, Brushes.Black, x, 328)
                g.DrawString(blue1, aFont, Brushes.Black, x, 336)
                g.DrawString(ave_y, bFont, Brushes.Chocolate, x, 350)

                y = 280 - red
                Dim brush As New SolidBrush(System.Drawing.Color.FromArgb(255, 255, 0, 0))
                g.FillEllipse(brush, x, y, 13, 13)

                y = 280 - green
                Dim brush1 As New SolidBrush(System.Drawing.Color.FromArgb(255, 0, 255, 0))
                g.FillEllipse(brush1, x, y, 11, 11)

                y = 280 - blue
                Dim brush2 As New SolidBrush(System.Drawing.Color.FromArgb(255, 0, 0, 255))
                g.FillEllipse(brush2, x, y, 9, 9)

            Next
            x0 += 300
        Next
        aFont = New System.Drawing.Font(" Arial", 12, FontStyle.Bold)
        g.DrawString(cname, aFont, Brushes.Black, 410, 375)

        For y = 30 To 280 Step 10
            g.DrawLine(Pens.PeachPuff, 0, y, Image.Width, y)
        Next

    End Sub
    Private Sub gnarledtree(ByVal x0, ByVal y0, ByVal distance)
        distance = distance
        ' y0 = Image.Height
        Dim thickness, trunkheight, n, nn, rsi, base As Integer
        Dim bwidth, thik, nthik, pixtrkhgt, pixtrkseg, boughLLx, boughLLy, Lx, Ly, Rx, Ry As Single
        Dim boughLx(8), boughLy(8), boughRx(8), boughRy(8)

        thickness = 1 'randh(2, 4) 'bottom thickness of tree, in feet
        trunkheight = 2 'randh(8, 12)    'length of trunk in feet - upwards 
        pixtrkhgt = pixelheight(trunkheight, distance)
        pixtrkseg = pixtrkhgt / 2    ' pixel distance for each segment of bough - fixed
        thik = pixelheight(thickness, distance) 'width at beginning portion of trunk
        nthik = thik
        bwidth = thik / 40
        'initial point
        For nn = 1 To 3
            For n = 1 To 4
                If base = 0 Then  'set up the beginning base coordinates for each trunk segment
                    boughLx(0) = x0
                    boughLy(0) = y0
                    boughRx(0) = x0 + thik
                    boughRy(0) = y0
                    base = 1
                Else
                    boughLx(0) = Lx
                    boughLy(0) = Ly + 1
                    boughRx(0) = Rx
                    boughRy(0) = Ry
                End If
                rsi = randsign() * 1
                For i As Integer = 1 To 8
                    thik -= bwidth : If thik < 2 Then thik = 2
                    boughLLx = boughLx(i - 1) + yrand(pixtrkseg) * rsi
                    boughLLy = boughLy(i - 1) - pixtrkseg + yrand(6) / 10 * pixtrkseg
                    boughLx(i) = boughLLx
                    boughLy(i) = boughLLy
                    boughRx(i) = boughLx(i) + thik * 0.4
                    boughRy(i) = boughLy(i)
                Next i

                Dim point0 As New Point(boughLx(0), boughLy(0))
                Dim point1 As New Point(boughLx(1), boughLy(1))
                Dim point2 As New Point(boughLx(2), boughLy(2))
                Dim point3 As New Point(boughLx(3), boughLy(3))
                Dim point4 As New Point(boughLx(4), boughLy(4))
                Dim point5 As New Point(boughLx(5), boughLy(5))
                Dim point6 As New Point(boughRx(5), boughRy(5))
                Dim point7 As New Point(boughRx(4), boughRy(4))
                Dim point8 As New Point(boughRx(3), boughRy(3))
                Dim point9 As New Point(boughRx(2), boughRy(2))
                Dim point10 As New Point(boughRx(1), boughRy(1))
                Dim point11 As New Point(boughRx(0), boughRy(0))

                Dim gnarl As Point() = {point0, point1, point2, point3, point4, point5, point6, point7, point8, _
                           point9, point10, point11}


                Dim gnarly = New Drawing2D.GraphicsPath
                gnarly.AddCurve(gnarl)
                g.SetClip(gnarly)
                Dim subject As String = "gnarly"
                colorblend(gnarly.GetBounds.X, gnarly.GetBounds.Y, gnarly.GetBounds.Width, _
                        gnarly.GetBounds.Height, subject, 0)
                g.ResetClip()
                If Settings.Outlines = True Then g.DrawPath(stainedglasspen, gnarly)
                bushleaves(gnarly.getbounds.x, gnarly.getbounds.y, distance)

                Lx = boughLx(5) : Ly = boughLy(5) : Rx = boughRx(5) : Ry = boughRy(5)
            Next n
            base = 0 : thik = nthik * randh(7, 10) / 10 : x0 += thik * 0.75
        Next nn
    End Sub
    Private Sub bush(ByVal x0, ByVal y0, ByVal distance)
        Dim n As Integer

        'makes one bush
        For n = 1 To 3
            gnarledtree(x0, y0, distance)
            distance += 10

        Next

    End Sub
    Private Sub lonepine()
        distance = randh(60, 100)
        Dim bottomwidth, trunkheight, trkht, botwidth, twidth As Single
        Dim leaves As String = ""
        Dim test As String = ""
        Dim large As String = ""
        Dim midl As String = ""
        Dim small As String = ""
        Dim xe, ye, r, p, leaftype, s1, s2, m1, m2, L1, L2 As Integer
        Dim trunkLx(13), trunkR_x(13), trunkLy(13), trunkR_y(13)
        Dim r_path As New Drawing2D.GraphicsPath
        Dim x, y As Double
        x = randh(20, 300)
        y = Image.Height
        bottomwidth = 1.0   '   in feet
        botwidth = pixelheight(bottomwidth, distance)

treetrunk:
        Dim trunky_path As New Drawing2D.GraphicsPath
        For n As Integer = 1 To 1  'Optional series of trees
            x += randh(25, 34)
            trunkheight = randh(6, 6) 'trunk segment lengths to work with
            trkht = pixelheight(trunkheight, distance)
            trunkLx(0) = x
            trunkLy(0) = y
            trunkR_x(0) = x + botwidth
            trunkR_y(0) = y
            twidth = botwidth / 11
            If yrand(9) > 4 Then
                For i As Integer = 1 To 12
                    botwidth -= twidth
                    trunkLx(i) = (trunkLx(i - 1) + randsign() * yrand(5))
                    trunkLy(i) = (trunkLy(i - 1) - randh(5, trkht))
                    trunkR_x(i) = (trunkLx(i) + botwidth)
                    If trunkR_x(i) - trunkLx(i) < 1 Then trunkR_x(i) += 1
                    trunkR_y(i) = trunkLy(i)
                Next
            Else
                For i As Integer = 1 To 12
                    botwidth -= twidth
                    trunkLx(i) = (trunkLx(i - 1)) '+ randsign() * yrand(5))
                    trunkLy(i) = (trunkLy(i - 1) - randh(5, trkht))
                    trunkR_x(i) = (trunkLx(i) + botwidth)
                    trunkR_y(i) = trunkLy(i)
                Next
            End If

            Dim point0 As New Point(trunkLx(0), trunkLy(0))
            Dim point1 As New Point(trunkLx(1), trunkLy(1))
            Dim point2 As New Point(trunkLx(2), trunkLy(2))
            Dim point3 As New Point(trunkLx(3), trunkLy(3))
            Dim point4 As New Point(trunkLx(4), trunkLy(4))
            Dim point5 As New Point(trunkLx(5), trunkLy(5))
            Dim point6 As New Point(trunkLx(6), trunkLy(6))
            Dim point7 As New Point(trunkLx(7), trunkLy(7))
            Dim point8 As New Point(trunkLx(8), trunkLy(8))
            Dim point9 As New Point(trunkLx(9), trunkLy(9))
            Dim point10 As New Point(trunkLx(10), trunkLy(10))
            Dim point11 As New Point(trunkLx(11), trunkLy(11))
            Dim point12 As New Point(trunkLx(12), trunkLy(12))
            Dim point13 As New Point(trunkR_x(12), trunkR_y(12))
            Dim point14 As New Point(trunkR_x(11), trunkR_y(11))
            Dim point15 As New Point(trunkR_x(10), trunkR_y(10))
            Dim point16 As New Point(trunkR_x(9), trunkR_y(9))
            Dim point17 As New Point(trunkR_x(8), trunkR_y(8))
            Dim point18 As New Point(trunkR_x(7), trunkR_y(7))
            Dim point19 As New Point(trunkR_x(6), trunkR_y(6))
            Dim point20 As New Point(trunkR_x(5), trunkR_y(5))
            Dim point21 As New Point(trunkR_x(4), trunkR_y(4))
            Dim point22 As New Point(trunkR_x(3), trunkR_y(3))
            Dim point23 As New Point(trunkR_x(2), trunkR_y(2))
            Dim point24 As New Point(trunkR_x(1), trunkR_y(1))
            Dim point25 As New Point(trunkR_x(0), trunkR_y(0))

            Dim trunkypoints As Point() = {point0, point1, point2, point3, point4, point5, _
                point6, point7, point8, point9, point10, point11, point12, point13, _
                point14, point15, point16, point17, point18, point19, point20, point21, _
                point22, point23, point24, point25}

            trunky_path.AddCurve(trunkypoints, 0.3)
            g.SetClip(trunky_path)
            colorblend(trunky_path.GetBounds.X, trunky_path.GetBounds.Y, trunky_path.GetBounds.Width, _
            trunky_path.GetBounds.Height, "pinetree", n)
            g.ResetClip()
            If Settings.Outlines = True Then g.DrawPath(outlinepen, trunky_path)

deadbbranches:
            'Place a number of dead branches on tree
            Dim thickness As Single
            For r = 1 To 10
                p = randh(1, 12)   'on which step of the tree to put it, 11 being highest step
                If p > 8 Then p = p - 8
                xe = trunkLx(p)
                ye = trunkLy(p) + randsign() * yrand(trkht / 3)
                thickness = Math.Abs(trunkLx(p) - trunkR_x(p))
                pinebough(xe, ye, thickness, distance)
            Next
            If yrand(10) < 2 Then Return


treefoliage:
            ' Now place foliage on tree
            If yrand(9) > 3 Then  'more weight on down-leafed trees
                s1 = 0 : s2 = 6 : m1 = 7 : m2 = 23 : L1 = 24 : L2 = 40
            Else
                s1 = 0 : s2 = 5 : m1 = 6 : m2 = 19 : L1 = 20 : L2 = 40
            End If
            large = ""
            For r = 1 To 6 ' no. of leaf clusters to place on tree
                p = randh(2, 12)   'on which step of the tree to put it, 11 being highest step
                xe = trunkLx(p) : ye = trunkLy(p)
                If p < 6 Then
                    leaftype = randh(L1, L2)    'large sized leaf cluster
                    test = CStr(leaftype)
                    If InStr(large, test) > 0 Then GoTo next_one
                    large = large + " " + test
                ElseIf p > 5 And p < 10 Then
                    leaftype = randh(m1, m2)     'middle-sized leaf cluster
                    test = CStr(leaftype)
                    If InStr(midl, test) > 0 Then GoTo next_one
                    midl = midl + " " + test
                ElseIf p > 9 Then
                    leaftype = randh(s1, s2)      'smallest size leaf cluster
                    test = CStr(leaftype)
                    If InStr(small, test) > 0 Then GoTo next_one
                    small = small + " " + test
                End If
                leaves = pineleaf_up(leaftype)
                pineleaves(xe, ye, distance, leaves, r)
                If r = 4 Then
                    g.FillClosedCurve(Brushes.Black, trunkypoints)
                End If
next_one:   Next r
        Next n

    End Sub
    Private Sub pinebough(ByVal x0, ByVal y0, ByVal thickness, ByVal distance) 'from lonepine

        Dim boulength, bou_segment, m, branchangle, LR As Integer
        Dim blen, bwidth, thik As Double
        Dim boughLx(7), boughLy(7), boughRx(7), boughRy(7)
        boulength = randh(5, 12)    'full length of bough in feet - upwards then turned 90o
        blen = pixelheight(boulength, distance)
        bou_segment = blen / 6     ' pixel distance for each segment or bough
        boughLx(0) = x0 + thickness * 0.5  'thickness is of the tree trunk, in points 
        boughLy(0) = y0
        thik = (randh(15, 30) / 100) * thickness
        If thik < 1 Then thik = 1
        boughRx(0) = x0 + thik
        boughRy(0) = y0
        m = 5
        bwidth = thik / m
        LR = randsign()
        If LR > 0 Then
            For i As Integer = 1 To m
                thik -= bwidth : If thik < 2 Then thik = 2
                boughLx(i) = boughLx(i - 1) + i / m * bou_segment + randsign() * yrand(bou_segment / 4)
                boughLy(i) = boughLy(i - 1) - (m - i) / m * bou_segment + randsign() * yrand(bou_segment / 4)
                boughRx(i) = boughLx(i) + thik
                boughRy(i) = boughLy(i)
            Next
        Else
            ' boughLx(0) = x0 + thickness * 2
            For i As Integer = 1 To m
                thik -= bwidth : If thik < 2 Then thik = 2
                boughLx(i) = boughLx(i - 1) - i / m * bou_segment + randsign() * yrand(bou_segment / 3)
                boughLy(i) = boughLy(i - 1) - (m - i) / m * bou_segment + randsign() * yrand(bou_segment / 3)
                boughRx(i) = boughLx(i) - thik
                boughRy(i) = boughLy(i)
            Next
        End If

        Dim point0 As New Point(boughLx(0), boughLy(0))
        Dim point1 As New Point(boughLx(1), boughLy(1))
        Dim point2 As New Point(boughLx(2), boughLy(2))
        Dim point3 As New Point(boughLx(3), boughLy(3))
        Dim point4 As New Point(boughLx(4), boughLy(4))
        Dim point5 As New Point(boughLx(5), boughLy(5))
        Dim point6 As New Point(boughLx(5), boughLy(5))
        Dim point7 As New Point(boughRx(4), boughRy(4))
        Dim point8 As New Point(boughRx(3), boughRy(3))
        Dim point9 As New Point(boughRx(2), boughRy(2))
        Dim point10 As New Point(boughRx(1), boughRy(1))
        Dim point11 As New Point(boughRx(0), boughRy(0))

        If LR < 0 Then
            Dim pinebough As Point() = {point0, point1, point2, point3, point4, point5, point6, point7, point8, _
            point9, point10, point11}
            branchangle = randh(-80, -150)
            Rotate(boughLx(0), boughLy(0), branchangle)
            g.FillClosedCurve(Brushes.Black, pinebough, 0.3)
            RotateBack(boughLx(0), boughLy(0), branchangle)
        Else
            Dim pinebough As Point() = {point11, point1, point2, point3, point4, point5, point6, point7, point8, _
                       point9, point10, point0}
            branchangle = randh(80, 150)
            Rotate(boughLx(0), boughLy(0), branchangle)
            g.FillClosedCurve(Brushes.Black, pinebough, 0.3)
            RotateBack(boughLx(0), boughLy(0), branchangle)
        End If
    End Sub

    Private Sub pineleaves(ByVal x0, ByVal y0, ByVal distance, ByVal leaves, ByVal r) 'from lonepine
        Dim x(5), y(5) As Integer
        Dim i, w, cluster As Integer
        Dim j, k As String
        ' Dim pine As New Drawing2D.GraphicsPath()
        cluster = 5                         'average size of leaf cluster in feet - arbitrary
        w = pixelheight(cluster, distance)


        For i = 1 To 5
            j = Mid(leaves, i * 4, 3)

            x(i) = CInt(j)
            If i > 1 Then
                x(i) = (x(i) - x(1)) / Math.Sqrt(distance / 4) + x0 - w / 4
                '(distance / 100 * 8) + x0 - w / 4
            End If
            x(1) = x0
        Next
        For i = 1 To 5
            k = Mid(leaves, (i + 5) * 4, 3)
            y(i) = CInt(k)
            If i > 1 Then
                y(i) = (y(i) - y(1)) / Math.Sqrt(distance / 4) + y0 + w / 2
                '(distance / 100 * 8) + y0 + w / 2
            End If
            y(1) = y0
        Next

        Dim point1 As New Point(x(1), y(1))
        Dim point2 As New Point(x(2), y(2))
        Dim point3 As New Point(x(3), y(3))
        Dim point4 As New Point(x(4), y(4))
        Dim point5 As New Point(x(5), y(5))
        Dim point6 As New Point(x(1), y(1))
        Dim pinebranch As Point() = {point1, point2, point3, point4, point5, point6}

        Dim pinepath As New Drawing2D.GraphicsPath
        pinepath.AddCurve(pinebranch)
        g.SetClip(pinepath)
        colorblend(pinepath.GetBounds.X, pinepath.GetBounds.Y, _
        pinepath.GetBounds.Width, pinepath.GetBounds.Height, "pineleaves", r)
        g.ResetClip()
        If Settings.Outlines = True Then g.DrawPath(outlinepen, pinepath)
        'g.FillClosedCurve(Brushes.Green, pinebranch, -0.5)

    End Sub
    Private Function pineleaf_down(ByVal leaftype)
        Dim leaves As String = "333333"

        If leaftype = 0 Then
            leaves = "   200 070 164 103 200 070 188 091 269 121"   '208 Ds
        ElseIf leaftype = 1 Then
            leaves = "   200 207 162 165 200 283 087 302 338 120"   '254 Ds
        ElseIf leaftype = 2 Then
            leaves = "   200 188 274 220 200 364 146 375 190 157"   '249 Ds
        ElseIf leaftype = 3 Then
            leaves = "   200 111 273 124 200 153 281 087 032 083"   '243 Ds
        ElseIf leaftype = 4 Then
            leaves = "   200 065 122 272 200 109 210 219 041 118"   '246 Ds
        ElseIf leaftype = 5 Then
            leaves = "   200 187 180 180 200 058 152 337 042 022"   '232 Ds
        ElseIf leaftype = 6 Then
            leaves = "   200 070 164 103 200 070 188 091 269 121"   '208 Dm
        ElseIf leaftype = 7 Then
            leaves = "   200 259 013 306 200 172 134 325 052 248"   '199 Dm
        ElseIf leaftype = 8 Then
            leaves = "   200 072 262 254 200 148 216 005 190 206"   '193 Dm
        ElseIf leaftype = 9 Then
            leaves = "   200 308 100 262 200 166 020 280 320 307"   '252 Dm
        ElseIf leaftype = 10 Then
            leaves = "   200 374 298 319 200 140 336 321 306 006"   '251 Dm
        ElseIf leaftype = 11 Then
            leaves = "   200 035 158 370 200 339 369 340 110 154"   '247 Dm
        ElseIf leaftype = 12 Then
            leaves = "   200 201 249 067 200 109 189 321 156 047"   '245 Dm
        ElseIf leaftype = 13 Then
            leaves = "   200 338 203 289 200 087 228 003 222 139"   '198 Dm
        ElseIf leaftype = 14 Then
            leaves = "   200 269 267 128 200 049 232 311 148 062"   '226 Dm
        ElseIf leaftype = 15 Then
            leaves = "   200 296 202 064 200 095 161 010 268 005"   '241 Dm
        ElseIf leaftype = 16 Then
            leaves = "   200 099 143 247 200 139 308 324 339 084"   '220 Dm
        ElseIf leaftype = 17 Then
            leaves = "   200 097 003 248 200 071 138 172 057 251"   '153 Dm
        ElseIf leaftype = 18 Then
            leaves = "   200 295 040 271 200 046 119 233 061 093"   '216 Dm
        ElseIf leaftype = 19 Then
            leaves = "   200 328 263 380 200 385 129 050 165 270"   '182 Dm
        ElseIf leaftype = 20 Then
            leaves = "   200 171 310 366 200 388 046 324 236 135"   '195 Dl
        ElseIf leaftype = 21 Then
            leaves = "   200 053 329 096 200 345 305 145 255 148"   '200 Dl
        ElseIf leaftype = 22 Then
            leaves = "   200 025 099 105 200 010 387 280 313 052"   '242 Dl
        ElseIf leaftype = 23 Then
            leaves = "   200 254 046 132 200 039 221 238 096 075"   '244 Dl
        ElseIf leaftype = 24 Then
            leaves = "   200 262 231 105 200 097 096 302 248 066"   '236 Dl
        ElseIf leaftype = 25 Then
            leaves = "   200 012 356 271 200 139 296 069 067 169"   '183 Dl
        ElseIf leaftype = 26 Then
            leaves = "   200 371 296 184 200 030 114 307 255 072"   '240 Dl
        ElseIf leaftype = 27 Then
            leaves = "   200 339 292 034 200 226 109 022 146 042"   '234 Dl
        ElseIf leaftype = 28 Then
            leaves = "   200 014 231 071 200 390 354 108 379 379"   '172 Dl
        ElseIf leaftype = 29 Then
            leaves = "   200 070 234 378 200 196 277 041 210 237"   '223 Dl
        ElseIf leaftype = 30 Then
            leaves = "   200 025 205 246 200 244 257 060 310 177"   '228 Dl
        ElseIf leaftype = 31 Then
            leaves = "   200 354 153 357 200 270 282 035 152 113"   '213 Dl
        ElseIf leaftype = 32 Then
            leaves = "   200 374 074 023 200 064 222 180 228 002"   '214 Dl
        ElseIf leaftype = 33 Then
            leaves = "   200 398 349 022 200 047 172 288 304 145"   '210 Dl
        ElseIf leaftype = 34 Then
            leaves = "   200 301 037 077 200 327 081 091 332 059"    '192 Dl
        ElseIf leaftype = 35 Then
            leaves = "   200 371 394 281 200 207 364 229 149 136"   '186 Dl
        ElseIf leaftype = 36 Then
            leaves = "   200 379 354 226 200 286 348 225 018 086"   '190 Dl
        ElseIf leaftype = 37 Then
            leaves = "   200 336 216 050 200 177 063 336 337 226"   '191 Dl
        ElseIf leaftype = 38 Then
            leaves = "   200 365 305 378 200 019 073 058 294 316"   '170 Dl
        ElseIf leaftype = 39 Then
            leaves = "   200 053 256 357 200 015 093 031 238 237"   '167 Dl
        ElseIf leaftype = 40 Then
            leaves = "   200 323 016 298 200 038 001 223 303 111"   '161 Dl

        End If
        Return leaves
    End Function
    Private Function pineleaf_up(ByVal leaftype)
        Dim leaves As String = "333333"

        If leaftype = 0 Then
            leaves = "   200 300 150 172 200 092 023 083 020 112"  ' 196, Us
        ElseIf leaftype = 1 Then
            leaves = "   200 328 322 159 200 045 122 057 095 070"   '235, Us
        ElseIf leaftype = 2 Then
            leaves = "   200 295 372 279 200 157 137 217 020 162"   '233, Us
        ElseIf leaftype = 3 Then
            leaves = "   200 180 148 200 200 044 037 037 032 028"   '227, Us
        ElseIf leaftype = 4 Then
            leaves = "   200 322 331 155 200 062 143 122 010 186"   '215, Us
        ElseIf leaftype = 5 Then
            leaves = "   200 144 013 211 200 099 074 014 125 274"   '222, Us
        ElseIf leaftype = 6 Then
            leaves = "   200 062 056 179 200 086 097 103 032 118"   '187, Us
        ElseIf leaftype = 7 Then
            leaves = "   200 393 294 225 200 310 171 342 333 037"   '205, Um
        ElseIf leaftype = 8 Then
            leaves = "   200 062 330 380 200 185 233 284 247 177"   '201, Um
        ElseIf leaftype = 9 Then
            leaves = "   200 338 239 100 200 088 176 268 165 198"   '255, Um
        ElseIf leaftype = 10 Then
            leaves = "   200 025 063 120 200 331 266 298 308 331"   '253, Um
        ElseIf leaftype = 11 Then
            leaves = "   200 323 197 150 200 002 115 180 200 088"   '194, Um
        ElseIf leaftype = 12 Then
            leaves = "   200 326 264 362 200 352 145 280 341 280"   '229, Um
        ElseIf leaftype = 13 Then
            leaves = "   200 100 087 392 200 091 019 041 168 094"   '230, Um
        ElseIf leaftype = 14 Then
            leaves = "   200 306 155 363 200 267 047 007 130 044"   '217, Um
        ElseIf leaftype = 15 Then
            leaves = "   200 054 242 284 200 288 184 226 106 306"   '221, Um
        ElseIf leaftype = 16 Then
            leaves = "   200 313 220 151 200 119 087 255 052 119"   '219, Um
        ElseIf leaftype = 17 Then
            leaves = "   200 180 068 281 200 167 131 196 042 246"   '218, Um
        ElseIf leaftype = 18 Then
            leaves = "   200 187 373 101 200 137 074 040 136 103"   '184, Um
        ElseIf leaftype = 19 Then
            leaves = "   200 305 354 371 200 180 216 249 117 091"   '209, Um
        ElseIf leaftype = 20 Then
            leaves = "   200 164 035 156 200 216 263 084 243 259"   '212, Um
        ElseIf leaftype = 21 Then
            leaves = "   200 350 164 102 200 222 234 108 163 378"   '171, Um
        ElseIf leaftype = 22 Then
            leaves = "   200 084 349 349 200 241 210 091 244 147"   '152, Um
        ElseIf leaftype = 23 Then
            leaves = "   200 303 045 249 200 025 098 077 001 082"   '143, Um
        ElseIf leaftype = 24 Then
            leaves = "   200 388 321 045 200 001 134 278 012 059"   '250, UL
        ElseIf leaftype = 25 Then
            leaves = "   200 089 225 316 200 058 181 308 076 095"   '248, UL
        ElseIf leaftype = 26 Then
            leaves = "   200 111 381 277 200 362 180 121 171 175"   '239, UL
        ElseIf leaftype = 27 Then
            leaves = "   200 067 364 378 200 036 158 237 237 213"   '237, UL
        ElseIf leaftype = 28 Then
            leaves = "   200 067 173 378 200 311 215 031 150 197"   '231, UL
        ElseIf leaftype = 29 Then
            leaves = "   200 041 111 286 200 229 125 234 255 090"   '225, UL
        ElseIf leaftype = 30 Then
            leaves = "   200 363 149 048 200 281 052 137 043 032"   '224, UL
        ElseIf leaftype = 31 Then
            leaves = "   200 137 340 351 200 239 125 112 285 326"   '211, UL
        ElseIf leaftype = 32 Then
            leaves = "   200 063 119 019 200 072 071 221 258 392"   '203, UL
        ElseIf leaftype = 33 Then
            leaves = "   200 283 377 263 200 089 009 134 223 236"   '177, UL
        ElseIf leaftype = 34 Then
            leaves = "   200 392 063 046 200 277 350 117 141 189"   '173, UL
        ElseIf leaftype = 35 Then
            leaves = "   200 065 342 107 200 259 343 250 223 048"   '163, UL
        ElseIf leaftype = 36 Then
            leaves = "   200 157 354 145 200 302 384 024 093 105"   '162, UL
        ElseIf leaftype = 37 Then
            leaves = "   200 131 304 383 200 189 167 096 012 350"   '156, UL
        ElseIf leaftype = 38 Then
            leaves = "   200 106 111 025 200 328 345 238 047 002"   '145, UL
        ElseIf leaftype = 39 Then
            leaves = "   200 053 256 357 200 015 093 031 238 237"   '167 Dl
        ElseIf leaftype = 40 Then
            leaves = "   200 323 016 298 200 038 001 223 303 111"   '161 Dl


        End If
        Return leaves
    End Function

    Private Sub beziertrial()
        Dim x, y, a1, a2, a3, a4, a5, b1, b2, b3, b4, b5, x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, _
        w, cluster, gi, girth As Integer
        Dim bezi As New Drawing2D.GraphicsPath()
        cluster = 14 'average width of leaf cluster in feet - arbitrary
        distance = 150
        w = pixelheight(cluster, distance)
        x = 350
        y = 80
        girth = 1
        gi = pixelheight(girth, distance)
        g.FillRectangle(Brushes.DarkGray, x, y, gi, 330)

        a1 = w / 2
        a2 = yrand(w)
        a3 = yrand(w)
        a4 = yrand(w)
        a5 = a1

        x1 = a1 + x - w / 2
        x2 = a2 + x - w / 2
        x3 = a3 + x - w / 2
        x4 = a4 + x - w / 2
        x5 = a5 + x - w / 2

        b1 = yrand(w)
        b2 = yrand(w)
        b3 = yrand(w)
        b4 = yrand(w)
        b5 = yrand(w)

        y1 = b1 + y - w / 2
        y2 = b2 + y - w / 2
        y3 = b3 + y - w / 2
        y4 = b4 + y - w / 2
        y5 = b5 + y - w / 2


        Dim point0 As New Point(x1, y1)
        Dim point1 As New Point(x2, y2)
        Dim point2 As New Point(x3, y3)
        Dim point3 As New Point(x4, y4)
        Dim point4 As New Point(x5, y5)
        Dim leaf As Point() = {point0, point1, point2, point3, point4}



        bezi.AddBezier(New Point(x1, y1), New Point(x2, y2), _
          New Point(x3, y3), New Point(x4, y4))
        ' g.SetClip(bezi)
        'colorblend(bezi.GetBounds.X, bezi.GetBounds.Y, bezi.GetBounds.Width, bezi.GetBounds.Height, "leaves", 3)
        'g.ResetClip()

        g.DrawString("                    w= " & w & "          a1= " & a1 & "    a2= " & a2 & "    a3= " & a3 & "    a4= " & a4 & "   a5= " & a5, _
        New Font("arial", 10), Brushes.Black, New RectangleF(16, 2, 800, 16))
        g.DrawString("                    x= " & x & "          b1= " & b1 & "    b2= " & b2 & "    b3= " & b3 & "    b4= " & b4 & "   b5= " & b5, _
        New Font("arial", 10), Brushes.Black, New RectangleF(16, 22, 800, 16))

        g.DrawLines(Pens.Orange, leaf)
        g.FillRectangle(Brushes.Black, x, y, 5, 5)
        g.FillRectangle(Brushes.Red, x1, y1, 4, 4)
        g.FillRectangle(Brushes.Red, x2, y2, 4, 4)
        g.FillRectangle(Brushes.Red, x3, y3, 4, 4)
        g.FillRectangle(Brushes.Red, x4, y4, 4, 4)
        g.FillRectangle(Brushes.Red, x5, y5, 4, 4)

    End Sub
    Private Sub Lombardy()
        Dim n, x, wid, hgt, x0, lx, rx, bottomy, beginy, midptx As Integer
        Dim ht As Single
        distance = randh(240, 500)
        x = 100 'to be declared on ref
        x0 = x
        bottomy = Image.Height - 160

        For n = 0 To 5
            ' x = x0 + randh(20, 150)
            hgt = randh(90, 100) 'in feet
            ht = pixelheight(hgt, distance)
            wid = ht / 12
            midptx = wid / 2 + x
            lx = x - (wid / 4) - 4
            rx = x + wid + 5
            beginy = bottomy - ht / 10
            For i = 1 To 3
                Dim point0 As New Point(x, beginy)
                Dim point1 As New Point(lx, beginy - ht / 1.4)
                Dim point2 As New Point(midptx + randsign() * yrand(10), beginy - ht + randsign() * yrand(5))
                Dim point3 As New Point(rx, beginy - ht / 1.5)
                Dim point4 As New Point(x + wid, beginy)
                '  Dim point5 As New Point(midptx, beginy + 10)

                '  Dim pfull As Point() = {point0, point2, point4}

                Dim lombfull As Point() = {point0, point1, point2, point3, point4}
                Dim lombardy = New Drawing2D.GraphicsPath
                lombardy.addclosedcurve(lombfull)
                g.SetClip(lombardy)

                colorblend(lombardy.getbounds.x, lombardy.getbounds.y, lombardy.getbounds.width, _
                 lombardy.getbounds.height, "lombardy", 0)

                'g.FillClosedCurve(Brushes.Green, pfull, -0.5)
                g.ResetClip()
                If Settings.Outlines = True Then g.DrawPath(outlinepen, lombardy)
            Next
            'Lombardy trunk
            'using bottomy as the very bottom of the tree - the trunk roots
            Dim startx, starty, y1, rtbotx, midbotx As Integer

            startx = x + wid / 3
            starty = bottomy + hgt / 3

            rtbotx = startx + wid * 0.3
            midbotx = startx + (rtbotx - startx) / 2
            For i = 1 To 3
                y1 = bottomy - hgt / 2 - randh(hgt / 2, hgt)   'height of trunk
                Dim point6 As New Point(startx, bottomy) 'bottom left
                Dim point7 As New Point(midptx + randsign() * yrand(wid / 2), y1) 'point
                Dim point8 As New Point(rtbotx, bottomy) 'bottom right
                Dim point9 As New Point(midbotx, bottomy * 1.06) 'midbottom

                Dim trunk As Point() = {point6, point7, point8, point9}
                Dim lombardy = New Drawing2D.GraphicsPath
                lombardy.addclosedcurve(trunk)
                g.SetClip(lombardy)

                colorblend(lombardy.getbounds.x, lombardy.getbounds.y, lombardy.getbounds.width, _
                 lombardy.getbounds.height, "lombardy", 0)
                ' g.FillClosedCurve(Brushes.Gray, trunk, -0.5)
                g.ResetClip()
                If Settings.Outlines = True Then g.DrawPath(outlinepen, lombardy)
            Next i

            x += wid * 3

        Next
    End Sub
    Private Sub deciduous()

        Dim wd, w, h, ht, n, i, j, v, topx, topy, wdd, htt, basex, basey, basefoliagey As Integer
        Dim foliage_x(12), foliage_y(12)
        distance = 120 'randh(50, 150)
        basex = yrand(600)
        basey = 450 '300 'true base
        For v = 1 To 2
            basex = yrand(600)
            For j = 1 To 1
                w = 25 'normal width of tree in feet
                h = 40 'normal height of tree in feet
                wd = pixelheight(w, distance)
                ht = pixelheight(h, distance)
                topx = basex

                topy = basey - ht '  top of tree
                wdd = wd / 9 'increments to work with
                htt = ht / 3

                basefoliagey = basey - ht * 0.45

                'Dim rect As New Rectangle(topx, topy, wd, ht)
                'g.FillRectangle(Brushes.Coral, rect)

                Dim decid As New Drawing2D.GraphicsPath
                foliage_x(0) = basex
                foliage_y(0) = basefoliagey
                For n = 1 To 9
                    foliage_x(n) = basex + n * wdd + randsign() * yrand(wdd)
                    foliage_y(n) = basefoliagey + randsign() * yrand(htt / 1.5)
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
                Dim point10 As New Point(foliage_x(9) - wd / 5, topy)
                Dim point11 As New Point(foliage_x(1) + wd / 4, topy)

                Dim leaves As Point() = {point0, point1, point2, point3, point4, point5, point6, _
               point7, point8, point9, point10, point11}
                decid.AddClosedCurve(leaves)

                'g.SetClip(decid)
                'colorblend(decid.GetBounds.X, decid.GetBounds.Y, decid.GetBounds.Width, _
                '       decid.GetBounds.Height, "deciduous", 2)
                'g.ResetClip()
                'If Settings.Outlines = "true" Then g.DrawPath(outlinepen, decid)

trunk:          '-------------------place trunk---------------
                If j < 3 Then
                    Dim bottomwidth, botwidth, trksg, twidth, trkht As Single
                    Dim trunkLx(6), trunkLy(6), trunkRx(6), trunkRy(6)
                    bottomwidth = 1.3   '   in feet

                    Dim trunk_path As New Drawing2D.GraphicsPath
                    trkht = ht
                    trksg = trkht / 6 'trunk segment lengths to work with - in feet
                    '  trksg = pixelheight(trunkseg, distance)
                    For n = 1 To 2
                        botwidth = pixelheight(bottomwidth, distance) / n
                        twidth = botwidth / 8
                        trunkLx(0) = basex + wd / 2
                        trunkLy(0) = basey
                        trunkRx(0) = basex + wd / 2 + botwidth
                        trunkRy(0) = basey


                        For i = 1 To 6
                            botwidth -= twidth
                            trunkLx(i) = (trunkLx(i - 1) + randsign() * yrand(botwidth))
                            trunkLy(i) = (trunkLy(i - 1) - randh(trksg / 2, trksg))
                            trunkRx(i) = (trunkLx(i) + botwidth)
                            '  If trunkRx(i) - trunkLx(i) < 1 Then trunkRx(i) += 1
                            trunkRy(i) = trunkLy(i)
                        Next

                        Dim point12 As New Point(trunkLx(0), trunkLy(0))
                        Dim point13 As New Point(trunkLx(1), trunkLy(1))
                        Dim point14 As New Point(trunkLx(2), trunkLy(2))
                        Dim point15 As New Point(trunkLx(3), trunkLy(3))
                        Dim point16 As New Point(trunkLx(4), trunkLy(4))
                        Dim point17 As New Point(trunkLx(5), trunkLy(5))
                        Dim point18 As New Point(trunkLx(6), trunkLy(6))
                        Dim point19 As New Point(trunkRx(6), trunkRy(6))
                        Dim point20 As New Point(trunkRx(5), trunkRy(5))
                        Dim point21 As New Point(trunkRx(4), trunkRy(4))
                        Dim point22 As New Point(trunkRx(3), trunkRy(3))
                        Dim point23 As New Point(trunkRx(2), trunkRy(2))
                        Dim point24 As New Point(trunkRx(1), trunkRy(1))
                        Dim point25 As New Point(trunkRx(0), trunkRy(0))

                        Dim trunkpoints As Point() = {point12, point13, _
                            point14, point15, point16, point17, point18, point19, point20, point21, _
                            point22, point23, point24, point25}

                        trunk_path.AddCurve(trunkpoints, 0.3)
                        g.SetClip(trunk_path)
                        colorblend(trunk_path.GetBounds.X, trunk_path.GetBounds.Y, _
                        trunk_path.GetBounds.Width, trunk_path.GetBounds.Height, "trunk", 0)
                        g.ResetClip()
                        If Settings.Outlines = "true" Then g.DrawPath(outlinepen, trunk_path)

placeleaveslast:        g.SetClip(decid)
                        colorblend(decid.GetBounds.X, decid.GetBounds.Y, decid.GetBounds.Width, _
                               decid.GetBounds.Height, "deciduous", 0)
                        g.ResetClip()
                        If Settings.Outlines = "true" Then g.DrawPath(outlinepen, decid)
placeflowersintree:
                        If yrand(10) > 8 Then
                            g.SetClip(decid)
                            Dim f, color1, color2, beginx, beginy, xwidth, yheight, newx, newy, newwidth, newheight As Integer
                            Dim flower
                            beginx = decid.GetBounds.X
                            beginy = decid.GetBounds.Y
                            xwidth = decid.GetBounds.Width
                            yheight = decid.GetBounds.Height
                            Dim area As New Rectangle(beginx, beginy, xwidth, yheight)

                            For f = 1 To 400
                                newx = beginx + yrand(xwidth)
                                newy = beginy + yrand(yheight)
                                newwidth = randh(3, 4)
                                newheight = randh(3, 4)

                                flower = New Rectangle(newx, newy, newwidth, newheight)
                                If f = 1 Then color1 = yrand(14) Else color1 = color2
                                Dim paintbrush As New SolidBrush(Color.Black)
                                paintbrush.Color = ModCMBcolors.getCMBcolor(cmbseason, 0, color1)
                                g.FillEllipse(paintbrush, flower)
                                If f = 1 Then color2 = color1




                            Next

                            g.ResetClip()
                            If Settings.Outlines = "true" Then g.DrawPath(outlinepen, decid)

                        End If



                    Next
                End If
            Next

        Next

    End Sub

    Private Sub logo()

        'make arcs
        Dim logo_path As New Drawing2D.GraphicsPath
        Dim brush4 As New SolidBrush(Color.ForestGreen)

        Dim x, y, width, height, arcstart, arc_end As Integer

        For i = 1 To 4
            x = Image.Width / 4 + yrand(150)
            y = Image.Height / 7 + yrand(100)
            width = randh(30, 400)
            height = randh(30, 300)
            arcstart = yrand(yrand(360))
            arc_end = yrand(yrand(180))

            logo_path.AddArc(x, y, width, height, arcstart, arc_end)

        Next i
        Dim subject As String = "logo"
        doflathousecolor(logo_path.GetBounds.X, logo_path.GetBounds.Y, logo_path.GetBounds.Width, logo_path.GetBounds.Height, 2, subject)

    End Sub

    Private Sub newtreetrunks(ByVal basex, ByVal basey, ByVal botwidth, ByVal p_hgt)
        'Dim hgt, wid, topx As Integer

        ' topx = basex
        'distance = 100
        'basex = 300 'randh(200, 500)
        'basey = 400
        'wid = 25 'normal width of whole tree in feet
        'hgt = 70 'normal height of total tree leaf section in feet
        'p_wid = pixelheight(wid, distance)
        'p_hgt = pixelheight(hgt, distance)

        Dim botwidth_mem, trksg, twidth, trkht As Single
        Dim trunkLx(7), trunkLy(7), trunkRx(7), trunkRy(7)
        'bottomwidth = 2   '   in feet
        'botwidth = pixelheight(bottomwidth, distance)
        trkht = p_hgt
        trksg = trkht / 6 'trunk segment lengths to work with - 
        '  trksg = pixelheight(trunkseg, distance)
        botwidth_mem = botwidth 'set up memory of width
        Dim trunk_path As New Drawing2D.GraphicsPath
        For n As Integer = 1 To 3
            botwidth = botwidth_mem  'set it back to original
            twidth = botwidth / 7
            trunkLx(0) = basex
            trunkLy(0) = basey
            trunkRx(0) = basex + botwidth
            trunkRy(0) = basey

            If n = 1 Then  'for left leaning tree
                For i = 1 To 7
                    botwidth -= twidth ' each segment upward loses a seventh of trump diameter
                    trunkLx(i) = trunkLx(i - 1) - twidth * randh(-1, 7)
                    trunkLy(i) = (trunkLy(i - 1) - randh(trksg / 2, trksg))
                    trunkRx(i) = (trunkLx(i) + botwidth)
                    trunkRy(i) = trunkLy(i)
                Next
            End If
            If n = 2 Then  'for right leaning tree
                For i = 1 To 6
                    botwidth -= twidth
                    trunkLx(i) = trunkLx(i - 1) + twidth * randh(-1, 5)
                    trunkLy(i) = (trunkLy(i - 1) - randh(trksg / 2, trksg))
                    trunkRx(i) = (trunkLx(i) + botwidth)
                    trunkRy(i) = trunkLy(i)
                Next
            End If
            If n = 3 Then 'for center tree
                For i = 1 To 6
                    botwidth -= twidth : If i = 7 Then botwidth = 2
                    trunkLx(i) = (trunkLx(i - 1) + twidth * (randh(-1, 1)))
                    trunkLy(i) = (trunkLy(i - 1) - randh(trksg / 2, trksg))
                    trunkRx(i) = (trunkLx(i) + botwidth)
                    trunkRy(i) = trunkLy(i)
                Next
            End If

            Dim point12 As New Point(trunkLx(0), trunkLy(0))
            Dim point13 As New Point(trunkLx(1), trunkLy(1))
            Dim point14 As New Point(trunkLx(2), trunkLy(2))
            Dim point15 As New Point(trunkLx(3), trunkLy(3))
            Dim point16 As New Point(trunkLx(4), trunkLy(4))
            Dim point17 As New Point(trunkLx(5), trunkLy(5))
            Dim point18 As New Point(trunkLx(6), trunkLy(6))
            Dim point19 As New Point(trunkRx(6), trunkRy(6))
            Dim point20 As New Point(trunkRx(5), trunkRy(5))
            Dim point21 As New Point(trunkRx(4), trunkRy(4))
            Dim point22 As New Point(trunkRx(3), trunkRy(3))
            Dim point23 As New Point(trunkRx(2), trunkRy(2))
            Dim point24 As New Point(trunkRx(1), trunkRy(1))
            Dim point25 As New Point(trunkRx(0), trunkRy(0))

            Dim trunkpoints As Point() = {point12, point13, _
                point14, point15, point16, point17, point18, point19, point20, point21, _
                point22, point23, point24, point25}
            trunk_path = New Drawing2D.GraphicsPath
            trunk_path.AddCurve(trunkpoints, 0.3)
            ' g.FillPath(Brushes.Black, trunk_path)
            g.SetClip(trunk_path)

            colorblend(trunk_path.GetBounds.X, trunk_path.GetBounds.Y, _
            trunk_path.GetBounds.Width, trunk_path.GetBounds.Height, "trunk", 0)

            If Settings.Outlines = "true" Then g.DrawPath(outlinepen, trunk_path)
            g.ResetClip()
        Next

    End Sub
    Private Sub makelake()
        '' -------------------------------------------------LINEAR COLOR GRADIENTS
        If FormSettings.scheme > 9 Then
            Dim rect5 As New Rectangle(50, 30, 200, 200) ' a cerulean blue
            Dim lBrush As New Drawing2D.LinearGradientBrush(rect5, Color.FromArgb(230, _
           yrand(255), yrand(255), yrand(255)), _
            Color.FromArgb(100, yrand(255), yrand(255), yrand(255)), _
            Drawing2D.LinearGradientMode.BackwardDiagonal)
            g.FillRectangle(lBrush, rect5)
        End If
    End Sub
    Private Sub makeriveraswinding2()
        Dim a, b, sparkley1, sparkley2, sparklex1, sparklex2, rx, strleftx(20), strrighty(20), strrightx(20), strlefty(20), _
                     streamx(4), startx(4), starty(4), riverwidth, streamy(4), diststep As Integer
        Dim rbeachx(20), rbeachy(20), lbeachx(20), lbeachy(20)
        Dim sidesway, y_drop, y_step As Single
        Dim river_path As New Drawing2D.GraphicsPath
        Dim riverbed_path As New Drawing2D.GraphicsPath

        riverwidth = randh(15, 30) 'randh(80, 200) 'in feet
        Dim begdistance As Integer = 300 'randh(5280, 11000) 'in feet, between one and two miles away
        distance = begdistance
        diststep = (begdistance - 20) / 20 '90 is distance to river at bottom of screen

        Dim beachkey As Integer = riverwidth / 8 ' in feet
        Dim p_riverwidth As Integer = pixelheight(riverwidth, begdistance)
        Dim p_beach As Integer = pixelheight(beachkey, begdistance)
        oceanpoint = 0
        'total number of divisions below horizon = 6
        ' divHeight = (Image.Height - horizon) / 6
        'determine approximate no. divisions left below beginning point of stream
        startx(3) = randh(Image.Width * 0.2, Image.Width * 0.8)
        starty(3) = horizon + randh(2, 20)
        Dim ystart As Single = starty(3)
        y_step = (Image.Height + 160 - ystart) / 20

        'restore beach color if ocean not wanted
        If oceanpoint < 1 Then
            Dim subject As String = "beach"
            colorblend(0, ystart, Image.Width, Image.Height, subject, 0)
        End If

        startx(4) = startx(3) - p_riverwidth
        starty(4) = starty(3)

        For j As Integer = 20 To 1 Step -1


            distance -= diststep
            p_riverwidth = pixelheight(riverwidth, distance)
            y_drop = pixelheight((Image.Height + 100 - starty(4)) / j, distance)
            ' y_drop = pixelheight(y_step, distance)
            rx = yrand(p_riverwidth * 0.75)
            If yrand(10) > 6 Then
                sidesway = -rx
            Else : sidesway = rx
            End If


            streamx(1) = startx(4)
            streamy(1) = starty(4)
            streamx(2) = startx(3)
            streamy(2) = starty(3)
            streamx(3) = streamx(2) + sidesway
            streamy(3) = streamy(2) + y_drop
            streamx(4) = streamx(3) - p_riverwidth
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
            p_beach = pixelheight(beachkey, distance)
            rbeachx(j) = strrightx(j) + p_beach
            rbeachy(j) = streamy(3)
            lbeachx(j) = strleftx(j) - p_beach
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

                colorblend(0, ystart, Image.Width, Image.Height - ystart, "river", 0)
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
    Private Sub makeriveraswinding()
        Dim a, b, sparkley1, sparkley2, sparklex1, sparklex2, rx, strleftx(20), strrighty(20), strrightx(20), strlefty(20), _
              streamx(4), startx(4), starty(4), riverwidth, streamy(4), divHeight As Integer
        Dim rbeachx(20), rbeachy(20), lbeachx(20), lbeachy(20)
        Dim level, levelkey, h, lv, sidesway, addonsw As Single
        Dim river_path As New Drawing2D.GraphicsPath
        Dim riverbed_path As New Drawing2D.GraphicsPath
        Dim beachkey As Integer = randh(5, 12)
        riverwidth = randh(60, 250)

        oceanpoint = 0
        'total number of divisions below horizon = 6
        divHeight = (Image.Height - horizon) / 6
        'determine approximate no. divisions left below beginning point of stream
        startx(3) = randh(Image.Width * 0.2, Image.Width * 0.8)
        starty(3) = horizon + randh(2, 20)

        Dim ystart As Single = starty(3)

        'restore beach color if ocean not wanted
        If oceanpoint < 1 Then
            Dim subject As String = "beach"
            colorblend(0, ystart, Image.Width, Image.Height, subject, 0)
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
            If yrand(10) > 7 Then
                sidesway = -rx
            Else : sidesway = rx
            End If


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

                colorblend(0, ystart, Image.Width, Image.Height - ystart, "river", 0)
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
        Dim q As Integer = 1 'direction of rise 
        For side1 As Integer = 1 To 2    'going down left side of river first, then down right 
            levelstartx = 0
            n = 1
            v = streampts(n).x : z = streampts(n + 1).x : vz = streampts(n - 1).x  ' take an average, test direction of river at source
            ave = (v + z) / 2
            If side1 = 1 Then        'goal, to have mountain go from cove to cove
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
            If side1 = 2 Then        'coming down the right side, do reverse
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

                If side1 = 1 Then
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

                If side1 = 2 Then
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
                Dim low As Integer = 1
                direction = ""
                riser = rise
                Dim hill_path As New Drawing2D.GraphicsPath
                ' starting points for the intersecting mountain shape
                initialx(0) = streampts(levelstartx).x
                initialy(0) = streampts(levelstartx).y
                If Settings.Flat = True Then
                    initialx(1) = streampts(levelstartx).x - (q * (xincr - randsign() * randh(2, 14)))
                    initialy(1) = initialy(0) - 20 * levelstartx / 10
                    low = 2
                    rise = 0
                End If

                Dim xrev As Integer = -10       'to extend beyond the edges of the form
                If side1 = 2 Then xrev = Image.Width + 10
                For w As Integer = low To 7
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

                If side1 = 1 Then hillcount_L += 1 Else hillcount_R += 1
                'If Settings.Prairie = True Then
                ' hill_path.AddCurve(curvepoints3, fillmode.winding)
                'Else
                hill_path.AddCurve(curvepoints3, 0.3F)

hilltrees:
                Dim hx, hy As Integer       'place trees on crests of mountains/hills before adding hills
                Dim hillpath
                hillpath = hill_path.PathPoints
                Dim width, height, starty As Integer
                Dim level, h, lv As Single
                Dim e As Integer = 0
                If side1 = 1 Then
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
                        If height < 3 Then GoTo loop1
                        width = height * randh(2, 4) / 10
                        If e = 0 Then e = 5 : GoTo loop1
                        If randsign() > 0 Then
                            rivertrees(hx, hy, width, height)
                        Else
                            Dim ellipse_path As New Drawing2D.GraphicsPath
                            ellipse_path.AddEllipse(hx, hy, width, -height)
                            g.SetClip(ellipse_path)
                            Dim subject1 As String = "hilltrees"
                            colorblend(hx, hy, width, -height, subject1, 0)

                            'Dim tree_brush As New SolidBrush(Color.FromArgb(255, beep.R, beep.G, beep.B))
                            'g.FillEllipse(tree_brush, hilltrees_rect)
                        End If

loop1:                  e += 1
                    Loop
                End If

                acchill_path.AddCurve(curvepoints3)     'accumulates the hillshapes
                g.SetClip(hill_path)
                Dim subject As String = "river_mountains"
                colorblend(hill_path.GetBounds.X, hill_path.GetBounds.Y, hill_path.GetBounds.Width, _
                hill_path.GetBounds.Height, subject, 0)

                Dim a As Integer
                If Settings.Winter = True Then a = 240 Else a = 100
                Dim haze_brush As New SolidBrush(Color.FromArgb(a, 240, 240, 255))
                If Settings.Mountainous = False Then
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

        Next side1
        'makerivertrees()
    End Sub

    Private Sub Rotate(ByVal axisx As Integer, ByVal axisy As Integer, ByVal a As Single)
        g.TranslateTransform(axisx, axisy)
        g.RotateTransform(a)
        g.TranslateTransform(-axisx, -axisy)
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
        'ht = randh(160 * 3, 270 * 3) 

        Dim treehtfactor As Integer ' height of trees, determining how close they seem to be
        treehtfactor = randh(3, 9)
        'If Settings.Stream = True Then treehtfactor = randh(60, 520)
        ht = randh(70, 200) * treehtfactor
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
            '    h = level ^ 2 'option
size:
            yheight1 = (ht + yrand(79)) * h
            Dim Con As Single
            Con = (randh(16, 24) / 10)
            xwidth1 = yheight1 / Con
            If xwidth1 < 3 Then GoTo loops
            If yheight1 > 540 Then Exit Do

            ybegin1 = ybase - yheight1 - 5

            For j = beg To en Step st
                If riverpts(j).y >= ybase Then
                    xbegin1 = riverpts(j - st).x + (ybase - riverpts(j - st).y) _
                                         * (riverpts(j).x - riverpts(j - st).x) _
                                         / (riverpts(j).y - riverpts(j - st).y)
                    xbegin1 = xbegin1 + q * xwidth1 + q * randh(0, 10) - xwidth1 / 2
                    Exit For
                End If
            Next
            If yrand(10) > 3 Then  'place trees
                rivertrees(xbegin1, ybegin1, xwidth1, yheight1)
            End If
            If yrand(10) > 1 Then 'place trunks, sometimes alone
                treetrunks(xbegin1, xwidth1, ybegin1, yheight1, h)
            End If

loops:      If ybase = ybase1 Then
                ybase1 = ybase1 + (randh(0, 4) ^ 4) * h
            Else
                ybase2 = ybase2 + (randh(0, 4) ^ 4) * h
            End If
        Loop Until False

    End Sub
    Private Sub treetrunks(ByVal xbegin1, ByVal xwidth1, ByVal ybegin1, ByVal yheight1, ByVal h)
        'for rivertrees
        Dim brush As New SolidBrush(Color.DarkGray)
        Dim xtrunkbottom1, xtrunkwidth, ytrunkbottom1, ytrunkbottom2, xtrunktop, ytrunktop, botwidth As Single
        For m As Integer = 1 To 2 ' do two trunks per tree
            xtrunkbottom1 = xbegin1 + xwidth1 / 2
            ytrunkbottom1 = ybegin1 - 3 + yheight1 + 60 * h
            xtrunkwidth = xtrunkbottom1 + 20 * h
            ytrunkbottom2 = ytrunkbottom1
            botwidth = (xtrunkwidth - xtrunkbottom1) * 1.5
            If botwidth < 1.0 Then Return
            xtrunktop = xtrunkbottom1 + randsign() * (yrand(6) * h)
            ytrunktop = randh(ytrunkbottom1 - yheight1 * 0.8, ytrunkbottom1 - yheight1 * 1.2)
            If Settings.River = True Then
                newtreetrunks(xtrunkbottom1, ytrunkbottom1, botwidth, ytrunkbottom1 - ytrunktop * 0.9)
                Return
            End If
            Dim point21 As New Point(xtrunkbottom1, ytrunkbottom1)
            Dim point22 As New Point(xtrunkwidth, ytrunkbottom2)
            Dim point23 As New Point(xtrunktop, ytrunktop)
            Dim curvepoints1 As Point() = {point21, point22, point23, point21}

            Dim trunk_path = New Drawing2D.GraphicsPath
            '  Rotate(xtrunkbottom1, ytrunkbottom1, treeangle) 'same treeangle used for main tree body
            trunk_path.AddCurve(curvepoints1, 0.1F)
            Dim w As Single = trunk_path.getbounds.width
            If trunk_path.GetBounds.Width < 2 Or trunk_path.getbounds.height < 1 Then Return
            g.SetClip(trunk_path)

            'If yrand(100) > 3 Then g.FillPath(Brushes.Black, trunk_path) Else g.FillPath(Brushes.DarkGray, trunk_path)
            colorblend(trunk_path.GetBounds.X, trunk_path.GetBounds.Y, _
            trunk_path.GetBounds.Width, trunk_path.GetBounds.Height, "trunk", 1)
            g.ResetClip()
            If Settings.Outlines = True Then g.DrawPath(Pens.Black, trunk_path)

        Next
        g.ResetClip()

    End Sub
    Private Sub makerivertreebranch2()
        'unused, outdated stuff
        'do the ellipse trees

        'Dim skyhole, ra, rb As Integer
        'Dim rect As New Rectangle(xbegin1, ybegin1, xwidth1, yheight1)
        'If Settings.Outlines = True And xwidth1 < 6 Then GoTo loops
        'skyhole = randh(1, 6)
        'Select Case skyhole
        '    Case Is = 1     'left gap
        '        ra = 230 : rb = 340
        '    Case Is = 2     'right gap
        '        ra = 335 : rb = 340
        '    Case Is = 3     'bottom out
        '        ra = 135 : rb = 270
        '    Case Is = 4     'top split
        '        ra = 280 : rb = 350
        '    Case Is >= 5    'no change
        '        ra = 280 : rb = 360
        'End Select
        ''treeangle = randh(-14, 14)

        '  This will rotate the following objects by "treeangle" degrees.
        '  It will rotate the objects around the x and y values 
        '  xbegin1 + xwidth1/2, ybegin1 + yheight1 rotates the object around the bottom-middle of the object.
        '  xbegin1 + xwidth1/2, ybegin1 + yheight1/2 rotates the object around the middle-middle of the object.

        'Rotate(xbegin1 + xwidth1 / 2, ybegin1 + yheight1, treeangle)
        'Dim brush1 As New SolidBrush(Color.Empty)
        ''brush1.Color = findcolor(2)    'tone will determine the tonal nature of the color
        'g.FillPie(brush1, rect, ra, rb)
        'g.SetClip(rect)
        ''Dim subject As String = "rivertrees" : Dim type As String = "ellipse"
        ''colorblend(xbegin1, ybegin1, xwidth1, yheight1, subject, type)
        'g.ResetClip()

        'If Settings.Outlines = True Then
        '    g.DrawPie(stainedglasspen, rect, ra, rb)
        '    g.DrawEllipse(stainedglasspen, rect)
        'End If
        '' RotateBack(xbegin1 + xwidth1 / 2, ybegin1 + yheight1, treeangle)

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
        'colorblend(xtrunkbottom1, ytrunktop, xtrunkwidth, -ybranchtop, 3, 2)
    End Sub
    Public Function findcolor(ByVal tone) 'tonal_distance is 3 = light, 2 = mid, 1 = dark
        tone = yrand(2)
CMB:
        If FormSettings.scheme > 9 Then
            Dim cmbseason As Integer = FormSettings.scheme - 10  'changing from 11, 12, 13, etc
            tone = tone - 1 ' to give a base of 0 (for CMB)
            color14 = c_randh(1, 14)       '14 colors in each CMB level
            Dim brush As New SolidBrush(Color.Empty)
            brush.Color = ModCMBcolors.getCMBcolor(cmbseason, tone, color14)
            findcolor = brush.Color
            Mred = brush.Color.R
            Mgreen = brush.Color.G
            Mblue = brush.Color.B

            Exit Function
        End If

NonCMB:
        'intensity and tone are separate variables in NonCMB colors
        tone = yrand(2)
        If tone = 0 Then M_light = c_randh(7, 9)
        If tone = 1 Then M_light = c_randh(4, 6)
        If tone = 2 Then M_light = c_randh(1, 3)

        Dim colsymb, domin As Integer
        If FormSettings.scheme > 0 And FormSettings.scheme < 3 Then domin = 1 Else domin = 2
        colsymb = c_randh(0, FormSettings.scheme * domin)
        If colsymb > FormSettings.scheme Then colsymb = 0 'setting dominance through weighting
        M_color = FormSettings.colours(colsymb)
        Dim brush2 As New SolidBrush(Color.Empty)
        brush2.Color = modColors.getcolor(M_color, 1, M_light)
        Mred = brush2.Color.R : Mgreen = brush2.Color.G : Mblue = brush2.Color.B
        findcolor = brush2.Color
    End Function

    Private Sub doflathousecolor(ByVal x, ByVal y, ByVal len, ByVal hgt, ByVal div, ByVal subject)
        Dim area As New Rectangle(x, y, len, hgt)
        Dim brush2 As New SolidBrush(Color.Empty)

        If subject = "leftfoundation" Or subject = "leftfooting" Or subject = "ls_frontstep" Then
            If Settings.left = True Then
                brush2.Color = Color.DarkGray
            Else
                brush2.Color = Color.Gray
            End If

        ElseIf subject = "rightfoundation" Or subject = "rightfooting" Or subject = "rs_frontstep" Then
            If Settings.Right = True Then
                brush2.Color = Color.DarkGray
            Else
                brush2.Color = Color.Gray
            End If

        ElseIf subject = "post1left" Or subject = "post2left" Then
            If Settings.left = True Then
                brush2.Color = Color.DarkGray
            Else
                brush2.Color = Color.Gray
            End If

        ElseIf subject = "lefteave" Or subject = "backrighteave" Or subject = "chimneyhole" Or subject = "curtain" Then
            brush2.Color = (Color.Black)

        ElseIf subject = "backwindow" Then
            brush2.Color = Color.Black

        ElseIf subject = "peakeave" Then
            If Settings.Right = True Then
                brush2.Color = Color.DarkGray
            Else
                brush2.Color = Color.Gray
            End If

        ElseIf subject = "fronteave" Or subject = "post1right" Or subject = "post2right" Then
            If Settings.Right = True Then
                brush2.Color = Color.LightGray
            Else
                brush2.Color = Color.DarkGray
            End If

        ElseIf subject = "door" Then
            If Settings.left = True Then
                brush2.Color = Color.DarkGray
            Else
                brush2.Color = (Color.FloralWhite)
            End If

        ElseIf subject = "doorjamb" Or subject = "doorjamb2" Or subject = "bkwindowcasing" _
        Or subject = "bkwindowcasing2" Then
            brush2.Color = Color.LightGray
        End If


        If FormSettings.scheme > 9 Then
            doflatCMBcolor(x, y, len, hgt, div, subject)
        Else
            doflatREGcolor(x, y, len, hgt, div, subject)
        End If
        g.FillRectangle(brush2, area)
    End Sub
    Private Sub setuphousecolorsCMB(ByVal type)
        Dim xtone As Integer
        Dim matchcolors As String = "333333333"
        Dim cmbseason As Integer = FormSettings.scheme - 10  'changing from 11, 12, 13, etc
        If cmbseason = 0 Then
            matchcolors = "001101 101201 002202 003002 004002 005003 005002 006107 006108 006002 004014 004006 010106 102208 011209 113203 014213 008211 004113 012203"
        ElseIf cmbseason = 1 Then
            matchcolors = "001203,002013,002004,003101,003204,004202,004203,005101,005003,006109,007211,008210,009206,010207,011208,012208,011107,013212,013213,014114"
        ElseIf cmbseason = 2 Then
            matchcolors = "002101,002202,103204,003210,004101,004107,005207,006105,009208,010101,010001,011211,012101,012110,013202,104014,002101,104003,114014,110210"
        ElseIf cmbseason = 3 Then
            matchcolors = "001002,101202,002203,003004,003005,004101,109110,005109,006102,006014,007107,007108,008013,009211,010109,011208,013113,014209,104207,109212"
        End If

        Dim clevel As Integer = c_randh(1, 20)
        Dim um1 As Integer = (clevel - 1) * 7 + 1
        xtone = ((Mid((matchcolors), um1, 1)))
        If type = "walls" Then
            wallslttone = xtone
        ElseIf type = "roof" Then
            rooflttone = xtone
        ElseIf type = "chimney" Then
            chimneylttone = xtone
        ElseIf type = "deck" Then
            decklttone = xtone
        End If

        Dim um2 As Integer = um1 + 1
        xtone = CInt(Mid(matchcolors, um2, 2))
        If type = "walls" Then
            wallsltcolor = xtone
        ElseIf type = "roof" Then
            roofltcolor = xtone
        ElseIf type = "chimney" Then
            chimneyltcolor = xtone
        ElseIf type = "deck" Then
            deckltcolor = xtone
        End If

        Dim um3 As Integer = um2 + 2
        xtone = CInt(Mid(matchcolors, um3, 1))
        If type = "walls" Then
            wallsdktone = xtone
        ElseIf type = "roof" Then
            roofdktone = xtone
        ElseIf type = "chimney" Then
            chimneydktone = xtone
        ElseIf type = "deck" Then
            deckdktone = xtone
        End If

        Dim um4 As Integer = um3 + 1
        xtone = CInt(Mid(matchcolors, um4, 2))
        If type = "walls" Then
            wallsdkcolor = xtone
        ElseIf type = "roof" Then
            roofdkcolor = xtone
        ElseIf type = "chimney" Then
            chimneydkcolor = xtone
        ElseIf type = "deck" Then
            deckdkcolor = xtone
        End If
    End Sub
    Private Sub setuphousecolorsREG(ByVal type)
        Dim colsymb, domin As Integer
        If FormSettings.scheme > 0 And FormSettings.scheme < 3 Then domin = 1 Else domin = 2
        colsymb = c_randh(0, FormSettings.scheme * domin)
        If colsymb > FormSettings.scheme Then colsymb = 0 'setting dominance through weighting

        M_color = FormSettings.colours(colsymb)

        If type = "walls" Then wallsltcolor = M_color
        If type = "roof" Then roofltcolor = M_color
        If type = "chimney" Then chimneyltcolor = M_color
        If type = "deck" Then deckltcolor = M_color
        Dim xlight As Integer = c_randh(4, 8)

        If type = "walls" Then
            wallslttone = xlight
            wallsdktone = xlight - 2
        ElseIf type = "roof" Then
            rooflttone = xlight
            roofdktone = xlight - 2
        ElseIf type = "chimney" Then
            chimneylttone = xlight
            chimneydktone = xlight - 2
        ElseIf type = "deck" Then
            decklttone = xlight
            deckdktone = xlight - 2
        End If


    End Sub
    Private Sub doflatCMBcolor(ByVal x, ByVal y, ByVal len, ByVal hgt, ByVal div, ByVal subject)  'div as picture division, top to bottom 1/3
        'from doflatcolor()
        Dim sunleft = Settings.left : Dim sunright = Settings.Right : Dim sunabove = Settings.Above
        'Dim sunright As settings.right
        Dim area As New Rectangle(x, y, len, hgt)
        Dim cmbseason As Integer = FormSettings.scheme - 10
        'set tone by tonal composition, based on the placement of the drawn object, sent as a number 1 - 3
        Dim tone As Integer = randh(1, 3) ' number 1,2 or 3 (1=light, med, or Dark)
        'with the following exceptions
        Dim brush2 As New SolidBrush(Color.Empty)

        If subject = "buildbox_side1" Or subject = "peakside" Then        'left side of building
            If sunright = True Then
                tone = wallsdktone : color14 = wallsdkcolor
            Else
                tone = wallslttone : color14 = wallsltcolor
            End If

        ElseIf subject = "buildbox_side2" Or subject = "nonpeakside" Then 'right side
            If sunleft = True Then
                tone = wallsdktone : color14 = wallsdkcolor
            Else
                tone = wallslttone : color14 = wallsltcolor
            End If

        ElseIf subject = "backroof" Then
            If sunright = True Or rooftest = "dark" Then
                tone = roofdktone : color14 = roofdkcolor
            Else
                tone = rooflttone : color14 = roofltcolor
            End If

        ElseIf subject = "rightroof" Then
            If sunleft = True Then
                tone = roofdktone : color14 = roofdkcolor
            Else
                tone = rooflttone : color14 = roofltcolor
            End If

        ElseIf subject = "leftshade" Or subject = "v_aboveshade" Or subject = "frontshade" Then
            tone = wallsdktone : color14 = wallsdkcolor

            'ElseIf subject = "window" Or subject = "windowbottom" Then
            '    If sunleft = True Then
            '        tone = rooflttone : color14 = roofltcolor
            '    Else
            '        tone = roofdktone : color14 = roofdkcolor
            '    End If

            'ElseIf subject = "backwindow" Then
            '    If sunright = True Then
            '        tone = rooflttone : color14 = roofltcolor
            '    Else
            '        tone = roofdktone : color14 = roofdkcolor
            '    End If

        ElseIf subject = "chimneyrtside" Then
            If sunleft = True Then
                tone = chimneydktone : color14 = chimneydkcolor
            Else
                tone = chimneylttone : color14 = chimneyltcolor
            End If

        ElseIf subject = "chimneyleftside" Then
            If sunright = True Then
                tone = chimneydktone : color14 = chimneydkcolor
            Else
                tone = chimneylttone : color14 = chimneyltcolor
            End If

        ElseIf subject = "chimneyshadow" Or subject = "pipeshadow" Then
            tone = roofdktone : color14 = roofdkcolor

        ElseIf subject = "deckfrontside" Then
            If sunright = True Then
                tone = decklttone : color14 = deckltcolor
            Else
                tone = deckdktone : color14 = deckdkcolor
            End If

        ElseIf subject = "deckleftside" Then
            If sunleft = True Then
                tone = decklttone : color14 = deckltcolor
            Else
                tone = deckdktone : color14 = deckdkcolor
            End If

        ElseIf subject = "deckbackside then" Then
            tone = deckdktone : color14 = deckdkcolor

        ElseIf subject = "deckfrontjoist" Then
            If sunright = True Then
                tone = wallslttone : color14 = wallsltcolor
            Else
                tone = wallsdktone : color14 = wallsdkcolor
            End If

        ElseIf subject = "deckleftjoist" Then
            If sunleft = True Then
                tone = wallslttone : color14 = wallsltcolor
            Else
                tone = wallsdktone : color14 = wallsdkcolor
            End If


        Else : tone = c_randh(0, 2) 'by random selection
            color14 = c_randh(1, 14)       '14 colors in each CMB level
        End If
        If color14 < 1 Then Stop
        Dim brush As New SolidBrush(Color.Empty)
        brush.Color = ModCMBcolors.getCMBcolor(cmbseason, tone, color14)
        g.FillRectangle(brush, area)

    End Sub

    Private Sub doflatREGcolor(ByVal x, ByVal y, ByVal len, ByVal hgt, ByVal div, ByVal subject)
        Dim sunleft = Settings.left : Dim sunright = Settings.Right : Dim sunabove = Settings.Above
        'intensity and tone are separate variables in NonCMB colors
        'set tone by tonal composition, based on the placement of the drawn object, sent as a number 1 - 3
        'Dim tone As Integer = CInt(Mid(Settings.tonalcomp, div, 1)) ' number 1,2 or 3 (1=light, med, or Dark)
        Dim area As New Rectangle(x, y, len, hgt)
        Dim brush2 As New SolidBrush(Color.Empty)
        'Sun direction and shadows on building
        If subject = "buildbox_side1" Or subject = "peakside" Then
            M_color = wallsltcolor
            If sunright = True Then
                M_light = wallsdktone
            Else
                M_light = wallslttone
            End If

        ElseIf subject = "buildbox_side2" Or subject = "nonpeakside" Then
            M_color = wallsltcolor
            If sunleft = True Then
                M_light = wallsdktone
            Else
                M_light = wallslttone
            End If

        ElseIf subject = "backroof" Then
            M_color = roofltcolor
            If sunright = True Or rooftest = "dark" Then
                M_light = roofdktone
            Else
                M_light = rooflttone
            End If

        ElseIf subject = "rightroof" Then
            M_color = roofltcolor
            If sunleft = True Then
                M_light = roofdktone
            Else
                M_light = rooflttone
            End If

        ElseIf subject = "v_aboveshade" Or subject = "frontshade" Or subject = "leftshade" Then
            M_color = wallsltcolor
            M_light = wallsdktone

        ElseIf subject = "window" Or subject = "windowbottom" Then
            M_color = roofltcolor
            If sunleft = True Then
                M_light = rooflttone
            Else
                M_light = roofdktone
            End If

        ElseIf subject = "backwindow" Then
            M_color = roofltcolor
            If sunright = True Then
                M_light = rooflttone
            Else
                M_light = roofdktone
            End If

        ElseIf subject = "chimneyrtside" Then
            M_color = chimneyltcolor
            If sunleft = True Then
                M_light = chimneydktone
            Else
                M_light = chimneylttone
            End If

        ElseIf subject = "deckleftside" Then
            M_color = deckltcolor
            If sunleft = True Then
                M_light = decklttone
            Else
                M_light = deckdktone
            End If

        ElseIf subject = "deckfrontside" Then
            M_color = deckltcolor
            If sunright = True Then
                M_light = decklttone
            Else
                M_light = deckdktone
            End If

        ElseIf subject = "deckbackside" Then
            M_color = deckdkcolor : M_light = deckdktone

        ElseIf subject = "deckfrontjoist" Then
            M_color = wallsltcolor
            If sunright = True Then
                M_light = wallslttone
            Else
                M_light = wallsdktone
            End If

        ElseIf subject = "deckleftjoist" Then
            M_color = wallsltcolor
            If sunleft = True Then
                M_light = wallslttone
            Else
                M_light = wallsdktone
            End If

        ElseIf subject = "chimneyleftside" Then
            M_color = chimneyltcolor
            If sunright = True Then
                M_light = chimneydktone
            Else
                M_light = chimneylttone
            End If

        ElseIf subject = "chimneyshadow" Or subject = "pipeshadow" Then
            M_color = roofdkcolor : M_light = roofdktone


        Else : Dim colsymb, domin As Integer
            If FormSettings.scheme > 0 And FormSettings.scheme < 3 Then domin = 1 Else domin = 2
            colsymb = c_randh(0, FormSettings.scheme * domin)
            If colsymb > FormSettings.scheme Then colsymb = 0 'setting dominance through weighting
            M_color = FormSettings.colours(colsymb) 'changed M_color
            M_light = c_randh(1, 9)
        End If

        brush2.Color = modColors.getcolor(M_color, 1, M_light)
        Mred = brush2.Color.R : Mgreen = brush2.Color.G : Mblue = brush2.Color.B
        g.FillRectangle(brush2, area)

colorIntensity:
        Dim intensity As Integer = yrand(2)
        Dim grays, grayness As Integer
        Dim gray As Integer = 0

        If intensity = 0 Then grayness = 0
        If intensity = 1 Then grayness = 180
        If intensity = 2 Then grayness = 200

        grays = CInt((Mid("050088120147170190210230245", (M_light * 3 - 2), 3)))

        Dim graybrush As New SolidBrush(Color.Empty)
        graybrush.Color = Color.FromArgb(grayness, grays, grays, grays)
        g.FillRectangle(graybrush, area)

        ' Dim light As Integer = (4 - (light1 + 1)) * 3 - 2  ... old system
        If Settings.side = 2 Then Settings.side = 0

    End Sub
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
        Dim subject As String = "bush" : Dim type As String = "rect"
        If bush_path.GetBounds.Height < 1 Or bush_path.GetBounds.Width < 1 Then g.ResetClip() : Return
        colorblend(bush_path.GetBounds.X, bush_path.GetBounds.Y, bush_path.GetBounds.Width, _
        bush_path.GetBounds.Height, subject, 0)
        'g.FillPath(brushy, bush_path)
        g.ResetClip()
        If Settings.Outlines = True Then g.DrawPath(stainedglasspen, bush_path)


    End Sub
    Private Sub colorblend(ByVal beginx, ByVal beginy, ByVal xwidth, ByVal yheight, ByVal subject, ByVal type_fill)
        If type_fill < 2 Then colormem = 0 'type_fill determines if objects in a series are to be same color
        If FormSettings.scheme > 9 Then
            dualCMBblend(beginx, beginy, xwidth, yheight, subject, type_fill)
        Else
            dualcolorblend(beginx, beginy, xwidth, yheight, subject, type_fill)
            Return
        End If
    End Sub

    Private Sub dualCMBblend(ByVal beginx, ByVal beginy, ByVal xwidth, ByVal yheight, ByVal subject, ByVal type_fill)
        Dim colors1 As Integer = 0
        Dim darkness, cmbseason, lightera, darkera, uplight, lowlight, applysun As Integer
        Dim area As New Rectangle(beginx, beginy, xwidth, yheight)
        cmbseason = FormSettings.scheme - 10  'CMB color seasons are from 0 to 3
        applysun = 0
striateclouds:
        If subject = "striate" Then
            darkness = 0
            Dim striate_color As Integer = c_randh(1, 4)
            If cmbseason = 0 Then colors1 = CInt(Mid("04051014", striate_color * 2 - 1, 2))
            If cmbseason = 1 Then colors1 = CInt(Mid("01020308", striate_color * 2 - 1, 2))
            If cmbseason = 2 Then colors1 = CInt(Mid("02050712", striate_color * 2 - 1, 2))
            If cmbseason = 3 Then colors1 = CInt(Mid("02040507", striate_color * 2 - 1, 2))
            lightera = 180 : uplight = 255
            darkera = 15 : lowlight = 25
            applysun = 1
            GoTo colorit
        End If

River:
        If subject = "river" Then
            'random choice of selected colors for each season
            darkness = 2
            Dim color_river As Integer = c_randh(1, 5)
            If cmbseason = 0 Then colors1 = CInt(Mid("0405060711", color_river * 2 - 1, 2))
            If cmbseason = 1 Then colors1 = CInt(Mid("0102030510", color_river * 2 - 1, 2))
            If cmbseason = 2 Then colors1 = CInt(Mid("0304060809", color_river * 2 - 1, 2))
            If cmbseason = 3 Then colors1 = CInt(Mid("0205061213", color_river * 2 - 1, 2))
            lightera = 200 : uplight = 255
            darkera = 200 : lowlight = 0
            GoTo colorit
        End If
riverbeach:
        If subject = "beach" Then
            darkness = 1
            Dim color_beach As Integer = c_randh(1, 4)
            If cmbseason = 0 Then colors1 = CInt(Mid("01050614", color_beach * 2 - 1, 2))
            If cmbseason = 1 Then colors1 = CInt(Mid("01081011", color_beach * 2 - 1, 2))
            If cmbseason = 2 Then colors1 = CInt(Mid("01040711", color_beach * 2 - 1, 2))
            If cmbseason = 3 Then colors1 = CInt(Mid("01050608", color_beach * 2 - 1, 2))
            lightera = 190 : uplight = 255
            darkera = 160 : lowlight = 0
            GoTo colorit
        End If

sea:
        If subject = "sea" Then
            darkness = 1
            Dim color_sea As Integer = c_randh(1, 4)
            If cmbseason = 0 Then colors1 = CInt(Mid("01031014", color_sea * 2 - 1, 2))
            If cmbseason = 1 Then colors1 = CInt(Mid("02030405", color_sea * 2 - 1, 2))
            If cmbseason = 2 Then colors1 = CInt(Mid("02030606", color_sea * 2 - 1, 2))
            If cmbseason = 3 Then colors1 = CInt(Mid("01040910", color_sea * 2 - 1, 2))
            lightera = 170 : uplight = 255
            darkera = 200 : lowlight = 0
            GoTo colorit
        End If

bg_mountains:
        If subject = "bg_mountains" Then '      background mountains
            darkness = 2
            Dim color_bg_m As Integer = c_randh(1, 3)
            If cmbseason = 0 Then colors1 = CInt(Mid("071011", color_bg_m * 2 - 1, 2))
            If cmbseason = 1 Then colors1 = CInt(Mid("020306", color_bg_m * 2 - 1, 2))
            If cmbseason = 2 Then colors1 = CInt(Mid("050609", color_bg_m * 2 - 1, 2))
            If cmbseason = 3 Then colors1 = CInt(Mid("021013", color_bg_m * 2 - 1, 2))
            lightera = bgmtn : uplight = 250
            darkera = bgmtn : lowlight = 150
            bgmtn -= 28
            If bgmtn < 0 Then bgmtn += 30
            applysun = 1
            GoTo colorit
        End If

land:   'basic warm land colors within CMB season, normally covered over by other things
        If subject = "land" Then
            If type_fill = 1 Then
                darkness = 1
                Dim color_land As Integer = c_randh(1, 3) 'number of color choices OK for that season
                If cmbseason = 0 Then colors1 = CInt(Mid("060708", color_land * 2 - 1, 2))
                If cmbseason = 1 Then colors1 = CInt(Mid("010708", color_land * 2 - 1, 2))
                If cmbseason = 2 Then colors1 = CInt(Mid("010809", color_land * 2 - 1, 2))
                If cmbseason = 3 Then colors1 = CInt(Mid("060708", color_land * 2 - 1, 2))
                lightera = 215 : uplight = 255
                darkera = 190 : lowlight = 0
                applysun = 1
                GoTo colorit
            Else
                darkness = 0
                Dim color_land As Integer = c_randh(1, 4)
                If cmbseason = 0 Then colors1 = CInt(Mid("04051113", color_land * 2 - 1, 2))
                If cmbseason = 0 Then colors1 = CInt(Mid("02040506", color_land * 2 - 1, 2))
                If cmbseason = 0 Then colors1 = CInt(Mid("02050609", color_land * 2 - 1, 2))
                If cmbseason = 0 Then colors1 = CInt(Mid("05060911", color_land * 2 - 1, 2))
                lightera = 215 : uplight = 255
                darkera = 100 : lowlight = 0
            End If
        End If
gnarly:
        If subject = "gnarly" Then
            Dim colorgnarly As Integer = c_randh(1, 4)
            darkness = 2
            If cmbseason = 0 Then colors1 = CInt(Mid("01040111", colorgnarly * 2 - 1, 2))
            If cmbseason = 1 Then colors1 = CInt(Mid("01031010", colorgnarly * 2 - 1, 2))
            If cmbseason = 2 Then colors1 = CInt(Mid("01040512", colorgnarly * 2 - 1, 2))
            If cmbseason = 3 Then colors1 = CInt(Mid("01020204", colorgnarly * 2 - 1, 2))
            lightera = 150 : uplight = 10
            darkera = 150 : lowlight = 30
            GoTo colorit
        End If

pinetree:
        If subject = "pineleaves" Or subject = "xmas" Or subject = "bushleaves" Then 'pineleaves refers to the shapely clumps of needles
            Dim colorpine As Integer = c_randh(1, 2)
            darkness = 2
            If cmbseason = 0 Then colors1 = CInt(Mid("0910", colorpine * 2 - 1, 2))
            If cmbseason = 1 Then colors1 = CInt(Mid("0607", colorpine * 2 - 1, 2))
            If cmbseason = 2 Then colors1 = CInt(Mid("0308", colorpine * 2 - 1, 2))
            If cmbseason = 3 Then colors1 = CInt(Mid("0708", colorpine * 2 - 1, 2))

            If colormem <> 0 Then colors1 = colormem
            If type_fill = 1 Then colormem = colors1
            lightera = 150 : uplight = 230
            darkera = 100 : lowlight = 30
            applysun = 1
            GoTo colorit
        End If

        If subject = "pinetree" Then 'refers to the main trunk 
            Dim colorpine As Integer = c_randh(1, 2)
            darkness = 2
            If cmbseason = 0 Then colors1 = CInt(Mid("0411", colorpine * 2 - 1, 2))
            If cmbseason = 1 Then colors1 = CInt(Mid("1011", colorpine * 2 - 1, 2))
            If cmbseason = 2 Then colors1 = CInt(Mid("0914", colorpine * 2 - 1, 2))
            If cmbseason = 3 Then colors1 = CInt(Mid("0304", colorpine * 2 - 1, 2))
            lightera = 180 : uplight = 80
            darkera = 10 : lowlight = 0
            GoTo colorit
        End If

river_mountains:
        If subject = "river_mountains" Then  'most all colors on level two will work
            darkness = randh(1, 2)
            colors1 = randh(1, 14)
            lightera = 150 : uplight = 250
            darkera = 140 : lowlight = 0
            applysun = 1
            GoTo colorit
        End If

river_trees:
        If subject = "river trees" Then
            darkness = randh(0, 2)
            colors1 = randh(1, 14)
            lightera = 0 : uplight = 30
            darkera = 240 : lowlight = 30
            applysun = 1
            GoTo colorit
        End If
trunk:
        If subject = "trunk" Then
            darkness = 2
            colors1 = randh(1, 14)
            lightera = 0 : uplight = 30
            darkera = 240 : lowlight = 50
        End If
posts:
        If subject = "post" Then
            darkness = randh(0, 2)
            colors1 = randh(1, 14)
            lightera = 230 : uplight = 250
            darkera = 240 : lowlight = 30
            applysun = 1
            GoTo colorit
        End If

other:
        'If subject = "other" Or subject = "trunk" etc
        darkness = c_randh(0, 2)
        colors1 = randh(1, 14)
        lightera = 150 : uplight = 255
        darkera = 150 : lowlight = 0
        applysun = 1
        GoTo colorit

colorit:
        ' present underlying basic color 
        Dim CMBbrush As New SolidBrush(Color.Empty)
        CMBbrush.Color = ModCMBcolors.getCMBcolor(cmbseason, darkness, colors1)
        g.FillRectangle(CMBbrush, area)


Sunlightandshadowit:
        If subject = "pineleaves" Or subject = "pinetree" Or subject = "trunk" Then
            Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
            Color.FromArgb(lightera, uplight, uplight, uplight), _
            Color.FromArgb(200, 0, 0, 0), _
            Drawing2D.LinearGradientMode.Vertical) 'darker at bottom.  
            g.FillRectangle(Brush3, area)
            Return
        End If
        If subject = "land" Or subject = "bushleaves" Then
            Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
                           Color.FromArgb(255, Mred, Mgreen, Mblue), _
                           Color.FromArgb(170, CMBbrush.Color), _
                           Drawing2D.LinearGradientMode.Vertical)
            g.FillRectangle(Brush3, area)
            Return
        End If
        If subject = "xmas" Then
            If Settings.left = True Then
                Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
                Color.FromArgb(130, 200, 200, 200), _
                Color.FromArgb(170, 0, 0, 0), _
                Drawing2D.LinearGradientMode.Horizontal)
                g.FillRectangle(Brush3, area)
                Return
            Else
                If Settings.Right = True Then
                    Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
                               Color.FromArgb(170, 30, 30, 30), _
                               Color.FromArgb(130, 200, 200, 200), _
                               Drawing2D.LinearGradientMode.Horizontal)
                    g.FillRectangle(Brush3, area)
                    Return
                End If
            End If
        End If
        If applysun = 0 Or Settings.Above = True Then
            Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
                            Color.FromArgb(lightera, uplight, uplight, uplight), _
                            Color.FromArgb(darkera, lowlight, lowlight, lowlight), _
                            Drawing2D.LinearGradientMode.Vertical)
            g.FillRectangle(Brush3, area)
        End If

        If applysun = 1 Then
            If Settings.Right = True Then 'backwardDiagonal
                Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
                            Color.FromArgb(lightera, uplight, uplight, uplight), _
                            Color.FromArgb(darkera, lowlight, lowlight, lowlight), _
                            Drawing2D.LinearGradientMode.BackwardDiagonal)
                g.FillRectangle(Brush3, area)
                Return
            End If
            If Settings.left = True Then  'forward diagonal
                Dim Brush3 = New Drawing2D.LinearGradientBrush(area, _
                             Color.FromArgb(lightera, uplight, uplight, uplight), _
                             Color.FromArgb(darkera, lowlight, lowlight, lowlight), _
                             Drawing2D.LinearGradientMode.ForwardDiagonal)
                g.FillRectangle(Brush3, area)
            End If
            If subject = "post" Then
                Dim Brush3 As New Drawing2D.LinearGradientBrush(area, _
                             Color.FromArgb(lightera, uplight, uplight, uplight), _
                             Color.FromArgb(darkera, lowlight, lowlight, lowlight), _
                             Drawing2D.LinearGradientMode.Horizontal)
                g.FillRectangle(Brush3, area)
            End If
            applysun = 0
        End If
    End Sub


    Private Sub dualcolorblend(ByVal x, ByVal y, ByVal wid, ByVal hgt, ByVal subject, ByVal type_fill)
        Dim area As New Rectangle(x, y, wid, hgt)
        Dim domin, dom, lowlight, shadecolor, shadegray, shadelight, colorno, bs As Integer
        Dim uppercolor, lowercolor, highlight
        lowlight = c_randh(4, 7)
        highlight = lowlight + 2

        'To get Mlight - pick lightness of color in terms of a small range of colors
        If subject = "striate" Then lowlight = 8 : highlight = 9
        If subject = "bg_mountains" Then lowlight = 5 : highlight = 7
        If subject = "river" Then lowlight = 5 : highlight = 8
        If subject = "beach" Then lowlight = 8 : highlight = 9
        If subject = "river trees" Then lowlight = 5 : highlight = 8
        If subject = "sea" Then lowlight = 4 : highlight = 8
        If subject = "land" Then lowlight = 4 : highlight = 7
        If subject = "birchstand" Then lowlight = 8 : highlight = 9
        If subject = "leaves" Then lowlight = 3 : highlight = 8
        If subject = "pineleaves" Then lowlight = 3 : highlight = 5
        If subject = "river_mountains" Then lowlight = 1 : highlight = 7
        If subject = "trunk" Then lowlight = 1 : highlight = 2
        'If subject = "rivertrunk" Then lowlight = 2 : highlight = 3

        'setting color dominance through weighting
        If FormSettings.scheme > 3 Then domin = 1 Else domin = 2 'a functional number, for dominant color
        dom = FormSettings.scheme * domin
        colorno = c_randh(0, dom)
        If colorno > FormSettings.scheme Then colorno = 0

        M_color = FormSettings.colours(colorno) ' the basic color itself
        If colormem <> 0 Then M_color = colormem
        If type_fill = 1 Then colormem = M_color
        M_gray = c_randh(1, 3) : If subject = "pines" Then M_gray = 3 'saturation level
        If subject = "flower" Then M_gray = 1
        M_light = c_randh(lowlight, highlight) ' refers to light or darkness of a color

        uppercolor = modColors.getcolor(M_color, M_gray, M_light)
        Dim brushupper As New SolidBrush(Color.Empty)
        brushupper.Color = uppercolor
        If subject = "birchstand" Then
            bs = c_randh(1, 3)
            If bs = 2 Then
                brushupper.Color = Color.White
            ElseIf bs = 3 Then
                brushupper.Color = Color.DarkGray
            End If
        End If
        g.FillRectangle(brushupper, area)
shade:
        'determine a darker color to act as shadow to uppercolor
        Dim shadelight_low, shadelight_high As Integer
        shadecolor = M_color  'keep same color
        If subject = "river" Then shadecolor = skycolor
        shadegray = M_gray 'keep same saturation
        shadelight_low = 1 'darkest representation of that color and saturation
        shadelight_high = 9 'lightest representation of that color and saturation
        If subject = "trunk" Then shadelight_high = 5
        If subject = "striate" Then shadelight = M_light - 1
        ' If subject = "bg_mountains" Then shadelight = 8

        lowercolor = modColors.getcolor(shadecolor, shadegray, shadelight_low)
        uppercolor = modColors.getcolor(shadecolor, shadegray, shadelight_high) 'get new uppercolor
        'make shading vertical
        Dim n As Integer
        If subject = "striate" Or Settings.Above = True Or subject = "river" Then
            n = 100
            If subject = "river" Then n = 220
            Dim blendbrushv As New Drawing2D.LinearGradientBrush(area, _
                       Color.FromArgb(n, uppercolor.r, uppercolor.g, uppercolor.b), _
                       Color.FromArgb(255, lowercolor.R, lowercolor.G, lowercolor.b), _
                               Drawing2D.LinearGradientMode.Vertical)
            g.FillRectangle(blendbrushv, area)
            Exit Sub
        End If

        'make shading horizontal
        If subject = "leaves" Or subject = "trunky" Or subject = "birch" Or subject = "trunk" Then
            Dim blendbrushh As New Drawing2D.LinearGradientBrush(area, _
                            Color.FromArgb(100, uppercolor.r, uppercolor.g, uppercolor.b), _
                            Color.FromArgb(255, lowercolor.R, lowercolor.G, lowercolor.b), _
                                    Drawing2D.LinearGradientMode.Horizontal)
            g.FillRectangle(blendbrushh, area)
            Exit Sub
        End If
        If subject = "birchstand" Then
            Dim blendbrushh As New Drawing2D.LinearGradientBrush(area, _
                                      Color.FromArgb(255, 255, 255, 255), _
                                      Color.FromArgb(255, 0, 0, 0), _
                                              Drawing2D.LinearGradientMode.Horizontal)
            g.FillRectangle(blendbrushh, area)
            Exit Sub
        End If
        If subject = "trunk" Then
            Dim blendbrushd As New Drawing2D.LinearGradientBrush(area, _
                                  Color.FromArgb(100, uppercolor.r, uppercolor.g, uppercolor.b), _
                                  Color.FromArgb(200, 0, 0, 0), _
                                          Drawing2D.LinearGradientMode.Horizontal)
            g.FillRectangle(blendbrushd, area)
            Exit Sub
        End If
        'make shading diagonal
        If Settings.left = "true" Then
            Dim blendbrushd As New Drawing2D.LinearGradientBrush(area, _
                                  Color.FromArgb(100, uppercolor.r, uppercolor.g, uppercolor.b), _
                                  Color.FromArgb(255, lowercolor.R, lowercolor.G, lowercolor.b), _
                                          Drawing2D.LinearGradientMode.ForwardDiagonal)
            g.FillRectangle(blendbrushd, area)
        End If

        'make shading backdiagonal
        If Settings.Right = "true" Then
            Dim blendbrushbd As New Drawing2D.LinearGradientBrush(area, _
                                          Color.FromArgb(100, uppercolor.r, uppercolor.g, uppercolor.b), _
                                          Color.FromArgb(255, lowercolor.R, lowercolor.G, lowercolor.b), _
                                                  Drawing2D.LinearGradientMode.BackwardDiagonal)
            g.FillRectangle(blendbrushbd, area)
        End If

        'If subject = "bg_mountains" Then
        '    Dim mistbrush As New SolidBrush(Color.FromArgb(bgmtn, 250, 250, 250))
        '    g.FillRectangle(mistbrush, area)
        '    bgmtn -= 28 : If bgmtn < 0 Then bgmtn = 30
        'End If
    End Sub

    Private Sub makepathpoints()
        Dim x, y As Integer
        For n As Integer = 1 To 123
            x = riverpts(n).x - 2 : y = riverpts(n).y - 2
            Dim pen As New Pen(Color.Black)
            g.DrawEllipse(pen, x, y, 5, 5)
        Next

    End Sub

    Private Sub CMBcolordisplay()
        Dim x, y, sy, width, height, l, t, m As Integer

        x = 90
        y = 20
        width = 30
        height = 30
        l = 5
        For i As Integer = 0 To 3
            For j As Integer = 0 To 2
                If j = 1 Then

                End If
                For k As Integer = 1 To 14
                    Dim box As New Rectangle(x, y, 20, 20)
                    Dim paintbrush As New SolidBrush(Color.Black)
                    paintbrush.Color = ModCMBcolors.getCMBcolor(i, j, k)
                    g.FillRectangle(paintbrush, box)
                    x += 27
                Next k

                x = 90
                y += 22
            Next j
            x = 90
            y += 30
            g.DrawString("1       2        3       4       5      6       7        8      9      10     11     12     13     14", _
                                         New Font("arial", 8), Brushes.Black, New RectangleF(96, l, 800, 14))
            l += 96
        Next i
        Dim cmbtypestring, darkness_string As String
        y = 45
        sy = 23
        For t = 0 To 3
            cmbtypestring = "CMB" & " " & CStr(t)
            g.DrawString(cmbtypestring, New Font("arial", 8), Brushes.Black, New RectangleF(10, y, 800, 14))
            y += 97
            For m = 0 To 2
                darkness_string = CStr(m)
                g.DrawString(darkness_string, New Font("arial", 8), Brushes.Black, New RectangleF(77, sy, 800, 14))
                sy += 22
            Next
            sy += 30
        Next

    End Sub

    Private Sub colordisplayreg()
        Dim x, y, i, j, k, n, e, f, b As Integer
        b = 15
        x = 16

        e = 1
        f = 9
        For n = 1 To 2
            If n = 1 Then
                g.DrawString("       Y(1)                   OY(2)                Y0(3)                 O(4)                   R0(5)                OR(6)                  R(7)                  PR(8)                  RP(9)", _
                New Font("arial", 8), Brushes.Black, New RectangleF(16, 2, 800, 14))
            Else
                g.DrawString("      P(10)                 BP(11)               PB(12)                B(13)                 GB(14)               BG(15)                G(16)                YG(17)              GY(18)", _
                       New Font("arial", 8), Brushes.Black, New RectangleF(16, 202, 800, 14))
            End If
            y = b
            For i = e To f
                For j = 1 To 3
                    For k = 9 To 1 Step -1
                        Dim box As New Rectangle(x, y, 20, 16)
                        Dim paintbrush As New SolidBrush(Color.Black)
                        paintbrush.Color = modColors.getcolor(i, j, k) 'color, grayness, lightness
                        g.FillRectangle(paintbrush, box)
                        y += 20
                    Next k
                    x += 23
                    y = b
                Next j
                x += 10
            Next i
            x = 16
            b = 215
            e = 10
            f = 18
        Next n
    End Sub

    Private Sub makeskyfluffclouds()
        Dim x, y, a, b, c, d As Integer

        Dim fluffwidth As Integer = randh(300, 400)
        Dim fluffheight As Integer = randh(20, 40)
        ''Fill an ellipse setting CenterColor and SurroundColors.
        For n As Integer = 1 To yrand(2)
            ' x = yrand(Image.Width) : y = randh(Image.Height * 0.2, Image.Height * 0.7)
            a = 150 : b = Image.Width - 200 : c = horizon - 30 : d = horizon + 20
            x = randh(a, b) : y = randh(c, d)

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

        Dim fluffwidth As Integer = randh(200, 450)
        Dim fluffheight As Integer = randh(25, 340)
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
    Private Sub ellipseondark()
        Dim a As Integer = 200
        Dim colors1 As Integer
        Dim rect As New Rectangle(0, 0, Image.Width, Image.Height)
        Dim rec_brush As New SolidBrush(Color.Black)
        If FormSettings.scheme > 9 Then cmbseason = FormSettings.scheme - 10
        Dim color_land As Integer = c_randh(1, 3)
        If cmbseason = 0 Then colors1 = CInt(Mid("010411", color_land * 2 - 1, 2))
        If cmbseason = 1 Then colors1 = CInt(Mid("101112", color_land * 2 - 1, 2))
        If cmbseason = 2 Then colors1 = CInt(Mid("010513", color_land * 2 - 1, 2))
        If cmbseason = 3 Then colors1 = CInt(Mid("010204", color_land * 2 - 1, 2))

        rec_brush.Color = ModCMBcolors.getCMBcolor(cmbseason, 2, colors1)
        g.FillRectangle(rec_brush, rect)

        cmbseason = FormSettings.scheme - 10 'CMB color seasons from 0 to 3
        Dim color1, tone As Integer
        If cmbseason = 0 Then color1 = 4
        If cmbseason = 1 Then color1 = 2
        If cmbseason = 2 Then color1 = 2
        If cmbseason = 3 Then color1 = 3
        tone = 1

        Dim CMBbrush As New SolidBrush(Color.Orange)
        CMBbrush.Color = ModCMBcolors.getCMBcolor(cmbseason, tone, color1)
        'g.FillRectangle(CMBbrush, rect)

        Dim rect_pts() As Point = {New Point(-a, -a), New Point(Image.Width + a, -a), _
         New Point(Image.Width + a, Image.Height + a), New Point(-a, Image.Height + a)}

        Dim ellipse_path As New Drawing2D.GraphicsPath()
        Dim path_brush As New Drawing2D.PathGradientBrush(rect_pts)
        ellipse_path.AddEllipse(-a, -a, Image.Width + a, Image.Height + a)
        path_brush = New Drawing2D.PathGradientBrush(ellipse_path)

        path_brush.CenterColor = CMBbrush.Color
        path_brush.SurroundColors = New Color() {Color.Empty}
        g.FillEllipse(path_brush, -a, -a, Image.Width + a, Image.Height + a)


    End Sub


    Private Sub bushleaves(ByVal x0, ByVal y0, ByVal distance)
        'x0 and y0 are groundzero points for placing this bushleaf.
        'distances sent to this site should be between 5 ft and 25 ft.
        Dim width, wid, ht, w, ww, h, hh, height, centerx, centery, xx0, yy0 As Single
        Dim x1, y1, r0t, width1, height1, beginx As Integer
        Dim plx, ply, pc, tipx, bleftx, brightx, beginy As Integer
        Dim n, ra, rb, rd, branchx, branchy As Single
        wid = 3 ' in feet 'of leaf clump
        ht = 2 ' in feet

        width = pixelheight(wid, distance) 'of clump size
        height = pixelheight(ht, distance)
        beginx = x0
        beginy = y0 - height
        Dim piepath1, piepath2, trunk, branch, flower As New Drawing2D.GraphicsPath
        Dim leaves As New Rectangle(beginx, beginy, width, height)
        Dim brush1 As New SolidBrush(Color.Empty)

        flower.AddEllipse(leaves)
        g.SetClip(flower)
        r0t = 240 'rotation angle ' for rotation of leaves into hanging downward
        width1 = width * 1.5 'expanded room to thin out leaves in extremities
        height1 = height * 1.5 '  ""
        x1 = beginx - (width1 - width) ' ""
        y1 = beginy - (height1 - height) '  ""
        w = 0.3 'in feet of individual leaf width
        h = 0.3 'in feet of "  " height
        ww = pixelheight(w, distance) 'of individual leaf
        hh = pixelheight(h, distance) '  ""

        For pc = 1 To 2 '           per clump of leaves
            If pc = 2 Then
                width = width1 '  expanded width and height of area to put leaves in
                height = height1
                beginx = x1
                beginy = y1
            End If

            For n = 1 To 25
                xx0 = beginx + yrand(width) 'leaves grouped within a given location: beginx, beginy, height,etc.
                yy0 = beginy + yrand(height)
                ww = ww + ww * 0.01  'rectangular width size of leaf expanding in width
                hh = hh + hh * 0.01 '   "    height of leaf expanding in height
                centerx = xx0 + ww / 2
                centery = yy0 + hh / 2
                Rotate(centerx, centery, r0t)
                piepath1 = New Drawing2D.GraphicsPath
                piepath2 = New Drawing2D.GraphicsPath
                ra = yrand(40)        'beginning point for pie shape - not far from zero degrees
                rb = ra + 140
                rd = ra - 165 ' this much angle needed in reverse to get a similar shape
                piepath1.AddArc(xx0, yy0, ww, hh, ra, rb)
                piepath2.AddArc(xx0, yy0, ww, hh, ra, rd)
                g.SetClip(piepath1)

                colorblend(piepath1.GetBounds.X, piepath1.GetBounds.Y, piepath1.GetBounds.Width, _
                piepath1.GetBounds.Height, "bushleaves", n)
                g.ResetClip()

                g.SetClip(piepath2)
                colorblend(piepath2.GetBounds.X, piepath2.GetBounds.Y, piepath2.GetBounds.Width, _
               piepath2.GetBounds.Height, "bushleaves", n)
                RotateBack(centerx, centery, r0t)
                g.ResetClip()
            Next
        Next
        ' Return
trunk:
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


    End Sub
    Private Sub xmastree(ByVal x0, ByVal y0, ByVal distance)
        Dim ra, rb, plx, ply, pc, pcf, pdf, pcm, pd, plym As Integer
        Dim xmas As New Drawing2D.GraphicsPath

        For i = 1 To 1
            plx = x0
            ply = y0

            pcf = 5 'normal width of a tree at the base
            pdf = 10 'height at base section of tree, will retain this ratio
            pc = pixelheight(pcf, distance)
            pd = pixelheight(pdf, distance)

            ' Dim brush1 As New SolidBrush(Color.Green)
            For n As Integer = 1 To 15

                Dim rect As New Rectangle(plx, ply, pc, pd)
                xmas = New Drawing2D.GraphicsPath
                ra = randh(335, 375) - 20 '355       'points for pie shapes - by angles
                rb = randh(165, 205) + 30 '185
                ' g.FillPie(brush1, rect, ra, rb)
                xmas.AddArc(plx, ply, pc, pd, ra, rb)
                g.SetClip(xmas)
                colorblend(xmas.GetBounds.X, xmas.GetBounds.Y, xmas.GetBounds.Width, xmas.GetBounds.Height, "xmas", n)
                pcm = pc 'pc memory - for determining x movement to center the tree
                g.ResetClip()
                If n = 1 Then membasey = ply + pd 'place height of base of tree in memory
                plym = pd
                pc *= 0.8
                pd *= 0.8
                plx = plx + (pcm - pc) / 2
                ply = ply - (plym - pc) / 3
            Next
        Next

    End Sub

    Private Sub birch()
        Dim x(8), y(8) As Integer

        x(0) = yrand(Image.Width) 'beginning point - upper left
        y(0) = 0
        x(1) = x(0) + randh(10, 36) 'other side of top
        y(1) = 0
        x(2) = x(0) + randsign() * yrand(30) 'bottom left
        y(2) = Image.Height
        x(3) = x(2) + (x(1) - x(0)) + yrand(5) 'bottom right
        y(3) = Image.Height

        Dim point0 As New Point(x(0), y(0))
        Dim point1 As New Point(x(1), y(1))
        Dim point2 As New Point(x(2), y(2))
        Dim point3 As New Point(x(3), y(3))
        Dim trunk As New Drawing2D.GraphicsPath
        trunk.AddLine(point0, point1)
        trunk.AddLine(point1, point3)
        trunk.AddLine(point3, point2)
        trunk.AddLine(point2, point0)
        g.SetClip(trunk)
        colorblend(trunk.GetBounds.X, trunk.GetBounds.Y, trunk.GetBounds.Width, _
            trunk.GetBounds.Height, "birch", 1)
        If Settings.Outlines = True Then g.DrawPath(outlinepen, trunk)
        g.ResetClip()
    End Sub
    Private Sub makemist(ByVal mistlevel) 'no. times mist of 25 - up to a maximum of 10
        Dim a, b As Integer
        a = 25 * mistlevel
        b = 245
        Dim rect5 As New Rectangle(0, 0, Image.Width, Image.Height)
        Dim makemist As New SolidBrush(Color.FromArgb(a, b, b, b))
        g.FillRectangle(makemist, rect5)


        ''  If randsign() = 1 Then
        'Dim brush2 As New Drawing2D.LinearGradientBrush(rect5, _
        'Color.FromArgb(255, (220), (20), (220)), _
        'Color.FromArgb(30, 220, 220, 200), _
        'Drawing2D.LinearGradientMode.Vertical)
        'g.FillRectangle(brush2, rect5)
        ''Else
        'Dim lBrush2 As New Drawing2D.LinearGradientBrush(rect5, _
        '     Color.FromArgb(100, 220, 220, 200), _
        '     Color.FromArgb(200, (220), (220), (200)), _
        '      Drawing2D.LinearGradientMode.Vertical)
        'g.FillRectangle(lBrush2, rect5)
        ' End If


    End Sub
    Public Function randsign()
        If FObjectRandom.NextDouble < 0.5 Then Return -1 Else Return 1
    End Function

    Public Function yrand(ByVal num1 As Integer)
        Return Int(FObjectRandom.NextDouble * num1 + 1)
    End Function
    Public Function randh(ByVal low As Integer, ByVal high As Integer)
        randh = Int(((high - low) + 1) * FObjectRandom.NextDouble + low)
    End Function
    Public Function c_randh(ByVal low As Integer, ByVal high As Integer)
        c_randh = Int(((high - low) + 1) * FColorRandom.NextDouble + low)
    End Function

    Private Sub trialcolorshade()
        Dim uppercolor, lowercolor
        Dim shadecolor, shadegray, shadelight As Integer
        M_color = yrand(18)
        M_gray = randh(2, 3)
        M_light = randh(3, 9)
        uppercolor = modColors.getcolor(M_color, M_gray, M_light)
        Dim brush1 As New SolidBrush(Color.Red)

        Dim area1 As New Rectangle(50, 150, 200, 200)
        Dim area2 As New Rectangle(300, 150, 200, 200)
        Dim area3 As New Rectangle(520, 150, 200, 200)
        Dim area4 As New Rectangle(0, 250, 780, 10)
        g.FillRectangle(brush1, area4)

        'determine a darker color to act as shadow to uppercolor
        shadecolor = M_color
        shadegray = M_gray
        shadelight = 1

        lowercolor = modColors.getcolor(shadecolor, shadegray, shadelight)
        Dim mess As String = CStr(M_light) & " " & CStr(shadelight) & " color: " & CStr(M_color)


        If Settings.left = True Then
            Dim blendbrush1 As New Drawing2D.LinearGradientBrush(area1, _
                            Color.FromArgb(255, uppercolor.r, uppercolor.g, uppercolor.b), _
                            Color.FromArgb(255, lowercolor.R, lowercolor.G, lowercolor.b), _
                                    Drawing2D.LinearGradientMode.Vertical)
            g.FillRectangle(blendbrush1, area1)
            g.DrawString(mess, New Font("arial", 10), Brushes.Black, New RectangleF(50, 210, 100, 30))
        End If
        'lowercolor = modColors.getcolor(shadecolor, shadegray, shadelight)
        If Settings.left = True Then
            Dim blendbrush2 As New Drawing2D.LinearGradientBrush(area2, _
                                    Color.FromArgb(255, uppercolor.r, uppercolor.g, uppercolor.b), _
                                    Color.FromArgb(255, lowercolor.R, lowercolor.G, lowercolor.b), _
                                            Drawing2D.LinearGradientMode.ForwardDiagonal)
            g.FillRectangle(blendbrush2, area2)
            g.DrawString(mess, New Font("arial", 10), Brushes.Black, New RectangleF(300, 210, 100, 30))
        End If

        If Settings.left = True Then
            'lowercolor = modColors.getcolor(shadecolor, shadegray, shadelight)
            Dim blendbrush3 As New Drawing2D.LinearGradientBrush(area3, _
                                           Color.FromArgb(255, uppercolor.r, uppercolor.g, uppercolor.b), _
                                           Color.FromArgb(255, lowercolor.R, lowercolor.G, lowercolor.b), _
                                                   Drawing2D.LinearGradientMode.BackwardDiagonal)
            g.FillRectangle(blendbrush3, area3)
            g.DrawString("black (0)", New Font("arial", 10), Brushes.Black, New RectangleF(550, 210, 100, 30))
        End If
    End Sub
    Public Function pixelheight(ByVal height, ByVal distance)
        Dim y, v As Single
        v = 1
        y = (v * height * 800) / distance
        'y = (v * height * 500) / distance
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
        Dim subject As String = "branch" : Dim type As String = "ellipse"
        colorblend(xbegin1, ybegin1, xwidth1, yheight1, subject, 0)
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
    'Private Sub trialtrees()
    '    Dim x, y, x1, y1, width, height, wid, hgt As Integer
    '    x = 300 : y = 300
    '    ' height  
    '    width = 100 : height = 300
    '    Dim rect As New Rectangle(x, y, width, height)
    '    g.FillRectangle(Brushes.BurlyWood, rect)
    '    Dim path As New Drawing2D.GraphicsPath
    '    path.AddRectangle(rect)
    ' colorblend

    'For n As Integer = 1 To 20
    '    x1 = yrand(width) + x
    '    y1 = yrand(height) + y
    '    wid = yrand(width / 2)
    '    hgt = yrand(height / 2)
    '    rects = New Rectangle(x1, y1, wid, hgt)
    '    g.FillEllipse(brush, rects)

    ' Next



    Private Sub housedesign()
        'from placebuilding()
        Dim wallheight, wallheight_y, peakht, peakface, frontwidth, _
         pl, ph, dd, ff, xtend, sidewidth, eaves As Single

        'lengths in feet
        wallheight = randh(12, 18)
        frontwidth = randh(40, 60)
        sidewidth = randh(25, 50)
        pl = wallheight * 0.4 : ph = wallheight * 0.9

        peakht = randh(pl, ph)
        xtend = randh(1.5, 3.0) 'extension of roof over walls
        eaves = 0.5
        peakface = 1 ' to side, not to front, which is  2 - to be used to select which way house sits

        Bx = Ax
        ' Now convert lengths to new length and pixel placement according to distance
        pl = pixelheight(wallheight, distance)   ' By
        wallheight_y = Ay - pl
        By = wallheight_y
        ff = pixelheight(frontwidth, distance)     ' ff
        dd = pixelheight(sidewidth, distance)       ' dd
        peakht = pixelheight(peakht, distance)      'just the peak height above roof
        xtend = pixelheight(xtend, distance)        'roof extension
        buildbuilding(peakface, peakht, ff, dd, xtend, frontwidth, sidewidth)

    End Sub

    Private Sub buildbuilding(ByVal peakface, ByVal peakheight, ByVal ff, ByVal dd, _
    ByVal xtend, ByVal frontwidth, ByVal sidewidth)

        Dim Gx, Gy, Hx, Hy, Jx, Jy, _
                     uvpx, uvpy, Ictrx, Ictry, Jctrx, Jctry, Imidx, Imidy, rtcornerx, rtcornery, AFx, AFy, _
                    cornerx, cornery, lcornerx, lcornery, IBx, IBy, BEx, BEy, FAx, FAy, _
                    dax, day, uvp2x, uvp2y, peak1x, peak1y, peak2x, peak2y, vbcornerx, _
                    vbcornery, EBx, EBy, peakhtx, peakhty, Lhtx, Lhty, roofmx, roofmy As Single

        If Ay = horizon Then Ay += 1
        If By = horizon Then By += 1
sides:
        Settings.side = 1
        subject = "buildbox_side1"
        buildbox(dd, Cx, Cy, Dx, Dy) 'and get C&D

        Settings.side = 2
        subject = "buildbox_side2"
        buildbox(ff, Ex, Ey, Fx, Fy) ' and get E&F

foundation:  'or footings
        If rowhousetype = "single" Then
            found = 1
            foundation() 'deeper foundation walls, for deck
        Else
            footings()
        End If

        Dim brush As New SolidBrush(Color.Black)
        intersection(RVP, horizon, Cx, Cy, LVP, horizon, Ex, Ey, Gx, Gy) 'get point G

        ' do flat roof
        'subject = "flatroof"
        'If By > horizon Then joinfour(Cx, Cy, Gx, Gy, Ex, Ey, Bx, By, subject)

        'do peaked roof
        intersection(RVP, horizon, Dx, Dy, LVP, horizon, Fx, Fy, Hx, Hy) 'get point H
        ' If peakface = 1 Then 'place peak on narrower side of building
        'for extended roof -   ' Two letters together mean extension point
        intersection(Bx, By, Dx, Dy, Ax, Ay, Cx, Cy, Ictrx, Ictry) 'get point Ictr (front center)
        intersection(Gx, Gy, Fx, Fy, Ex, Ey, Hx, Hy, Jctrx, Jctry)  'get point Jctr (back center)
        intersection(Ictrx, Ictry, Ictrx, 0, Cx, Cy, Bx, By, Imidx, Imidy)  'get point Imid
        Iy = Imidy - peakheight : Ix = Imidx
        If Iy = Cy Then Iy += 0.1
        intersection(Ix, Iy, RVP, horizon, Jctrx, Jctry, Jctrx, 0, Jx, Jy)   'get point J, back peak
        intersection(LVPM, horizon, Ax + ff + xtend, Ay, Ax, Ay, Fx, Fy, AFx, AFy) 'extens.point,Bott.Rt
        intersection(AFx, AFy, AFx, 0, Bx, By, Ex, Ey, BEx, BEy) 'BExy extension
        intersection(Bx, By, Ix, Iy, Ex, Ey, Jx, Jy, uvpx, uvpy) 'get upper vanishing point
        intersection(uvpx, uvpy, BEx, BEy, Ix, Iy, Jx, Jy, peak2x, peak2y) 'get IJxy extension
        intersection(LVPM, horizon, Ax - xtend, Ay, Ax, Ay, Fx, Fy, FAx, FAy) 'get extend point on AF line
        intersection(RVPM, horizon, Ax + xtend, Ay, Ax, Ay, Dx, Dy, dax, day) 'get extend point on AD line
        intersection(dax, day, dax, 0, Ix, Iy, Bx, By, IBx, IBy) ' get IBxy point (from fa extended point
        intersection(FAx, FAy, FAx, 0, Ex, Ey, Bx, By, EBx, EBy) 'get CBxy point (from da extended point)
        intersection(uvpx, uvpy, EBx, EBy, Jx, Jy, Ix, Iy, peak1x, peak1y) 'get front peak point
        intersection(RVP, horizon, IBx, IBy, peak1x, peak1y, EBx, EBy, cornerx, cornery) ' get front corner
        intersection(RVP, horizon, cornerx, cornery, peak2x, peak2y, BEx, BEy, rtcornerx, rtcornery) ' get rtcorner
        intersection(Cx, Cy, Ix, Iy, Gx, Gy, Jx, Jy, uvp2x, uvp2y)   'other vanishing point lower)
        intersection(LVP, horizon, cornerx, cornery, uvp2x, uvp2y, peak1x, peak1y, lcornerx, lcornery) 'left corner
        intersection(RVP, horizon, lcornerx, lcornery, LVP, horizon, rtcornerx, rtcornery, vbcornerx, vbcornery) 'verybackcorner
        intersection(Ax, Ay, Bx, By, peak1x, peak1y, peak2x, peak2y, peakhtx, peakhty) 'get peak height
        intersection(Cx, Cy, Dx, Dy, peak1x, peak1y, lcornerx, lcornery, Lhtx, Lhty) ' get left gable height
        intersection(cornerx, cornery, cornerx, 0, peak1x, peak1y, peak2x, peak2y, roofmx, roofmy) 'get roofm point
        Dim testx, testy As Single
        intersection(vbcornerx, vbcornery, vbcornerx, 0, peak1x, peak1y, lcornerx, lcornery, testx, testy) 'get for color info
        If testy > vbcornery Then rooftest = "light" Else rooftest = "dark"
        'do eaves
        Dim maineavex, maineavey, rteavex, rteavey, pkeavex, pkeavey, leavex, leavey, vbeavex, _
        vbeavey, pk2eavex, pk2eavey, epointx, epointy, midptx, midpty As Single
        Dim eave As Single = pixelheight(0.7, distance)
        maineavex = cornerx
        maineavey = cornery + eave
        intersection(RVP, horizon, maineavex, maineavey, rtcornerx, rtcornery, rtcornerx, 0, rteavex, rteavey) 'right eave point
        intersection(uvpx, uvpy, maineavex, maineavey, peak1x, peak1y, peak1x, 0, pkeavex, pkeavey) 'get peak eave point
        intersection(uvp2x, uvp2y, pkeavex, pkeavey, lcornerx, lcornery, lcornerx, 0, leavex, leavey) 'get left eave point
        intersection(RVP, horizon, leavex, leavey, vbcornerx, vbcornery, vbcornerx, 0, vbeavex, vbeavey) 'get very back eave point
        intersection(uvpx, uvpy, rteavex, rteavey, peak2x, peak2y, peak2x, 0, pk2eavex, pk2eavey) 'get peak2 eave point
        Dim roofptx As Single
        intersection(Cx, Cy, Dx, Dy, lcornerx, lcornery, peak1x, peak1y, roofptx, roofpty) ' get roofpt
        intersection(peak2x, peak2y, peak2x, 0, peak1x, peak1y, cornerx, cornery, epointx, epointy) 'point below/above jy
        Dim testpointx, testpointy As Single
        intersection(cornerx, cornery, cornerx, 0, peak1x, peak1y, peak2x, peak2y, testpointx, testpointy)

        'shadow points
        Dim LS1x, LS1y, LS2x, LS2y, RS1x, RS1y, RS2x, RS2y, FS1x, FS1y, FS2x, FS2y, LRx, LRy, tmidx As Single 'LeftSideShadow1point, RightSideShadow1point etc.
        If Settings.left = True Then 'shade from sun at left of building
            LS1x = Cx : LS1y = Cy + xtend * 3 'leftdiag leftpoint
            intersection(Bx, By, Ix, Iy, uvp2x, uvp2y, LS1x, LS1y, LS2x, LS2y) 'get leftdiag rtpoint.
        ElseIf Settings.Right = True Then
            FS1x = Bx : FS1y = By + xtend * 2
            intersection(FS1x, FS1y, RVP, horizon, Ex, Ey, Fx, Fy, FS2x, FS2y) 'get frtshade, rtpoint
        ElseIf Settings.Above = True Then  ' shade from above building
            LS1x = Cx : LS1y = Cy + xtend * 2 'leftdiag leftpoint,deeper
            intersection(Bx, By, Ix, Iy, uvp2x, uvp2y, LS1x, LS1y, LS2x, LS2y) 'get leftdiag Rtpoint, deeper
            RS1x = Bx : RS1y = By + xtend * 2 'rtdiag,rtpoint
            intersection(RS1x, RS1y, uvpx, uvpy, Ix, Iy, Cx, Cy, RS2x, RS2y) 'get rtdiag leftpoint
            intersection(LS1x, LS1y, LS2x, LS2y, RS2x, RS2y, RS1x, RS1y, LRx, LRy) 'get mid crossover pt
            FS1x = Bx : FS1y = By + xtend * 2 'frtshade, lftpoint
            tmidx = Bx + (Ex - Bx) / 3
            intersection(RS1x, RS1y, Fx, Fy, tmidx, By, tmidx, 0, midptx, midpty) 'get frtshade midpoint
            intersection(midptx, midpty, RVP, horizon, Ex, Ey, Fx, Fy, FS2x, FS2y) 'get frtshade, rtpoint

        End If

        'BUILDING INSTRUCTIONS, after main box and foundation finished
        ' 1. do left eave first
        joinfour(lcornerx, lcornery, leavex, leavey, vbeavex, vbeavey, vbcornerx, vbcornery, "lefteave")

        ' 2. then back roof
        joinfour(lcornerx, lcornery, vbcornerx, vbcornery, peak2x, peak2y, peak1x, peak1y, "backroof")

        ' 3. then right back eave
        joinfour(peak2x, peak2y, pk2eavex, pk2eavey, rteavex, rteavey, rtcornerx, rtcornery, "backrighteave")

        ' 4. Place front roof 
        joinfour(peak2x, peak2y, rtcornerx, rtcornery, cornerx, cornery, peak1x, peak1y, "rightroof")

        ' 5. redo left side of bldg, with roof point added
        buildpeakside()

        ' 6.  redo right side of bldg (after backrighteave in place)
        buildnonpeakside()

        ' 7. Place shadow on left and right sides of building
        If Settings.left = True Then
            joinfour(LS1x, LS1y, LS2x, LS2y, Ix, Iy, Cx, Cy, "leftshade")
        End If

        If Settings.Above = True Then
            joinsix(Cx, Cy, LS1x, LS1y, LRx, LRy, RS1x, RS1y, Bx, By, Ix, Iy, "v_aboveshade")
            joinfive(FS1x, FS1y, midptx, midpty, FS2x, FS2y, Ex, Ey, Bx, By, "frontshade")
        End If

        ' 8. do right roof again if view is not upward
        If testpointy < cornery Then
            joinfour(peak2x, peak2y, rtcornerx, rtcornery, cornerx, cornery, peak1x, peak1y, "rightroof")
        End If

        ' 9. do door and windows
        basicdoor_windows(dd, ff, distance, frontwidth, sidewidth)

        ' 10. Build chimney 
        chimney(distance, Ix, Iy, Jx, Jy, uvpx, uvpy, Ex, Ey, Bx, By, _
        cornerx, cornery, rtcornerx, rtcornery, peak1x, peak1y, peak2x, peak2y, Dx, Ax, Fx)

        ' 11. Build vent pipe
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
        'do shade first
        Dim ppoint1x, ppoint1y, ppoint2x, ppoint2y As Single
        If Settings.Right = True Then
            intersection(pipex, pipey, 1000, pipey - 30, pipex - pl, pipey, pipex - pl, 0, ppoint1x, ppoint1y)
            intersection(pipex + pw, pipey - 1, 1000, pipey - 30, pipex + pw - pl, pipey - 1, pipex + pw - pl, 0, ppoint2x, ppoint2y)

            'joinfour(ppoint1x, ppoint1y, ppoint2x, ppoint2y - 1, pipex + pw, pipey, pipex, pipey - 1, "pipeshadow")
            joinfour(ppoint1x, ppoint1y, ppoint2x, ppoint2y - 1, pipex, pipey - 1, pipex + pw, pipey, "pipeshadow")

        End If
        g.FillRectangle(New SolidBrush(Color.Black), pipex, pipey - pl, pw, pl)

        ' 12.  front and peak eaves
        subject = "fronteave"
        joinfour(cornerx, cornery, maineavex, maineavey, rteavex, rteavey, rtcornerx, rtcornery, subject)
        subject = "peakeave"
        joinfour(cornerx, cornery, maineavex, maineavey, pkeavex, pkeavey, peak1x, peak1y, subject)
        subject = "peakeave"
        joinfour(pkeavex, pkeavey, peak1x, peak1y, lcornerx, lcornery, leavex, leavey, subject)

        '8a. do telephone poles just before deck (which sub is in the windows sub
        roadpts()



    End Sub

    Private Sub joinsix(ByVal Cx, ByVal Cy, ByVal LS1x, ByVal LS1y, ByVal LRx, ByVal LRy, _
    ByVal RS1x, ByVal RS1y, ByVal Bx, ByVal By, ByVal Ix, ByVal Iy, ByVal subject)
        Dim pt1 As New Point(Cx, Cy)
        Dim pt2 As New Point(LS1x, LS1y)
        Dim pt3 As New Point(LRx, LRy)
        Dim pt4 As New Point(RS1x, RS1y)
        Dim pt5 As New Point(Bx, By)
        Dim pt6 As New Point(Ix, Iy)
        Dim wall As Point() = {pt1, pt2, pt3, pt4, pt5, pt6}
        Dim spath As New Drawing2D.GraphicsPath
        spath.AddPolygon(wall)
        g.SetClip(spath)
        If subject = "pole" Then
            colorblend(spath.GetBounds.X, spath.GetBounds.Y, spath.GetBounds.Width, spath.GetBounds.Height, subject, 0)
        Else
            doflathousecolor(spath.GetBounds.X, spath.GetBounds.Y, spath.GetBounds.Width, spath.GetBounds.Height, 2, subject)
        End If
        g.ResetClip()
        If Settings.Outlines = True And subject <> "v_aboveshade" Then g.DrawPolygon(stainedglasspen, wall)
    End Sub
    Private Sub pole() 'from roadfill()

        Dim poleht, poletoplx, poletoply, poletoprx, poletopry, polebotlx, polebotly, polebotrx, polebotry, _
        polewidth, pw, pht, midbotx, midboty, midtopx, midtopy As Single
        Dim ni, rdi As Integer

        poleht = 35
        polewidth = 1
        rdi = rd - 1
        For ni = 0 To rdi
            pw = pixelheight(polewidth, dist(ni))
            pht = pixelheight(poleht, dist(ni))
            polebotlx = polebotx(ni)
            polebotly = poleboty(ni)
            polebotrx = polebotlx + pw
            polebotry = polebotly
            poletoply = polebotly - pht
            poletoplx = polebotlx + yrand(3) * randsign()
            poletoprx = poletoplx + pw * 0.5
            poletopry = poletoply
            midbotx = polebotlx + (polebotrx - polebotlx) / 2
            midboty = polebotly + pw / 10
            midtopx = poletoplx + (poletoprx - poletoplx) / 2
            midtopy = poletoply - pw / 10
            joinsix(polebotlx, polebotly, midbotx, midboty, polebotrx, polebotry, _
            poletoprx, poletopry, midtopx, midtopy, poletoplx, poletoply, "pole")

        Next ni
    End Sub
    Private Sub roadpts()
        Dim roadwidth, toroad, rw, pd, poledistance, tr As Integer
        poledistance = 30 'from house corner
        roadwidth = randh(22, 28)
        toroad = 9 'distance from poles to road
        pd = pixelheight(poledistance, distance)
        tr = pixelheight(toroad, distance)
        rw = pixelheight(roadwidth, distance)

        roadlx(rd) = Ax + pd + tr
        roadly(rd) = Ay
        roadrx(rd) = roadlx(rd) + rw
        roadry(rd) = Ay

        polebotx(rd) = roadlx(rd) - tr ' set up beginning coordinates of pole for that house
        ' polebotx(rd) = Ax
        poleboty(rd) = Ay
        dist(rd) = distance
        rd += 1


    End Sub
    Private Sub roadfill()
        Dim n As Integer
        For n = 0 To rd 'endhs
            Dim point0 As New Point(roadlx(0), roadly(0))
            Dim point1 As New Point(roadlx(1), roadly(1))
            Dim point2 As New Point(roadlx(2), roadly(2))
            Dim point3 As New Point(roadlx(3), roadly(3))
            Dim point4 As New Point(roadlx(4), roadly(4))
            Dim point5 As New Point(roadlx(5), roadly(5))
            If roadlx(5) = 0 Then point5 = point4
            Dim point6 As New Point(roadlx(6), roadly(6))
            If roadlx(6) = 0 Then point6 = point5
            Dim point7 As New Point(roadlx(7), roadly(7))
            If roadlx(7) = 0 Then point7 = point6
            Dim point8 As New Point(roadrx(7), roadry(7))
            If roadrx(7) = 0 Then point8 = point7
            Dim point9 As New Point(roadrx(6), roadry(6))
            If roadrx(6) = 0 Then point9 = point8
            Dim point10 As New Point(roadrx(5), roadry(5))
            If roadrx(5) = 0 Then point10 = point9
            Dim point11 As New Point(roadrx(4), roadry(4))
            Dim point12 As New Point(roadrx(3), roadry(3))
            Dim point13 As New Point(roadrx(2), roadry(2))
            Dim point14 As New Point(roadrx(1), roadry(1))
            Dim point15 As New Point(roadrx(0), roadry(0))

            Dim roadPoints As Point() = {point0, point1, point2, point3, point4, point5, point6, point7, _
            point8, point9, point10, point11, point12, point13, point14, point15}
            Dim roadpath As New Drawing2D.GraphicsPath
            roadpath.AddClosedCurve(roadPoints, 0.3F)
            g.SetClip(roadpath)
            colorblend(roadpath.GetBounds.X, roadpath.GetBounds.Y, roadpath.GetBounds.Width, _
            roadpath.GetBounds.Height, "road", 3)
            g.ResetClip()
            If Settings.Outlines = True Then g.DrawPath(stainedglasspen, roadpath)
        Next

    End Sub

    Private Sub footings()
        Dim adepthfootings, Adepthy, ddepthx, ddepthy, FDx, FFx As Single
        FDx = Dx + (Ax - Dx) * 0.015 'to give a small lip at top
        FFx = Fx - (Fx - Ax) * 0.015

        adepthfootings = 0.7
        Adepthy = Ay + pixelheight(adepthfootings, distance)

        intersection(Ax, Adepthy, LVP, horizon, FDx, Dy, FDx, 0, ddepthx, ddepthy) 'get depth under D
        intersection(Ax, Adepthy, RVP, horizon, FFx, Fy, FFx, 0, fdepthx, fdepthy) 'get depth under F
        joinfour(Ax, Ay, Ax, Adepthy, ddepthx, ddepthy, FDx, Dy, "leftfooting")
        joinfour(Ax, Ay, Ax, Adepthy, fdepthx, fdepthy, FFx, Fy, "rightfooting")


    End Sub
    Private Sub foundation()
        Dim ZDy, ZDAx, ZDAy, ZAFx, ZAFy, Ddepth, Fdepth, Adepth, Dmin, Amin, Fmin, widthd, widthd1, widthd2, widthf, _
               topDAx, topDAy, topAFx, topAFy, DistallowDA, DistallowAF, ADx, AFx, FDx, FFx, widthf1, widthf2 As Single

        Ddepth = (Dy - Cy) * 0.5 '   max depth limit at D corner - approx. half of house box height
        Dmin = Ddepth * 0.36
        Fdepth = (Fy - Ey) * 0.5 '    " at F corner
        Fmin = Fdepth * 0.36
        Adepth = (Ay - By) * 0.5 '    " at A corner
        Amin = Adepth * 0.36
        FDx = Dx + (Ax - Dx) * 0.015 'shift foundation under house a little
        FFx = Fx - (Fx - Ax) * 0.015
        widthd = Ax - FDx
        widthd1 = (FDx + widthd * 0.3)
        widthd2 = (Ax - widthd * 0.3)
        widthf = FFx - Ax
        widthf1 = Ax + widthf * 0.3
        widthf2 = FFx - widthf * 0.3

        ADx = randh(widthd1, widthd2) 'left side lateral point at which a ground deviation takes place
        AFx = randh(widthf1, widthf2) 'right side " "

        ZDy = Dy + randh(Dmin, Ddepth) 'depth under D
        zAy = Ay + randh(Amin, Adepth) ' depth under A
        zFy = Fy + randh(Fmin, Fdepth) ' depth under F

        intersection(ADx, Ay, ADx, 0, FDx, ZDy, Ax, zAy, ZDAx, ZDAy) ' get deviation start point, below DA
        intersection(AFx, Ay, AFx, 0, FFx, zFy, Ax, zAy, ZAFx, ZAFy) 'A to F
        intersection(ZDAx, ZDAy, ZDAx, 0, FDx, Dy, Ax, Ay, topDAx, topDAy) '
        intersection(ZAFx, ZAFy, ZAFx, 0, Ax, Ay, FFx, Fy, topAFx, topAFy)
        DistallowDA = (ZDAy - topAFy) * 0.2
        DistallowAF = (ZAFy - topAFy) * 0.2
        ZDAy = ZDAy + randsign() * randh(2, DistallowDA)
        ZAFy = ZAFy + randsign() * randh(2, DistallowAF)

        joinfive(FDx, Dy, Ax, Ay, Ax, zAy, ZDAx, ZDAy, FDx, ZDy, "leftfoundation")
        joinfive(Ax, Ay, FFx, Fy, FFx, zFy, ZAFx, ZAFy, Ax, zAy, "rightfoundation")

    End Sub
    Private Sub frontstep(ByVal lbotjx, ByVal lbotjy, ByVal rbotjx, ByVal rbotjy)
        Dim stepdepth, uppersteplx, uppersteprx, lupstx, lupsty, rupstx, rupsty, flupstx, flupsty, _
        llowstx, llowsty, rlowstx, rlowsty, frupstx, frupsty, fllowstx, fllowsty, frlowstx, frlowsty As Single
        stepdepth = (Ax - Dx) * 0.2
        uppersteplx = lbotjx - (lbotjx - Ax) * 0.2
        uppersteprx = rbotjx + (Fx - rbotjx) * 0.2
        intersection(uppersteplx, lbotjy, uppersteplx, 0, Ax, Ay, RVP, horizon, lupstx, lupsty) ' back left upper point of step
        intersection(lupstx, lupsty, lupstx, 0, Fx, fdepthy, RVP, horizon, llowstx, llowsty) 'back left lower step pt
        intersection(uppersteprx, rbotjy, uppersteprx, 0, Ax, Ay, RVP, horizon, rupstx, rupsty) 'back right upper pt
        intersection(llowstx, llowsty, RVP, horizon, rupstx, rupsty, rupstx, 0, rlowstx, rlowsty) 'back right lower pt
        intersection(lupstx, lupsty, LVP, horizon, lupstx + stepdepth, lupsty, lupstx + stepdepth, 0, flupstx, flupsty) 'front left upper pt
        intersection(llowstx, llowsty, LVP, horizon, flupstx, flupsty, flupstx, 0, fllowstx, fllowsty) 'front left lower  pt
        intersection(flupstx, flupsty, RVP, horizon, rupstx, rupsty, LVP, horizon, frupstx, frupsty) 'front right upper pt
        intersection(fllowstx, fllowsty, RVP, horizon, frupstx, frupsty, frupstx, 0, frlowstx, frlowsty) 'front right lower pt
        joinfour(lupstx, lupsty, rupstx, rupsty, frupstx, frupsty, flupstx, flupsty, "topfrontstep")

        joinfour(lupstx, lupsty, llowstx, llowsty, fllowstx, fllowsty, flupstx, flupsty, "ls_frontstep")

        joinfour(flupstx, flupsty, fllowstx, fllowsty, frlowstx, frlowsty, frupstx, frupsty, "rs_frontstep")
        Exit Sub
    End Sub
    Private Sub deck(ByVal lbotjx, ByVal lbotjy, ByVal rbotjx, ByVal rbotjy)
        Dim zlpx, zrpx, lpx, lpy, rpx, rpy, porchw, pfcx, pfcy, prcx, prcy, zpost1x, zpost1y, zpost2x, _
        zpost2y As Single
        If doorx - Ax > Fx - doorox Then '  on window side
            zlpx = Ax + (wtoplx - Ax) * 0.5 'left porch zx point
            zrpx = doorox + yrand(Fx - doorox) 'right porch zx point
        Else
            zlpx = Ax + yrand(doorx - Ax)
            zrpx = Fx - yrand(Fx - doorox)
        End If

        intersection(zlpx, Ay, zlpx, 0, Ax, Ay, Fx, Fy, lpx, lpy) 'get left porch xy point
        intersection(zrpx, Fy, zrpx, 0, Ax, Ay, Fx, Fy, rpx, rpy) 'get right porch xy point
        intersection(lpx, lpy, lpx, 0, Ax, zAy, Fx, zFy, zpost1x, zpost1y) 'temp pt for post
        intersection(rpx, rpy, rpx, 0, Ax, zAy, Fx, zFy, zpost2x, zpost2y) 'temp pts

        porchw = (Ax - Dx) * 0.37 'porch depth

        intersection(lpx, lpy, LVP, horizon, lpx + porchw, lpy, lpx + porchw, 0, pfcx, pfcy) 'get porch front corner xy
        intersection(pfcx, pfcy, RVP, horizon, rpx, rpy, LVP, horizon, prcx, prcy) 'get porch right corner xy

        'edge joists
        Dim edgedepth, ed, frontx, fronty, zlpy, zprcx, zprcy As Single
        edgedepth = 1
        ed = pixelheight(edgedepth, distance)
        frontx = pfcx
        fronty = pfcy + ed
        intersection(frontx, fronty, LVP, horizon, lpx, lpy, lpx, 0, zlpx, zlpy) 'get leftlower building attachment of deck
        intersection(frontx, fronty, RVP, horizon, prcx, prcy, prcx, 0, zprcx, zprcy) 'get lower right side

        'two front posts
        Dim postwidth, ph, pw, post1x, post1y, difx, post2x, post2y, rpost2ax, lpost1ax, rpost1ax, _
        lpost2x, lpost2y, lpost1botx, lpost1boty, post2boty, lpost2botx, lpost2boty, post2botx, diffx, _
        diffy, ph2 As Single

        postwidth = 0.5 'six inches
        ph = zpost1y - lpy ' postheight

        pw = pixelheight(postwidth, distance)
        rpost1ax = frontx
        lpost1ax = post1x - pw
        lpost1botx = post1x
        lpost1boty = post1y + ph
        post2x = zprcx : post2y = zprcy 'edge of porch
        rpost2ax = post2x
        intersection(frontx - pw, fronty, RVP, horizon, prcx, prcy, 0, rpy, diffx, diffy) 'thickness of post at right
        difx = (prcx - diffx)
        ph2 = zpost2y - rpy
        post2boty = post2y + ph2
        intersection(post2botx, post2boty, RVP, horizon, lpost2x, lpost2y, lpost2x, 0, lpost2botx, lpost2boty) 'first left

        'railings (siding)
        Dim railht, rh, railbotfx, railbotfy, railbotrx, railbotry, rwallrailbotx, _
        rwallrailboty, rwallrailtopx, rwallrailtopy, frontcorntopx, frontcorntopy, rcornx, rcorny As Single
        railht = 3.5
        rh = pixelheight(railht, distance)
        intersection(lpx, lpy, LVP, horizon, pfcx, pfcy, pfcx, 0, railbotfx, railbotfy) 'get front bottom of rail
        intersection(railbotfx, railbotfy, RVP, horizon, prcx, prcy, prcx, 0, railbotrx, railbotry) 'get right bot rail
        intersection(railbotrx, railbotry, LVP, horizon, rpx, rpy, rpx, 0, rwallrailbotx, rwallrailboty) 'get right wall lower attachment
        intersection(lpx, lpy - rh, RVP, horizon, rpx, rpy, rpx, 0, rwallrailtopx, rwallrailtopy) 'get right top rail
        intersection(lpx, lpy - rh, LVP, horizon, pfcx, pfcy, pfcx, 0, frontcorntopx, frontcorntopy) 'get topfrontcorner rail
        intersection(frontcorntopx, frontcorntopy, RVP, horizon, prcx, prcy, prcx, 0, rcornx, rcorny) 'get toprightcorner rail

        'g.FillEllipse(Brushes.Red, diffx, diffy, 3, 3)


        'Assembling the deck
        '1. the deck itself - or porch - all p's represent "porch"
        joinfour(lpx, lpy, pfcx, pfcy, prcx, prcy, rpx, rpy, "deck")
        If rowhousetype <> "withlowdeck" Then 'place posts
            If pw < 1 Then pw = 1
            colorblend(pfcx - pw, pfcy, pw, ph, "post", 0)
            colorblend(prcx - 3, diffy, 3, ph2, "post", 0)
            If Settings.Outlines = True Then
                g.DrawRectangle(Pens.Black, pfcx - pw, pfcy, pw, ph)
                g.DrawRectangle(Pens.Black, prcx - 3, diffy, 3, ph2)
            End If
        End If
        '2. Back rail,front rail,left rail
        joinfour(rwallrailbotx, rwallrailboty, rwallrailtopx, rwallrailtopy, rcornx, rcorny, railbotrx, railbotry, "deckbackside")
        g.DrawLine(Pens.Brown, rwallrailtopx, rwallrailtopy, rcornx, rcorny) 'put top on rail
        joinfour(lpx, lpy, pfcx, pfcy, prcx, prcy, rpx, rpy, "deckrightside")
        joinfour(frontcorntopx, frontcorntopy, railbotfx, railbotfy, railbotrx, railbotry, rcornx, rcorny, "deckfrontside")
        g.DrawLine(Pens.Brown, rcornx, rcorny, frontcorntopx, frontcorntopy)
        g.DrawLine(Pens.Brown, frontcorntopx, frontcorntopy, lpx, lpy - rh)

        'add joists at the end
        joinfour(frontx, fronty, pfcx, pfcy, prcx, prcy, zprcx, zprcy, "deckfrontjoist") 'add joist on front
        joinfour(frontx, fronty, pfcx, pfcy, lpx, lpy, zlpx, zlpy, "deckleftjoist") 'add joist to left
        joinfour(lpx, lpy, lpx, lpy - rh, frontcorntopx, frontcorntopy, railbotfx, railbotfy, "deckleftside")

    End Sub


    Private Sub joinfour(ByVal x0, ByVal y0, ByVal x1, ByVal y1, ByVal x2, ByVal y2, ByVal x3, ByVal y3, ByVal subject)
        Dim pt0 As New Point(x0, y0)
        Dim pt1 As New Point(x1, y1)
        Dim pt2 As New Point(x2, y2)
        Dim pt3 As New Point(x3, y3 - 1)
        Dim quart As Point() = {pt0, pt1, pt2, pt3}
        Dim fourpath As New Drawing2D.GraphicsPath
        fourpath.AddPolygon(quart)
        g.SetClip(fourpath)
        doflathousecolor(fourpath.GetBounds.X, fourpath.GetBounds.Y, fourpath.GetBounds.Width, fourpath.GetBounds.Height, 2, subject)
        g.ResetClip()
        If Settings.Outlines = True And subject <> "leftshade" And subject <> "chimneyshadow" And subject <> "pipeshadow" Then
            g.DrawPolygon(stainedglasspen, quart)
        End If
    End Sub
    Private Sub buildbox(ByVal dd, ByRef Cx, ByRef Cy, ByRef Dx, ByRef Dy)

        Dim x0, y0, x1, y1, x2, y2, x3, y3, x4, y4, M12, M34, B12, B34 As Single
        ' 
        'line1
        If Settings.side = 1 Then
            x1 = RVPM : y1 = horizon
            x2 = Ax - dd : y2 = Ay
        Else
            x1 = LVPM : y1 = horizon
            x2 = Ax + dd : y2 = Ay
        End If

        'line2, to be intersected with line1
        If Settings.side = 1 Then
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

        Dim spath As New Drawing2D.GraphicsPath
        spath.AddPolygon(wall)
        g.SetClip(spath)
        doflathousecolor(spath.GetBounds.X, spath.GetBounds.Y, spath.GetBounds.Width, spath.GetBounds.Height, 2, subject)
        g.ResetClip()
        If Settings.Outlines = True Then g.DrawPolygon(stainedglasspen, wall)


    End Sub
    Private Sub joinfive(ByVal ax, ByVal ay, ByVal bx, ByVal by, ByVal cx, ByVal cy, ByVal dx, ByVal dy, ByVal ex, ByVal ey, ByVal subject)
        Dim pt1 As New Point(ax, ay)
        Dim pt2 As New Point(bx, by)
        Dim pt3 As New Point(cx, cy)
        Dim pt4 As New Point(dx, dy)
        Dim pt5 As New Point(ex, ey)
        Dim wall As Point() = {pt1, pt2, pt3, pt4, pt5}
        Dim spath As New Drawing2D.GraphicsPath
        spath.AddPolygon(wall)
        g.SetClip(spath)
        doflathousecolor(spath.GetBounds.X, spath.GetBounds.Y, spath.GetBounds.Width, spath.GetBounds.Height, 2, subject)
        g.ResetClip()
        If Settings.Outlines = True And subject <> "frontshade" Then g.DrawPolygon(stainedglasspen, wall)
    End Sub

    Private Sub buildpeakside()
        Dim pt1 As New Point(Ax, Ay)
        Dim pt2 As New Point(Bx, By)
        Dim pt3 As New Point(Ix, Iy)
        Dim pt4 As New Point(Cx, Cy)
        Dim pt5 As New Point(Dx, Dy)
        Dim wall As Point() = {pt1, pt2, pt3, pt4, pt5}
        Dim spath As New Drawing2D.GraphicsPath
        spath.AddPolygon(wall)
        g.SetClip(spath)
        doflathousecolor(spath.GetBounds.X, spath.GetBounds.Y, spath.GetBounds.Width, spath.GetBounds.Height, 2, "peakside")
        g.ResetClip()
        If Settings.Outlines = True Then g.DrawPolygon(stainedglasspen, wall)

        'lapboards
        Dim lapx, lapy, w, wid, asfaras, asfarasx, asfarasy As Single
        w = 0.9 'lapboard width about 11"
        wid = pixelheight(w, distance)
        If wid < 2.5 Then Exit Sub
        lapy = Ay
        Dim lappen As New Pen(Color.Black)
        If FormSettings.scheme < 10 Then
            lappen.Color = modColors.getcolor(wallsltcolor, 2, wallsdktone)
        Else
            lappen.Color = ModCMBcolors.getCMBcolor(cmbseason, wallsdktone, wallsdkcolor)
        End If

        Do While lapy > By
            lapx = Ax
            lapy = lapy - wid
            asfaras = (Ax - Dx) * yrand(15) * 0.01
            intersection(lapx, lapy, LVP, horizon, Ax - asfaras, lapy, Ax - asfaras, 0, asfarasx, asfarasy) ' as far as this point
            g.DrawLine(lappen, lapx, lapy, asfarasx, asfarasy)
        Loop

    End Sub
    Private Sub buildnonpeakside()
        Dim pt1 As New Point(Ax, Ay)
        Dim pt2 As New Point(Bx, By)
        Dim pt3 As New Point(Ex, Ey)
        Dim pt4 As New Point(Fx, Fy)
        Dim wall As Point() = {pt1, pt2, pt3, pt4}
        Dim brush As New SolidBrush(Color.Empty)
        Dim spath As New Drawing2D.GraphicsPath
        spath.AddPolygon(wall)
        g.SetClip(spath)
        Dim subject As String = "nonpeakside"
        doflathousecolor(spath.GetBounds.X, spath.GetBounds.Y, spath.GetBounds.Width, spath.GetBounds.Height, 2, subject)
        g.ResetClip()
        If Settings.Outlines = True Then g.DrawPolygon(stainedglasspen, wall)

        'lapboards
        Dim lapx, lapy, w, wid, asfaras, asfarasx, asfarasy As Single
        w = 0.9 'lapboard width about 11"
        wid = pixelheight(w, distance)
        If wid < 2.5 Then Exit Sub
        lapy = Ay
        Dim lappen As New Pen(Color.Black)
        If FormSettings.scheme < 10 Then
            lappen.Color = modColors.getcolor(wallsltcolor, 2, wallsdktone)
        Else
            lappen.Color = ModCMBcolors.getCMBcolor(cmbseason, wallsdktone, wallsdkcolor)
        End If

        Do While lapy > By
            lapx = Ax
            lapy = lapy - wid
            asfaras = (Fx - Ax) * yrand(15) * 0.01
            intersection(lapx, lapy, RVP, horizon, Ax + asfaras, lapy, Ax + asfaras, 0, asfarasx, asfarasy) ' as far as this point
            g.DrawLine(lappen, lapx, lapy, asfarasx, asfarasy)
        Loop

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
    Private Sub basicdoor_windows(ByVal dd, ByVal ff, ByVal distance, ByVal frontwidth, ByVal sidewidth)
        Dim doorstart, doory, s, L, ds, dh, dw, toprtx, toprty, doorsill, dooroy As Single
        Dim doorht, doorwidth As Integer
        Dim subject As String = "222"

        s = ff * 0.2 : L = ff * 0.7
        doorstart = randh(s, L)
        doorwidth = 4
        dw = pixelheight(doorwidth, distance)
        intersection(LVPM, horizon, Ax + doorstart, Ay, Ax, Ay, Fx, Fy, doorx, doory) ' get door start point
        intersection(LVPM, horizon, Ax + doorstart + dw, Ay, Ax, Ay, Fx, Fy, doorox, dooroy) ' get opp. side of door
        doorht = 7
        dh = pixelheight(doorht, distance)
        doorsill = 0.08
        ds = pixelheight(doorsill, distance)
        intersection(RVP, horizon, doorx, doory - dh, doorox, dooroy, doorox, 0, toprtx, toprty) 'get top right point of door
        subject = "door"
        joinfour(doorx, doory - ds, doorx, doory - dh, toprtx, toprty, doorox, dooroy - ds, subject)

        'DOOR JAMB
        Dim jamwidth, jwid, topjwx, topjwy, rbotjx, rbotjy, lbotjx, lbotjy, rtopjx, rtopjy As Single
        jamwidth = 0.1
        jwid = pixelheight(jamwidth, distance)
        topjwx = toprtx - jwid  'starters
        topjwy = toprty
        intersection(LVP, horizon, doorox, dooroy - ds, topjwx, topjwy, toprtx, 0, rbotjx, rbotjy) 'get bottom right jamb point 
        intersection(RVP, horizon, rbotjx, rbotjy, doorx, doory - ds, doorx, 0, lbotjx, lbotjy) 'get bottom left point
        intersection(RVP, horizon, toprtx, toprty, rbotjx, rbotjy, rbotjx, 0, rtopjx, rtopjy) 'get top right point
        subject = "doorjamb"
        joinfour(rtopjx, rtopjy, toprtx, toprty, doorox, dooroy - ds, rbotjx, rbotjy, subject)
        subject = "doorjamb2"
        joinfour(doorox, dooroy - ds, rbotjx, rbotjy, lbotjx, lbotjy, doorx, doory - ds, subject)

        'DOOR HANDLE
        Dim handlex, handley, inset, it, handsize, hz As Single
        inset = 0.8
        handsize = 0.25
        hz = pixelheight(handsize, distance)
        it = pixelheight(inset, distance)
        handley = dooroy - ds - (((dooroy - ds) - toprty) * 0.5)
        handlex = doorox - it
        Dim pen As New Pen(Color.Black)
        g.DrawEllipse(pen, handlex, handley, hz, hz)

        'FRONT WINDOW
        'get widest side of door
        If doorx - Ax > Fx - doorox Then
            Dim closd, cl, wtopry, wtoply, wwx, wwy, vvx, vvy, mdlx, mdly, mdrx, mdry, curtainulx, curtainuly, _
            curtainurx, curtainury, curtainllx, curtainlly, curtainlrx, curtainlry, curtainl, curtainr As Single
            'Choose 4 feet from door to start
            closd = 4
            cl = pixelheight(closd, distance)
            intersection(LVPM, horizon, Ax + doorstart - cl, Ay, Ax, Ay, Fx, Fy, wwx, wwy) 'get projection
            intersection(RVP, horizon, doorx, doory - dh, wwx, wwy, wwx, 0, wtoprx, wtopry) 'top right point
            intersection(LVPM, horizon, Ax + cl, Ay, Ax, Ay, Fx, Fy, vvx, vvy) 'get lower left proj.
            intersection(RVP, horizon, doorx, doory - dh, vvx, vvy, vvx, 0, wtoplx, wtoply) 'get top left point
            intersection(RVP, horizon, doorx, doory - dh / 3, vvx, vvy, vvx, 0, mdlx, mdly) 'get lower left point
            intersection(RVP, horizon, mdlx, mdly, wwx, wwy, wwx, 0, mdrx, mdry) 'get lower right point
            subject = "window"
            joinfour(wtoprx, wtopry, wtoplx, wtoply, mdlx, mdly, mdrx, mdry, subject)


            'FRONT WINDOW CURTAIN - or black hole between open curtains
            Dim cw As Single = randh(15, 40) * 0.01

            curtainl = (wtoprx - wtoplx) * cw
            curtainr = (wtoprx - wtoplx) * (1 - cw)
            intersection(wtoplx, wtoply, wtoprx, wtopry, wtoplx + curtainl, wtoply, wtoplx + curtainl, 0, _
            curtainulx, curtainuly) 'top left curtian

            'g.FillEllipse(Brushes.Red, wtoplx + curtainl, wtopry, 3, 3)
            'Exit Sub

            intersection(wtoplx, wtoply, wtoprx, wtopry, wtoplx + curtainr, wtoply, wtoplx + curtainr, 0, _
            curtainurx, curtainury) 'top rtside curtain
            intersection(mdlx, mdly, mdrx, mdry, curtainulx, curtainuly, curtainulx, 0, curtainllx, curtainlly)
            intersection(mdlx, mdly, mdrx, mdry, curtainurx, curtainury, curtainurx, 0, curtainlrx, curtainlry)
            subject = "curtain"
            joinfour(curtainulx, curtainuly, curtainurx, curtainury, curtainlrx, curtainlry, curtainllx, curtainlly, subject)

            'FRONT WINDOW CASING (OUTSIDE)
            Dim sillwidth, sill, topsx, topsy, botrx, botry, botlx, botly, toprx, topry As Single
            sillwidth = 0.3
            sill = pixelheight(sillwidth, distance)
            topsx = wtoprx - sill   'starters
            topsy = wtopry    '          "
            intersection(LVP, horizon, mdrx, mdry, topsx, topsy, topsx, 0, botrx, botry) 'get bottom right point
            intersection(RVP, horizon, mdlx, mdly, wtoplx, wtoply, wtoplx, 0, botlx, botly) 'get bottom left point
            intersection(botrx, botry, botrx, 0, wtoprx, wtopry, wtoplx, wtoply, toprx, topry)      'get top right point
            subject = "rtsidefrontwindow"
            joinfour(wtoprx, wtopry, toprx, topry, botrx, botry, mdrx, mdry, subject) 'paint right side
            subject = "windowbottom"
            joinfour(mdlx, mdly, botlx, botly, botrx, botry, mdrx, mdry, subject)    'paint bottom

            'CROSS PIECES IN FRONT WINDOW
            Dim midx, midy, transomht, tr, lbarx, lbary, rbarx, rbary, centx, centy, botcentx, botcenty As Single
            transomht = 2
            tr = pixelheight(transomht, distance)
            lbarx = botlx
            lbary = botly - tr
            intersection(RVP, horizon, lbarx, lbary, botrx, botry, botrx, 0, rbarx, rbary) 'get right bar point
            intersection(wtoplx - sill, wtoply, botrx, botry, toprx, topry, botlx - sill, botly, midx, midy) 'center point of window
            intersection(midx, midy, midx, 0, lbarx, lbary, rbarx, rbary, centx, centy) 'center bar point
            intersection(midx, midy, midx, 0, botlx, botly, botrx, botry, botcentx, botcenty) 'bottom center point
            g.DrawLine(Pens.LightGray, toprx, topry, botrx, botry)
            g.DrawLine(Pens.LightGray, botrx, botry, botlx, botly)
            g.DrawLine(Pens.LightGray, lbarx, lbary, rbarx, rbary) 'crossbar
            g.DrawLine(Pens.LightGray, centx, centy, botcentx, botcenty) 'vertical bar
        End If
        If rowhousetype = "withlowdeck" Or rowhousetype = "single" Then
            deck(lbotjx, lbotjy, rbotjx, rbotjy)
        ElseIf rowhousetype = "withdoorstep" Then
            frontstep(lbotjx, lbotjy, rbotjx, rbotjy)
        End If
        found = 0
        'BACK WINDOW
        Dim bh, wh, ww, LLx, LLy, WLLx, WLLy, WULx, WULy, Wrx, Wry, WURx, WURy, WLRx, WLRy As Single
        bh = pixelheight(3.0, distance) 'beginning height
        ww = pixelheight(3.2, distance) 'width
        wh = pixelheight(5.0, distance) 'height
        intersection(RVPM, horizon, Ax - 0.7 * dd, Ay, Ax, Ay, Dx, Dy, LLx, LLy) 'get helper point for left side of window
        intersection(LVP, horizon, Ax, Ay - bh, LLx, LLy, LLx, 0, WLLx, WLLy) 'get lower left windowpoint
        intersection(LVP, horizon, WLLx, WLLy - wh, WLLx, WLLy, WLLx, 0, WULx, WULy) ' get upper left windowpoint
        intersection(RVPM, horizon, Ax - 0.7 * dd + ww, Ay, Ax, Ay, Dx, Dy, Wrx, Wry) 'get helper point for right side of window
        intersection(LVP, horizon, WULx, WULy, Wrx, Wry, Wrx, 0, WURx, WURy) 'get upper right point 
        intersection(LVP, horizon, WLLx, WLLy, WURx, WURy, WURx, 0, WLRx, WLRy) 'get lower right point
        subject = "backwindow"
        joinfour(WLLx, WLLy, WLRx, WLRy, WURx, WURy, WULx, WULy, subject)

        'BACK WINDOW CASING
        Dim sULx, sULy, sLLx, sLLy, sLRx, sLRy, sillwid, sil As Single
        sillwid = 0.3
        sil = pixelheight(sillwid, distance)
        intersection(RVP, horizon, WLLx, WLLy, WULx + sil, WULy, WULx + sil, 0, sLLx, sLLy) 'get lowerleft sill point
        intersection(LVP, horizon, WULx, WULy, sLLx, sLLy, sLLx, 0, sULx, sULy) 'get upperleft point
        intersection(LVP, horizon, sLLx, sLLy, WLRx, WLRy, WLRx, 0, sLRx, sLRy) 'get lowerright point
        subject = "bkwindowcasing"
        joinfour(WULx, WULy, sULx, sULy, sLLx, sLLy, WLLx, WLLy, subject)
        subject = "bkwindowcasing2"
        joinfour(WLLx, WLLy, sLLx, sLLy, sLRx, sLRy, WLRx, WLRy, subject)

        'BACK WINDOW CROSS BARS
        Dim ctrx, ctry, UMpx, UMpy, BMpx, BMpy, LMpx, LMpy, RMpx, RMpy As Single
        intersection(sLLx, sLLy, WURx + sil, WURy, WLRx, WLRy, sULx, sULy, ctrx, ctry) 'get center of window
        intersection(LVP, horizon, WURx, WURy, ctrx, ctry, ctrx, 0, UMpx, UMpy) 'get upper middle point
        intersection(LVP, horizon, sLLx, sLLy, UMpx, UMpy, ctrx, ctry, BMpx, BMpy) 'get bottom middle point
        intersection(LVP, horizon, ctrx, ctry, sLLx, sLLy, sULx, sULy, LMpx, LMpy) 'get left middle point
        intersection(LVP, horizon, ctrx, ctry, WLRx, WLRy, WURx, WURy, RMpx, RMpy) 'get right middle point
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
    Private Sub chimney(ByVal distance, ByVal Ix, ByVal Iy, ByVal Jx, ByVal Jy, _
    ByVal uvpx, ByVal uvpy, ByVal Ex, ByVal Ey, ByVal Bx, ByVal By, _
    ByVal cornerx, ByVal cornery, ByVal rtcornerx, ByVal rtcornery, ByVal peak1x, ByVal peak1y, ByVal peak2x, ByVal peak2y, ByVal dx, ByVal ax, ByVal fx)
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

        bgptx = Ix + (Jx - Ix) * 0.6 '+ randsign() * ((Jx - Ix) / 4)
        bgpty = (Iy - Jy) / 2

        'FIND BEGINNING Y POINT
        intersection(RVP, horizon, Ix, Iy, bgptx, bgpty, bgptx, 0, roofx, roofy) 'get roof point

        If Bx < bgptx Then  'IF USING FRONT ROOF EDGE
            intersection(RVP, horizon, Bx, By, bgptx, bgpty, bgptx, 0, edgex, edgey) 'get edge point
        Else
            intersection(uvpx, uvpy, Bx, By, bgptx, bgpty, bgptx, 0, edgex, edgey)
        End If
        botx = roofx
        boty = edgey - (edgey - roofy) * 0.6
        If boty > edgey Then boty = edgey
        topx = roofx
        topy = boty - chy

        Dim uppershptlx, uppershptly, uppershptrx, upprx, uppry, uppershptry As Single
        intersection(botx - cwx, boty, botx - cwx, 0, uvpx, uvpy, botx, boty, botlx, botly) 'bottom left point
        intersection(LVP, horizon, topx, topy, botlx, botly, botlx, 0, toplx, toply) 'top left point
        intersection(botx + clx, boty, botx + clx, 0, RVP, horizon, botx, boty, botrx, botry) 'get bottom right point   
        intersection(uvpx, uvpy, Bx, By, botrx, botry, botrx, 0, othx, othy) 'other bottom right point
        If botry > othy Then botry = othy
        intersection(RVP, horizon, topx, topy, botrx, botry, botrx, 0, toprx, topry) 'get top right point
        intersection(LVP, horizon, toprx, topry, RVP, horizon, toplx, toply, backptx, backpty) 'get back point of chim.
        If topry > othy Then topry = othy
        'Shade
        If Settings.Right = True Then 'if sun from the right
            'build right roof and clip it to prevent shadows going off roof
            Dim pt0 As New Point(peak2x, peak2y) : Dim pt1 As New Point(rtcornerx, rtcornery)
            Dim pt2 As New Point(cornerx, cornery) : Dim pt3 As New Point(peak1x, peak1y)
            Dim quart As Point() = {pt0, pt1, pt2, pt3}
            Dim roofpath As New Drawing2D.GraphicsPath
            roofpath.AddPolygon(quart)
            intersection(botlx, botly, RVP, horizon, botrx, botry, uvpx, uvpy, upprx, uppry) 'back bottom corner
            Dim chgt As Integer = (boty - topy) * 0.75
            intersection(botx, boty, 1000, boty - 30, botx - chgt, boty, botx - chgt, 0, uppershptlx, uppershptly)
            intersection(botlx, botly, 1000, botly - 30, botlx - chgt, botry, botrx - chgt, 0, uppershptrx, uppershptry)
            g.SetClip(roofpath)

            subject = "chimneyshadow"
            joinfour(botlx, botly, botx, boty, uppershptlx, uppershptly, uppershptrx, uppershptry, subject)
            joinfour(upprx, uppry, botrx, botry, uppershptlx, uppershptly, uppershptrx, uppershptry, subject)
            g.ResetClip()
        End If

        'now the chimney itself
        subject = "chimneyhole"
        joinfour(toplx, toply, topx, topy, toprx, topry, backptx, backpty, subject) ' top hole
        subject = "chimneyrtside"
        joinfour(toprx, topry, topx, topy, botx, boty, botrx, botry, subject)    'right side
        subject = "chimneyleftside"
        joinfour(toplx, toply, topx, topy, botx, boty, botlx, botly, subject)    'left side

        'chimney bricks

        Dim brickx, bricky, w, widt, asfaras, asfarasx, asfarasy As Single
        w = 0.5 'brickboard width about 11"
        widt = pixelheight(w, distance)
        If widt < 1.5 Then Exit Sub
        bricky = boty - widt
        Dim brickpen As New Pen(Color.White)
        If Settings.left = True Then brickpen.Color = Color.LightGray
        Do
            brickx = botx
            bricky = bricky - widt
            If bricky < (topy + 3) Then Exit Do
            asfaras = (toprx - topx) * yrand(15) * 0.05
            intersection(brickx, bricky, RVP, horizon, brickx + asfaras, bricky, brickx + asfaras, 0, asfarasx, asfarasy) ' as far as this point
            g.DrawLine(brickpen, brickx, bricky, asfarasx, asfarasy)
            g.DrawLine(brickpen, asfarasx, asfarasy, asfarasx, bricky + widt)
        Loop
        If Settings.Outlines = True Then g.DrawLine(Pens.Black, botx, boty, topx, topy)


    End Sub

    Public Sub New()

    End Sub
End Class
