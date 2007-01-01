
Public Class FormSettings
    '    Private Sub btnGOPAINT_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

    '    End Sub
    'Private Sub clbFOREGROUND_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)

    'End Sub

    Private Sub lbSKY_SelectedIndexChanged_1(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Private Sub lbFOREGROUND_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub
    Private Sub lbOPT_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Private Sub ListBox4_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Private Sub ListBox3_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub
    '


    Public rand, comp, dominantcolor, colours(6), canvaswidth, canvasheight As Integer
    Public colour, sky, background, midground, foreground, scheme, CMBcolors(,,), mycolors(,,), temperature, compu, items, options, sunspot, colorscheme, season, _
    intensity, glaze, tonalcomp, design, colortest, key, lines, shapes, summary1, summary2 As String
    Public ButtonIndex As Integer
    Private SelectedColors(0 To 18) As Boolean

    '    Private ObjectSeed As Integer
    Private Settings As TSettings = New TSettings

    Private Sub obtainrequests()
        Dim comp1 As String
        Dim i As Integer

        comp1 = Mid(lbCOMPUTER.SelectedItem, 1, 1)
        If comp1 = "" Then comp1 = "2"
        compu = CInt(comp1)

        If dominantcolor = 0 Or compu = 1 Then ' this is probably not the proper way to do this... ;-)
            ButtonIndex = Rnd() * 17
            IncludeColor(ButtonIndex)
            UpdateSelectedColors()
            Panel1.Refresh()
        End If

        Settings.DominantColor = dominantcolor
        Settings.Scheme = scheme
        For i = 0 To 5
            Settings.Colours(i) = colours(i)
        Next


        If lbCOLORSCHEME.SelectedItems.Count = 0 Or compu = 1 Then
            lbCOLORSCHEME.SetSelected(Rnd() * (lbCOLORSCHEME.Items.Count - 1), True)
        End If


        Dim strValue As String = ""

        'background 
        Settings.BigSky = False
        Settings.Mountains = False
        Settings.Sea = False
        Settings.Hills = False
        Settings.Indistinct = False
        If (Me.lbBACKGROUND.SelectedIndex = -1 And compu = 2) Or compu = 1 Then
            Me.lbBACKGROUND.SelectedItems.Clear()
            rand = Rnd() * 4 : lbBACKGROUND.SelectedIndex = rand
        End If

        For Each strValue In lbBACKGROUND.SelectedItems
            If strValue = "Big Sky" Then Settings.BigSky = True
            If strValue = "Mountains" Then Settings.Mountains = True
            If strValue = "Sea" Then Settings.Sea = True
            If strValue = "Hills" Then Settings.Hills = True
            If strValue = "Indistinct" Then Settings.Indistinct = True
        Next
     
        ' midground 
        Settings.Mountainous = False
        Settings.RollingHills = False
        Settings.Prairie = False
        Settings.Lake = False
        Settings.River = False
        Settings.LocalArea = False
        Settings.Hazy = False
        If (Me.lbMIDGROUND.SelectedIndex = -1 And compu = 2) Or compu = 1 Then
            Me.lbMIDGROUND.SelectedItems.Clear()
            rand = Rnd() * 6 : lbMIDGROUND.SelectedIndex = rand
        End If
        For Each strValue In lbMIDGROUND.SelectedItems
            If strValue = "Mountainous" Then Settings.Mountainous = True
            If strValue = "Rolling Hills" Then Settings.RollingHills = True
            If strValue = "Prairie" Then Settings.Prairie = True
            If strValue = "Lake" Then Settings.Lake = True
            If strValue = "River" Then Settings.River = True
            If strValue = "Local Area" Then Settings.LocalArea = True
            If strValue = "Hazy" Then Settings.Hazy = True
        Next

        'foreground
        Settings.Road = False
        Settings.Waterfall = False
        Settings.Buildings = False
        Settings.Flowers = False
        Settings.Farmstuff = False
        Settings.Nil = False
        If (Me.lbFOREGROUND.SelectedIndex = -1 And compu = 2) Or compu = 1 Then
            Me.lbFOREGROUND.SelectedItems.Clear()
            rand = Rnd() * 5 : lbFOREGROUND.SelectedIndex = rand
        End If
        For Each strValue In lbFOREGROUND.SelectedItems
            If strValue = "Road" Then Settings.Road = True
            If strValue = "Waterfall" Then Settings.Waterfall = True
            If strValue = "Building(s)" Then Settings.Buildings = True
            If strValue = "Flowers" Then Settings.Flowers = True
            If strValue = "Farmstuff" Then Settings.Farmstuff = True
            If strValue = "Nil" Then Settings.Nil = True
        Next

        'sky
        Settings.Clear = False
        Settings.Clouds = False
        Settings.Overcast = False
        If (Me.lbSKY.SelectedIndex = -1 And compu = 2) Or compu = 1 Then
            Me.lbSKY.SelectedItems.Clear()
            rand = Rnd() * 2 : lbSKY.SelectedIndex = rand
        End If
        For Each strValue In lbSKY.SelectedItems
            If strValue = "Clear" Then Settings.Clear = True
            If strValue = "Clouds" Then Settings.Clouds = True
            If strValue = "Overcase" Then Settings.Overcast = True
        Next

        'season
        Settings.Spring = False
        Settings.Summer = False
        Settings.Fall = False
        Settings.Winter = False
        If (Me.lbSEASON.SelectedIndex = -1 And compu = 2) Or compu = 1 Then
            Me.lbSEASON.SelectedItems.Clear()
            rand = Rnd() * 3 : lbSEASON.SelectedIndex = rand
        End If
        For Each strValue In lbSEASON.SelectedItems
            If strValue = "Spring" Then Settings.Spring = True
            If strValue = "Summer" Then Settings.Summer = True
            If strValue = "Fall" Then Settings.Fall = True
            If strValue = "Winter" Then Settings.Winter = True
        Next


        'Options
        Settings.Fog = False
        Settings.Sunset = False
        Settings.Outlines = False
        Settings.Neither = False
        For Each strValue In lbOPT.SelectedItems
            If strValue = "Outlines" Then Settings.Outlines = True
            If strValue = "Fog" Then Settings.Fog = True
            If strValue = "Neither" Then Settings.Neither = True
        Next


        'colortest 
        Settings.Cmb = False
        Settings.Other = False
        Settings.Ntr = False
        For Each strValue In lbCOLORTEST.SelectedItems
            If strValue = "Cmb" Then Settings.Cmb = True
            If strValue = "Other" Then Settings.Other = True
            If strValue = "Ntr" Then Settings.Ntr = True
        Next

        'Sun
        Settings.left = False
        Settings.Central = False
        Settings.Right = False
        Settings.Ambient = False
        Settings.Evening = False
        If (Me.lbSUN.SelectedIndex = -1 And compu = 2) Or compu = 1 Then
            Me.lbSUN.SelectedItems.Clear()
            rand = Rnd() * 3 : lbSUN.SelectedIndex = rand
        End If
        For Each strValue In lbSUN.SelectedItems
            If strValue = "Left" Then Settings.left = True
            If strValue = "Central" Then Settings.Central = True
            If strValue = "Right" Then Settings.Right = True
            If strValue = "Ambient" Then Settings.Ambient = True
            If strValue = "Evening" Then Settings.Evening = True
        Next


        key = ""
        key = lbKEY.SelectedItem
        If (key = "" And compu = 2) Or compu = 1 Then
            rand = Rnd() * 3 : lbKEY.SelectedIndex = rand
            key = lbKEY.SelectedItem
        End If

        'intensity
        Settings.FullRange = False
        Settings.High = False
        Settings.Medium = False
        Settings.Low = False
        If (Me.lbINTENSITY.SelectedIndex = -1 And compu = 2) Or compu = 1 Then
            Me.lbINTENSITY.SelectedItems.Clear()
            rand = Rnd() * 3 : lbINTENSITY.SelectedIndex = rand
        End If
        For Each strValue In lbINTENSITY.SelectedItems
            If strValue = "FullRange" Then Settings.FullRange = True
            If strValue = "High" Then Settings.High = True
            If strValue = "Medium" Then Settings.Medium = True
            If strValue = "Low" Then Settings.Low = True

        Next

        tonalcomp = ""
        tonalcomp = lb3TONALCOMP.SelectedItem
        If (tonalcomp = "" And compu = 2) Or compu = 1 Then
            rand = Rnd() * 5 : lb3TONALCOMP.SelectedIndex = rand
            tonalcomp = lb3TONALCOMP.SelectedItem
        End If

        'determine screen size of painting form
        Dim ratio As Single
        Dim height As Integer = 0
        Dim extra As Integer = 32
        Dim width As Integer
        height = nudHEIGHT.Value
        If height = 0 Then height = 5
        width = nudWIDTH.Value
        If width = 0 Then width = 8
        ratio = width / height

        'frm.Height = 489   '5 inches (5*87=435) plus upper menu section which is 32 pixels
        'frm.Width = 522     '6 inches (6*87)
        Select Case ratio
            Case 1.0
                Settings.CanvasHeight = 500
            Case Is > 1
                Settings.CanvasHeight = 460
            Case Is > 1.3
                Settings.CanvasHeight = 440
            Case Is < 1
                Settings.CanvasHeight = 609
        End Select
        Settings.CanvasWidth = Settings.CanvasHeight * ratio

    End Sub

    Private Sub btnINTRODUCTION_Click(ByVal sender As System.Object, _
    ByVal e As System.EventArgs) Handles BtnINTRODUCTION.Click
        Dim frm As New FormIntro
        frm.ShowDialog()


    End Sub


    Private Sub btnGOPAINT_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnGOPAINT.Click
        obtainrequests()

        Settings.NewObjectSeed()
        Settings.NewColorSeed()

        'btnREPEAT.Enabled = True

        Dim frm As New FormPainting
        frm.Settings = Settings
        '        frm.ObjectSeed = ObjectSeed
        frm.Show()
    End Sub

    Private Sub btnREPEAT_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        obtainrequests()

        Dim frm As New FormPainting
        frm.Settings = Settings
        '       frm.ObjectSeed = ObjectSeed
        frm.Show()
    End Sub


    'Public Sub summary()
    '    summary1 = "A(n) " & scenes & " painting with " & items & " and " & options & _
    '           " using a " & colorscheme & " color scheme with " _
    '    & colour & " being a main color. "

    '    summary2 = "There may also be a " & key & " key of values " & _
    '     intensity & " color intensity in a " _
    '     & design & " design and tonal composition of " & tonalcomp & ".  "
    '    MsgBox(summary1 & summary2)

    'End Sub

    Private Sub FormSettings_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
        '        obtainrequests()
    End Sub

    Private Sub FormSettings_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Randomize()
    End Sub

    '  COLOR WHEEL STUFF
    Private Sub lbCOLORSCHEME_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbCOLORSCHEME.SelectedIndexChanged
        colorscheme = lbCOLORSCHEME.SelectedItem
        If ButtonIndex <> 0 Then
            UpdateSelectedColors()
        End If
    End Sub

    Private Sub IncludeColor(ByVal Index As Integer)
        SelectedColors((ButtonIndex + Index + 17) Mod 18) = True
    End Sub

    Private Sub UpdateSelectedColors()

        For i As Integer = 0 To 17
            SelectedColors(i) = False
        Next
        dominantcolor = ButtonIndex
        If dominantcolor > 18 Then dominantcolor -= 18
        colour = colorname(dominantcolor)
        If dominantcolor < 8 Or dominantcolor > 16 Then
            temperature = "warm"
        Else
            temperature = "cool"
        End If

        colorscheme = Strings.Trim(lbCOLORSCHEME.Text)
        'If lbCOLORSCHEME.SelectedItems.Count = 0 Or compu = 1 Then
        '    lbCOLORSCHEME.SetSelected(Rnd() * lbCOLORSCHEME.Items.Count-1, True)
        'End If
        Dim dc As Integer = dominantcolor

        colours(0) = dc
        Select Case colorscheme
            Case "Monochromatic*"
                scheme = 0
                IncludeColor(0)
            Case "Triad*"
                scheme = 2
                IncludeColor(0)
                IncludeColor(6)
                IncludeColor(12)
                colours(1) = dc + 6
                colours(2) = dc + 12
            Case "Complementary*"
                scheme = 1
                IncludeColor(0)
                IncludeColor(9)
                colours(1) = dc + 9
            Case "Split Complementary*"
                scheme = 2
                IncludeColor(0)
                IncludeColor(8)
                IncludeColor(10)
                colours(1) = dc + 8
                colours(2) = dc + 10
            Case "Analogous*"
                scheme = 4
                IncludeColor(0)
                IncludeColor(1)
                IncludeColor(2)
                IncludeColor(16)
                IncludeColor(17)
                colours(1) = dc + 1
                colours(2) = dc + 2
                colours(3) = dc + 16
                colours(4) = dc + 17
            Case "Analogous with Complement*"
                scheme = 5
                IncludeColor(0)
                IncludeColor(1)
                IncludeColor(2)
                IncludeColor(9)
                IncludeColor(16)
                IncludeColor(17)
                colours(1) = dc + 1
                colours(2) = dc + 2
                colours(3) = dc + 9
                colours(4) = dc + 16
                colours(5) = dc + 17
            Case "CMB1"
                scheme = 10
                IncludeColor(0)
                initialize_CMBcolors()
            Case "CMB2"
                scheme = 11
                IncludeColor(0)
                initialize_CMBcolors()
            Case "CMB3"
                scheme = 12
                IncludeColor(0)
                initialize_CMBcolors()
            Case "CMB4"
                scheme = 13
                IncludeColor(0)
                initialize_CMBcolors()

        End Select
        For i As Integer = 0 To 5
            If colours(i) > 18 Then colours(i) -= 18
        Next
        Panel1.Refresh()

    End Sub

    Private Sub Panel1_Paint(ByVal sender As System.Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles Panel1.Paint

        Dim g As Graphics = Panel1.CreateGraphics()
        g.SmoothingMode = Drawing2D.SmoothingMode.HighQuality

        Dim outer As New Rectangle(0, 0, Panel1.Width, Panel1.Height)
        Dim inner As New Rectangle(40, 40, Panel1.Width - 80, Panel1.Height - 80)

        Dim pen As New Pen(Color.Cornsilk, 2)
        g.DrawEllipse(pen, inner)
        g.DrawEllipse(pen, outer)
        Dim brush As New SolidBrush(Color.Orange)
        Dim patch As Integer = 0
        For i As Integer = 0 To 359 Step 20

            patch = i / 20 + 1
            brush.Color = modColors.getcolor(patch, 1, 5)
            'initialize_cmbcolors()
            If patch = 1 Then ' lighter yellow
                brush.Color = Color.FromArgb(239, 239, 0)
            End If
            If patch = 2 Then  'lighter orange-yellow
                brush.Color = Color.FromArgb(240, 213, 0)
            End If
            If patch = 3 Then  'lighter yellow-orange
                brush.Color = Color.FromArgb(227, 178, 1)
            End If
            If patch = 4 Then  'lighter orange
                brush.Color = Color.FromArgb(252, 130, 10)
            End If
            If patch = 5 Then  'lighter red-orange
                brush.Color = Color.FromArgb(250, 100, 10)
            End If
            If patch = 7 Then  'darker red
                brush.Color = Color.FromArgb(255, 40, 40)
            End If
            If patch = 9 Then  'darker red-purple
                brush.Color = Color.FromArgb(230, 0, 230)
            End If
            If patch = 13 Then  'darker blue
                brush.Color = Color.FromArgb(88, 88, 255)
            End If
            If patch = 18 Then  'lighter green-yellow
                brush.Color = Color.FromArgb(185, 231, 0)
            End If
            If patch = 17 Then  'lighter yellow-green
                brush.Color = Color.FromArgb(113, 224, 0)
            End If
            If patch = 16 Then  'lighter green
                brush.Color = Color.FromArgb(0, 225, 0)
            End If
            If patch = 15 Then  'lighter blue-green
                brush.Color = Color.FromArgb(0, 185, 170)
            End If

            Dim path As New Drawing2D.GraphicsPath
            path.AddArc(outer, i - 99, 19)
            path.AddArc(inner, i - 80, -19)
            g.FillPath(brush, path)

        Next

        'design a black dot to represent selected colors on color chart
        Dim cx, cy, x, y As Integer
        cx = Panel1.Width / 2
        cy = Panel1.Height / 2
        For i As Integer = 0 To 17
            If SelectedColors(i) Then
                x = cx + (cx - 15) * Math.Sin(((i * 20) + 1) * Math.PI / 180)
                y = cy - (cy - 15) * Math.Cos(((i * 20) + 1) * Math.PI / 180)
                g.FillEllipse(Brushes.Black, x - 6, y - 6, 12, 12)
            End If
        Next
    End Sub

    Private Sub Panel1_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel1.MouseClick
        Dim d, rx, ry, a As Double
        Dim cx, cy As Integer
        cx = Panel1.Width / 2
        cy = Panel1.Height / 2
        rx = e.X - cx
        ry = e.Y - cy
        d = Math.Sqrt(rx * rx + ry * ry)
        If (d < cx) And (d > cx - 40) Then
            a = Math.Acos(rx / d) * 180 / Math.PI
            If ry < 0 Then a = 360 - a
            ButtonIndex = CInt(Math.Round((a + 10) / 20)) + 5 Mod 18
            UpdateSelectedColors()
        End If

    End Sub

    Private Sub Label18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label18.Click

    End Sub

    Private Sub lbOPTIONS_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbOPT.SelectedIndexChanged

    End Sub

    Private Sub lbSKY_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbSKY.SelectedIndexChanged

    End Sub
    Private Sub lbINTENSITY_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbINTENSITY.SelectedIndexChanged

    End Sub
    Private Sub lbSEASON_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbSEASON.SelectedIndexChanged

    End Sub

    Private Sub lbBACKGROUND_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbBACKGROUND.SelectedIndexChanged

    End Sub

    Private Sub lbMIDGROUND_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbMIDGROUND.SelectedIndexChanged

    End Sub

    Private Sub Label10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label10.Click

    End Sub

    Private Sub Label9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label9.Click

    End Sub

    Private Sub lbKEY_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbKEY.SelectedIndexChanged

    End Sub

    Private Sub Panel4_Paint(ByVal sender As System.Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles Panel4.Paint

    End Sub

    Private Sub lbCOLORTEST_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbCOLORTEST.SelectedIndexChanged

    End Sub

    Private Sub Label3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label3.Click

    End Sub
End Class

