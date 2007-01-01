Public Class FormPainting

    Private FSettings As TSettings
    Private FFilename As String
    '    Private FObjectSeed As Integer

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

    Public Sub FormPainting_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim Yextra, Xextra As Integer
        Yextra = Height - PictureBox1.Height
        Xextra = Width - PictureBox1.Width

        If Settings.CanvasWidth <> 0 Then
            Height = Settings.CanvasHeight + Yextra
            Width = Settings.CanvasWidth + Xextra
        End If
    End Sub

    Private Sub PrintToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PrintToolStripMenuItem.Click
        MsgBox("Print")
    End Sub

    Private Sub SaveToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveToolStripMenuItem.Click
        If FFilename = "" Then
            SaveAsToolStripMenuItem_Click(sender, e)
        Else
            PictureBox1.Image.Save(FFilename, GetFormat(FFilename))
        End If
    End Sub

    Private Sub SaveAsToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveAsToolStripMenuItem.Click
        SaveFileDialog1.Title = "Choose file to save in"
        SaveFileDialog1.Filter = "Bitmap Files (*.bmp)|*.bmp|" & _
                                 "JPEG Files (*.jpg)|*.jpg|" & _
                                 "PNG Files (*.png)|*.png|" & _
                                 "All Files (*.*)|*.*"
        FFilename = ""
        If SaveFileDialog1.ShowDialog <> Windows.Forms.DialogResult.Cancel Then
            FFilename = SaveFileDialog1.FileName

            MsgBox("File name = " & FFilename)

            PictureBox1.Image.Save(FFilename, GetFormat(FFilename))
        End If
    End Sub

    Private Function GetFormat(ByVal AFileName As String) As Imaging.ImageFormat
        Dim Extension As String
        Extension = IO.Path.GetExtension(AFileName).ToLower

        Select Case Extension
            Case ".jpg" : Return Imaging.ImageFormat.Jpeg
            Case ".bmp" : Return Imaging.ImageFormat.Bmp
            Case ".png" : Return Imaging.ImageFormat.Png
            Case Else : Return Imaging.ImageFormat.Jpeg
        End Select
    End Function

    Private Sub RepaintToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RepaintToolStripMenuItem.Click
        Settings.NewColorSeed()
        Repaint()
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Repaint()
        Timer1.Enabled = False
    End Sub

    Private Sub Repaint()
        Dim painter As New TPainter
         PictureBox1.Size = Me.ClientSize 'New System.Drawing.Size(Me.WinWidth, Me.Height)
        ' PictureBox1.Image = New Bitmap(PictureBox1.Width, PictureBox1.Height)
        PictureBox1.Image = New Bitmap(PictureBox1.Width, 460) 'PictureBox1.Height)

        painter.Image = PictureBox1.Image
        painter.Settings = FSettings
        'painter.ObjectSeed = FObjectSeed

        painter.Make____Nice____Painting()
    End Sub

    Private Sub NewWindowToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim frm As New FormPainting
        frm.Settings = Settings
        'frm.FObjectSeed = FObjectSeed
        frm.Show()
    End Sub

    Private Sub RegenerateToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RegenerateToolStripMenuItem.Click
        Settings.NewObjectSeed()
        Settings.NewColorSeed()
        Repaint()
    End Sub

    Private Sub PictureBox1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.Click

    End Sub

    Protected Overrides Sub Finalize()
        MyBase.Finalize()
    End Sub
End Class