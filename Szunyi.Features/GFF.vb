Imports System.Drawing
Imports System.IO
Imports Bio
Imports Bio.IO.GenBank
Imports Szunyi.Common
Imports Szunyi.Features

Public Class GFF
    Public Class GFF_Annotation
        Public SeqID As String
        Public Type As String
        Public Start As Integer
        Public Endy As Integer
        Public IsComplementer As Boolean = False
        Public Frame As Integer
        Public Parent As String
        Public ID As String
        Public Product As String
        Public Label As String
        Public transcript_id As String
        Public gene_id As String
        Dim s1() As String
        Public Sub New(Line As String)
            s1 = Split(Line, vbTab)
            Me.SeqID = s1(0)
            Me.Type = s1(2)
            Me.Start = s1(3)
            Me.Endy = s1(4)
            If s1(6) = "-" Then IsComplementer = True
            If IsNumeric(s1(7)) = True Then Me.Frame = s1(7)

            Dim s2 = Split(s1(8), ";")
            Me.ID = s2(0).Split("=").Last.Split(" ").Last.Replace(Chr(34), "")
            For i1 = 0 To s2.Count - 1
                's2(i1) = 'Trim(s2(i1), " ").First
                If s2(i1).StartsWith("Parent") Then
                    Me.Parent = s2(i1).Split("=").Last
                ElseIf s2(i1).StartsWith("Product") Then
                    Me.Product = s2(i1).Split("=").Last
                ElseIf s2(i1).StartsWith("description") Then
                    Me.Product = s2(i1).Split("=").Last
                ElseIf s2(i1).StartsWith("Name") Then
                    Me.Label = s2(i1).Split("=").Last
                ElseIf s2(i1).StartsWith("conf_class") Then
                    Dim t1 As Integer = 43
                ElseIf s2(i1).StartsWith("transcript_id") Then
                    Me.transcript_id = Split(s2(i1), "transcript_id").Last.Replace(Chr(34), "").Replace(" ", "")
                ElseIf s2(i1).StartsWith("gene_id") Then
                    Me.gene_id = Split(s2(i1), "gene_id").Last.Replace(Chr(34), "").Replace(" ", "")
                    If IsNothing(Me.Parent) = True Then
                        Me.Parent = Me.gene_id
                    End If
                Else

                    Dim alft As Integer = 43
                End If

            Next
            Dim alf As Int16 = 43
        End Sub
        Public Overrides Function ToString() As String
            Return s1.GetText()


        End Function

    End Class

    Private Shared Function GetLocation(p1 As String, ann As GFF_Annotation) As Bio.IO.GenBank.Location
        Try
            Dim loc As New LocationBuilder
            If Split(p1, "..").Count > 2 Then p1 = "join(" & p1 & ")"
            If ann.IsComplementer = True Then p1 = "complement(" & p1 & ")"
            Return loc.GetLocation(p1)
        Catch ex As Exception
            Return Nothing
        End Try

    End Function
    Private Shared Function GetLocation(Item As GFF_Annotation) As Bio.IO.GenBank.Location
        Try
            Dim loc As New LocationBuilder
            Dim p1 As String = Item.Start & ".." & Item.Endy
            If Item.IsComplementer = True Then p1 = "complement(" & p1 & ")"
            Return loc.GetLocation(p1)
        Catch ex As Exception
            Return Nothing
        End Try

    End Function
    Private Shared Sub AddFeature(x As FeatureItem, ann As GFF_Annotation, Seq As Bio.Sequence)
        Try
            x.Add_Qualfier(StandardQualifierNames.LocusTag, ann.ID)

            If ann.Product <> "" Then
                x.Add_Qualfier(StandardQualifierNames.Product, ann.Product)
            End If
            If ann.Label <> "" Then
                x.Add_Qualfier(StandardQualifierNames.Label, ann.Label)
            End If



            Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey).Features.All.Add(x)
            Dim alf As Int16 = 54
        Catch ex As Exception
            Dim alf As Int16 = 56
        End Try

    End Sub
    Public Shared Function Parse(seqs As List(Of ISequence), Annotations As List(Of GFF_Annotation)) As List(Of ISequence)
        Dim c As New Szunyi.Sequences.Sorters.ByID

        seqs.Sort(c)
        Dim t As New Bio.Sequence(Alphabets.AmbiguousDNA, "T")
        Dim str As New System.Text.StringBuilder
        For i1 = 0 To Annotations.Count - 1
            With Annotations(i1)
                t.ID = Annotations(i1).SeqID
                Dim Index = seqs.BinarySearch(t, c)
                If Index > -1 Then
                    Dim cSeq = seqs(Index)

                    Dim loc = GetLocation(Annotations(i1))
                    Select Case .Type
                        Case "gene"
                            Dim x As New Gene(loc)
                            AddFeature(x, Annotations(i1), cSeq)
                        Case "CDS"
                            str.Length = 0
                            Dim pts As New List(Of Point)
                            For i2 = i1 To Annotations.Count - 1
                                If Annotations(i2).Parent <> Annotations(i1).Parent Or Annotations(i2).Type <> "CDS" Then

                                    Dim h = From x1 In pts Select x1 Order By x1.X

                                    For Each v In h
                                        str.Append(v.X & ".." & v.Y & ",")
                                    Next
                                    str.Length -= 1
                                    loc = GetLocation(str.ToString, Annotations(i1))
                                    Dim x As New CodingSequence(loc)
                                    AddFeature(x, Annotations(i1), cSeq)
                                    i1 = i2 - 1
                                    Exit For
                                Else
                                    pts.Add(New Point(Annotations(i2).Start, Annotations(i2).Endy))

                                End If
                            Next
                        Case "exon"
                            Dim alf As Integer = 43
                        Case "mRNA"
                            Dim x As New MessengerRna(loc)
                            AddFeature(x, Annotations(i1), cSeq)
                        Case "transposable_element"

                        Case "tRNA"
                            Dim x As New TransferRna(loc)
                            AddFeature(x, Annotations(i1), cSeq)
                        Case "rRNA"
                            Dim x As New RibosomalRna(loc)
                            AddFeature(x, Annotations(i1), cSeq)
                        Case Else

                    End Select


                End If

            End With

        Next
        Return seqs
    End Function

    Public Shared Function Get_Annotations(File As FileInfo) As List(Of GFF_Annotation)
        Dim res As New List(Of GFF_Annotation)
        Using sr As New StreamReader(File.FullName)
            Do
                Dim Line As String = sr.ReadLine
                If Line.StartsWith("#") = False Then ' Else This is a comment and forget it!
                    Dim x As New GFF_Annotation(Line)
                    res.Add(x)
                End If
            Loop Until sr.EndOfStream = True
        End Using
        Return res
    End Function

    Public Shared Function Get_Annotations(Files As List(Of FileInfo)) As List(Of GFF_Annotation)
        Dim res As New List(Of GFF_Annotation)
        For Each File In Files
            res.AddRange(Get_Annotations(File))
        Next

        Return res
    End Function
End Class
