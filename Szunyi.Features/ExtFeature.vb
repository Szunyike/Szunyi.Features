Imports Bio
Imports Bio.IO.GenBank
Imports Szunyi.Features.Manipulation
Imports Szunyi.Common.Enums
Public Class ExtFeature

    Private LociBuilder As New Bio.IO.GenBank.LocationBuilder
    Public Property Feature As FeatureItem
    Public Property LocusTag As String
    Public Property FirstItemToInvestigate As Integer
    Public Property LastItemToInvestigate As Integer
    Public Property Count As Integer = 0
    ' Searching/Sorting is faster SeqID And Location
    Public Property LocationString As String
    Public Property Seq As Bio.Sequence
    Public Sub New(feature As FeatureItem, Seq As Bio.Sequence)
        Me.Feature = feature
        Me.Seq = Seq
        If feature.Qualifiers.ContainsKey(StandardQualifierNames.LocusTag) Then
            Me.LocusTag = feature.Qualifiers(StandardQualifierNames.LocusTag).First.Replace(Chr(34), "")
        End If
        Me.LocationString = Me.Seq.ID & " " & LociBuilder.GetLocationString(feature.Location).Replace("<", "").Replace(">", "")
    End Sub
    Public Sub New(Ltag As String)
        Me.LocusTag = Ltag.Replace(Chr(34), "")
    End Sub

    Public Function GetQuilifierValues(Qulifiers As List(Of String), FivePrime As Integer, ThreePrime As Integer,
                                Optional Separator As String = " ") As String

        Dim str As New System.Text.StringBuilder
        For Each Qulifier In Qulifiers

            If Me.Feature.Qualifiers.ContainsKey(Qulifier) = True Then
                str.Append(Szunyi.Common.Text.General.GetText(Me.Feature.Qualifiers(Qulifier), Separator)).Append(Separator)
            Else
                str.Append(Separator)
            End If

        Next
        If str.Length >= Separator.Length Then str.Length -= Separator.Length
        If FivePrime <> 0 Then str.Append("|5' Extension " & FivePrime)
        If ThreePrime <> 0 Then str.Append("|3' Extension " & ThreePrime)
        Return str.ToString
    End Function

    Public Function GetQuilifierValue(Qulifier As String, FivePrime As Integer, ThreePrime As Integer,
                            Optional Separator As String = " ") As String

        Dim str As New System.Text.StringBuilder

        If Me.Feature.Qualifiers.ContainsKey(Qulifier) = True Then
            str.Append(Szunyi.Common.Text.General.GetText(Me.Feature.Qualifiers(Qulifier), Separator))
        Else
            str.Append(Separator)
        End If

        str.Length -= Separator.Length
        If FivePrime <> 0 Then str.Append("|5' Extension " & FivePrime)
        If ThreePrime <> 0 Then str.Append("|3' Extension " & ThreePrime)
        Return str.ToString
    End Function

End Class

Public Class ExtFeatureList
    ''' <summary>
    ''' Sort By Location (SeqID and After StartPositions After EndPosition After LocationString)
    ''' </summary>
    ''' <returns></returns>
    Public Property Features As New List(Of ExtFeature)
    Public Property FetauresByLocustag As New List(Of ExtFeature)
    Public Property SearchSetting As New SettingForSearchInFeaturesAndQulifiers
    Public Property ShortFileName As String
    Public Property UniqueId As Integer
    '  Public Property Type = Szunyi.Constants.BackGroundWork.Features
    Public Property Seqs As New List(Of Bio.ISequence)

    Public Sub New(SearchSetting As SettingForSearchInFeaturesAndQulifiers,
                           OriginalSeqs As List(Of Bio.ISequence), Optional Type As String = "")

        Me.SearchSetting = SearchSetting
        Me.Seqs = OriginalSeqs

    End Sub
    Public Sub New()

    End Sub

    Public Sub New(Seqs As List(Of Bio.ISequence), FeatureType As String)

        Me.SearchSetting = New SettingForSearchInFeaturesAndQulifiers(FeatureType)
        Me.Seqs = Seqs
        DoIt()
    End Sub
    Public Sub DoIt()
        Dim Result As List(Of ExtFeature) = Szunyi.Features.ExtFeatureManipulation.GetExtFeatures(Me.SearchSetting, Me.Seqs)
        FetauresByLocustag = Result

        FetauresByLocustag.Sort(New Comparers.ExtFeatureLocusTagComparer)

        Features.Sort(New Comparers.ExtFeatureLocationComparer)
        If SearchSetting.SelectedFeatures.Count = 46 Then
            Me.ShortFileName = Me.ShortFileName & " All Features"
        Else
            ShortFileName = Szunyi.Common.Text.General.GetText(SearchSetting.SelectedFeatures, " ") & " " & Me.ShortFileName

        End If

        '  SetIndexes()
    End Sub
    Public Sub SetIt()
        Dim Result As List(Of ExtFeature) = Szunyi.Features.ExtFeatureManipulation.GetExtFeatures(Me.SearchSetting, Me.Seqs)

        'FetauresByLocustag = CloneExtFeatureList(Result)
        FetauresByLocustag.Sort(New Comparers.ExtFeatureLocusTagComparer)
        Features.Sort(New Comparers.ExtFeatureLocationComparer)


    End Sub
#Region "Settings"

    Private Function GetMaxLength(Features As List(Of ExtFeature)) As Long
        Dim MaxLength As Integer = 0
        For Each item In Features
            If item.Feature.Location.LocationEnd - item.Feature.Location.LocationStart > MaxLength Then _
                        MaxLength = item.Feature.Location.LocationEnd - item.Feature.Location.LocationStart
        Next
        Return MaxLength
    End Function
    Private Sub SetIndexes()
        If Features.Count < 2 Then Exit Sub
        Dim MaxLength = GetMaxLength(Features)
        Dim cSeqID As String = Features.Last.Seq.ID
        For i1 = Features.Count - 1 To 0 Step -1
            If Features(i1).Seq.ID = cSeqID Then
                Features(i1).FirstItemToInvestigate = i1
                For i2 = i1 - 1 To 0 Step -1
                    If cSeqID = Features(i2).Seq.ID Then
                        If Features(i2).Feature.Location.LocationEnd >= Features(i1).Feature.Location.LocationStart Then
                            Features(i1).FirstItemToInvestigate = i2
                        End If
                        If Features(i2).Feature.Location.LocationStart + MaxLength < Features(i1).Feature.Location.LocationStart Then
                            Exit For
                        End If
                    Else
                        Exit For
                    End If

                Next
            Else
                ' Next sequence
                cSeqID = Features(i1).Seq.ID
                If i1 = 0 Then
                    Features(i1).FirstItemToInvestigate = 0
                    Features(i1).LastItemToInvestigate = 0
                    Exit For
                Else
                    i1 += 1
                End If
            End If
        Next

        For i1 = 0 To Features.Count - 1
            Features(i1).LastItemToInvestigate = i1
            If Features(i1).Seq.ID = cSeqID Then
                For i2 = i1 + 1 To Features.Count - 1
                    If Features(i1).Feature.Location.LocationEnd < Features(i2).Feature.Location.LocationStart Or
                                cSeqID <> Features(i2).Seq.ID Then
                        Exit For
                    Else
                        Features(i1).LastItemToInvestigate = i2
                    End If
                Next
            Else
                cSeqID = Features(i1).Seq.ID
                If i1 = Features.Count - 1 Then
                    Features(i1).LastItemToInvestigate = i1
                    Exit For
                Else
                    i1 = i1 - 1
                End If
            End If

        Next
    End Sub
#End Region

    Public Function GetOverLappedItems(x As ExtFeature) As List(Of ExtFeature)
        Dim Index = Features.BinarySearch(x, New Comparers.ExtFeatureLocationComparer)
        Dim tempy As New List(Of ExtFeature)
        If Index = -1 Then
            Return New List(Of ExtFeature)
        ElseIf Index < -1 Then
            Index = System.Math.Abs(Index) - 2
        End If
        Dim s = Features(Index).FirstItemToInvestigate - 1
        If s < 0 Then s = 0
        Dim e = Features(Index).FirstItemToInvestigate + 1
        If e = Features.Count Then e -= 1
        For i1 = s To e
            If Features(i1).Feature.Location.LocationStart >= x.Feature.Location.LocationStart _
                        And Features(i1).Feature.Location.LocationStart <= x.Feature.Location.LocationEnd Then
                tempy.Add(Features(i1))
            ElseIf Features(i1).Feature.Location.LocationEnd <= x.Feature.Location.LocationEnd And
                        Features(i1).Feature.Location.LocationEnd >= x.Feature.Location.LocationStart Then
                tempy.Add(Features(i1))
            ElseIf Features(i1).Feature.Location.LocationStart < x.Feature.Location.LocationStart And
                        x.Feature.Location.LocationEnd > x.Feature.Location.LocationEnd Then
                tempy.Add(Features(i1))
            End If
        Next



        Return tempy

    End Function


End Class
Public Class SettingForSearchInFeaturesAndQulifiers
    Public Property SelectedFeatures As New List(Of String)

    Public Property SettingForSearchInQulifier As New List(Of SettingForSearchInQulifier)


    Public Sub New()

    End Sub
    Public Sub New(Type As String)
        Me.SelectedFeatures.Add(Type)
    End Sub
    Public Sub New(Types As List(Of String))
        Me.SelectedFeatures = Types
    End Sub
End Class
Public Class ExtFeatureManipulation
    Public Class Parse
        Public Shared Iterator Function ByLocationString(ls As List(Of ExtFeature)) As IEnumerable(Of List(Of ExtFeature))
            Dim Gr = From a In ls Group By locString = Szunyi.bls.Location.Common.GetLocationString(a.Feature.Location) Into Group

            For Each g In Gr
                Yield g.Group.ToList
            Next
        End Function
    End Class
    Public Shared Property ExtFeatureLocusTagComparer As New Comparers.ExtFeatureLocusTagComparer

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="searchSetting"></param>
    ''' <param name="seqs"></param>
    ''' <returns></returns>
    Public Shared Function GetExtFeatures(searchSetting As SettingForSearchInFeaturesAndQulifiers,
     seqs As List(Of Bio.ISequence)) _
    As List(Of ExtFeature)
        Dim out As New List(Of ExtFeature)
        Dim Features As New List(Of FeatureItem)
        For Each Seq In seqs
            Try
                If Seq.Metadata.ContainsKey(Bio.Util.Helper.GenBankMetadataKey) Then
                    Dim x As New List(Of FeatureItem)
                    Dim md As Bio.IO.GenBank.GenBankMetadata = Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey)

                    For Each FeatureType In searchSetting.SelectedFeatures

                        Dim t1 = GetFeaturesByType.GetFeturesByTypeFromSeq(Seq, FeatureType)
                        If IsNothing(t1) = False Then
                            Dim t = t1.ToList
                            Dim XXX As New List(Of List(Of FeatureItem))
                            If searchSetting.SettingForSearchInQulifier.Count > 0 Then

                                For Each Setting In searchSetting.SettingForSearchInQulifier
                                    Dim tmp As New List(Of FeatureItem)
                                    Select Case Setting.Type
                                        Case SearchType.Contains
                                            tmp.AddRange(GetFeatureByQualifier.GetFeaturesByQulifiersContains(t, Setting))

                                        Case SearchType.Exact
                                            tmp.AddRange(GetFeatureByQualifier.GetFeaturesByQulifiersPerfect(t, Setting))
                                        Case SearchType.NoValue
                                            tmp.AddRange(GetFeatureByQualifier.GetFeaturesByNoValues(t, Setting))
                                    End Select
                                    XXX.Add(tmp)
                                Next
                                Dim h As List(Of FeatureItem) = XXX.First
                                For i1 = 1 To XXX.Count - 1
                                    Dim l = h.Intersect(XXX(i1))
                                    If l.Count > 0 Then h = l.ToList
                                Next
                                x.AddRange(h)
                            Else
                                x.AddRange(t)
                            End If
                        End If

                    Next
                    For Each Item In x
                        out.Add(New ExtFeature(Item, Seq))
                    Next
                End If
            Catch ex As Exception
                Dim alf As Int16 = 54
            End Try
        Next
        out.Sort(New Comparers.ExtFeatureLocusTagComparer)
        Return out
    End Function



    Public Shared Function GetExtFeaturesFromFeature(Features As List(Of FeatureItem), Seq As Bio.Sequence) _
                  As List(Of ExtFeature)
        Dim out As New List(Of ExtFeature)
        If IsNothing(Features) = True Then Return Nothing
        For Each Item In Features
            out.Add(New ExtFeature(Item, Seq))
        Next

        out.Sort(New Comparers.ExtFeatureLocationComparer)
        Return out
    End Function

    ''' <summary>
    ''' Return the String Reprasantioan of Annotation of Features
    ''' Optionally Remove special Strings
    ''' Annotation are Separated by VbCrlf
    ''' </summary>
    ''' <param name="FeatureList"></param>
    ''' <param name="Qulifiers"></param>
    ''' <param name="Separator"></param>
    ''' <param name="ToRemove"></param>
    ''' <returns></returns>
    Public Shared Function GetTextFromExtFeatureList(FeatureList As ExtFeatureList,
                                                  Qulifiers As List(Of String), Separator As String, Optional ToRemove As String = "") As String
        Dim str As New System.Text.StringBuilder

        For Each Feature In FeatureList.Features
            str.Append(GetTextFromExtFeature(Feature.Feature, Qulifiers, Separator, ToRemove))
            str.Append(vbTab).Append(Feature.Feature.Location.LocationStart).Append(vbTab).Append(Feature.Feature.Location.LocationEnd)
            str.AppendLine()
        Next
        If str.Length > 1 Then str.Length -= 1
        Return str.ToString
    End Function

    ''' <summary>
    ''' Return the String Representation of Selected Qulifiers From Feature
    ''' Optionally Remove Special Strings
    ''' </summary>
    ''' <param name="Feature"></param>
    ''' <param name="Qulifiers"></param>
    ''' <param name="Separator"></param>
    ''' <param name="ToRemove"></param>
    ''' <returns></returns>
    Public Shared Function GetTextFromExtFeature(Feature As FeatureItem,
                                              Qulifiers As List(Of String),
                                              Separator As String, Optional ToRemove As String = "") As String
        Dim str As New System.Text.StringBuilder
        Dim LociBuilder As New Bio.IO.GenBank.LocationBuilder
        For Each Qulifier In Qulifiers

            If Feature.Qualifiers.ContainsKey(Qulifier) = True Then
                str.Append(Szunyi.Common.Text.General.GetText(Feature.Qualifiers(Qulifier), " ")).Append(Separator)
            Else
                    str.Append(Separator)
                End If

        Next
        str.Length -= Separator.Length
        If ToRemove = "" Then Return str.ToString
        Return str.ToString.Replace(ToRemove, "")
    End Function
    Public Shared Function GetTextFromExtFeature(feature As FeatureItem, Qulifier As String) As String
        If feature.Qualifiers.ContainsKey(Qulifier) = True Then
            Return Szunyi.Common.Text.General.GetText(feature.Qualifiers(Qulifier), " ")
        Else
            Return String.Empty
        End If
    End Function

    ''' <summary>
    ''' Return Sorted By ExtFeatureLocusTag
    ''' </summary>
    ''' <param name="extFeatureLists"></param>
    ''' <returns></returns>
    Public Shared Function MergeLists(extFeatureLists As List(Of ExtFeatureList)) As List(Of ExtFeature)
        Dim Out As New List(Of ExtFeature)
        If IsNothing(extFeatureLists) = True Then Return Out
        For Each ExtFeatureList In extFeatureLists
            Out.AddRange(ExtFeatureList.Features)
        Next
        Out.Sort(New Comparers.ExtFeatureLocusTagComparer)
        Return Out
    End Function
    Public Shared Function GetCDSLength(extFeatures As List(Of ExtFeature)) As List(Of String)
        Dim out As New List(Of String)
        For Each ExtFeature In extFeatures
            Dim Seq = ExtFeature.Feature.GetSubSequence(ExtFeature.Seq)
            out.Add(Seq.Count)
        Next
        Return out

    End Function

    Public Shared Function GetFirstExon(extFeature As ExtFeature) As Bio.Sequence
        With extFeature.Feature
            If .Location.Operator = LocationOperator.Complement Then
                If .Location.SubLocations.First.Operator = LocationOperator.Join Then
                    Dim s = .Location.SubLocations.First.SubLocations.Last.LocationStart
                    Dim e = .Location.SubLocations.First.SubLocations.Last.LocationEnd
                    Dim Seq As Bio.Sequence = extFeature.Seq.GetSubSequence(s - 1,
                                                                               e - s + 1)
                    Dim Lseq As Bio.Sequence = Seq.GetReverseComplementedSequence
                    Return Seq
                Else ' Single Exon
                    Dim Seq As Bio.Sequence = extFeature.Seq.GetSubSequence(.Location.SubLocations.First.LocationStart - 1,
                                                                               .Location.SubLocations.First.LocationEnd -
                                                                               .Location.SubLocations.First.LocationStart + 1)

                    Return Seq.GetReverseComplementedSequence
                End If
            ElseIf .Location.Operator = LocationOperator.Join Then
                Dim Seq As Bio.Sequence = extFeature.Seq.GetSubSequence(.Location.SubLocations.First.LocationStart - 1,
                                                                                .Location.SubLocations.First.LocationEnd -
                                                                                .Location.SubLocations.First.LocationStart + 1)
                Return Seq
            Else ' Single Exon
                Dim Seq As Bio.Sequence = extFeature.Seq.GetSubSequence(.Location.LocationStart - 1,
                                                                               .Location.LocationEnd -
                                                                               .Location.LocationStart + 1)
                Return Seq

            End If
        End With
    End Function

    Public Shared Function GetCDSLengthinAA(extFeatures As List(Of ExtFeature)) As List(Of String)
        Dim out As New List(Of String)
        For Each ExtFeature In extFeatures
            Dim Seq = ExtFeature.Feature.GetSubSequence(ExtFeature.Seq)
            out.Add(Seq.Count \ 3)
        Next
        Return out
    End Function

#Region "Get Sequences"

    Public Shared Function Get_Original_Sequences(feature_Lists As List(Of ExtFeatureList)) As List(Of Bio.ISequence)
        Dim out As New List(Of Bio.ISequence)
        If IsNothing(feature_Lists) = True Then Return New List(Of Bio.ISequence)

        For Each feature_List In feature_Lists
            out.AddRange(Get_Original_Sequences(feature_List))
        Next
        Return out.Distinct.ToList
    End Function
    Public Shared Function Get_Original_Sequences(feature_List As ExtFeatureList) As List(Of Bio.ISequence)
        Dim Seqs As New List(Of Bio.ISequence)
        If IsNothing(feature_List) = True Then Return New List(Of Bio.ISequence)
        For Each Feat In feature_List.Features
            If Seqs.Contains(Feat.Seq) = False Then
                Seqs.Add(Feat.Seq)
            End If
        Next
        Return Seqs
    End Function
#End Region
    Public Shared Function GetSequences(featureList As ExtFeatureList, qulifiers As List(Of String),
                                                  Optional OnlyFirst As Boolean = False,
                                               Optional With_Filename As Boolean = True,
                                               Optional With_Location As Boolean = False,
                                               Optional ExtraPromoterLength As Integer = 0,
                                               Optional ExtraUTRLength As Integer = 0) As IEnumerable(Of Sequence)
        Dim Out As New List(Of Bio.Sequence)
        Dim log As New List(Of String)
        For Each Item In featureList.Features
            Try
                Out.Add(GetSequence(Item, qulifiers, OnlyFirst, With_Filename, With_Location, ExtraPromoterLength, ExtraUTRLength))

            Catch ex As Exception
                log.Add(Item.Feature.Key & vbTab & Item.LocationString)
            End Try

        Next
        Return Out
    End Function
    Public Shared Function GetSequence(gene As ExtFeature,
                                               Optional Qulifiers As List(Of String) = Nothing,
                                               Optional OnlyFirst As Boolean = False,
                                               Optional With_Filename As Boolean = True,
                                               Optional With_Location As Boolean = False,
                                               Optional ExtraPromoterLength As Integer = 0,
                                               Optional ExtraUTRLength As Integer = 0) As Bio.Sequence
        Try



            Dim tmpSeq As Bio.Sequence = gene.Feature.GetSubSequence(gene.Seq)
            If With_Filename = True Then tmpSeq.ID = gene.Seq.ID & vbTab

            tmpSeq.ID = tmpSeq.ID & Szunyi.Features.Qulifiers.Get_Values_From_Feature(gene.Feature, Qulifiers, OnlyFirst)
            If With_Location = True Then
                tmpSeq.ID = tmpSeq.ID & "_" & Szunyi.BLs.Location.Common.GetLocationString(gene.Feature.Location)
            End If
            tmpSeq.ID = tmpSeq.ID.Replace(Chr(34), "").Replace(" ", "_").Replace("__", "_")
            If IsNothing(tmpSeq.ID) = True Then tmpSeq.ID = gene.Seq.ID
            If gene.Feature.Location.Operator = LocationOperator.Complement Then
                tmpSeq = tmpSeq.GetReversedSequence
            End If

            If IsNothing(tmpSeq.ID) = True Then
                tmpSeq.ID = ""
            Else
                tmpSeq.ID = tmpSeq.ID.Replace(Chr(34), "")

            End If

            Return tmpSeq
        Catch ex As Exception
            Dim ald As Int16 = 43
        End Try
        Return Nothing
    End Function
#Region "Get Ext_Features From Seqs"
    Public Shared Function GetExtFeaturesFromSeqsByTypes(Seqs As List(Of Sequence), FeatureTypes As List(Of String)) As List(Of ExtFeature)
        Dim x As New List(Of String)
        Dim Out As New List(Of ExtFeature)
        Dim tmpExtFeature As New ExtFeature("")
        Dim LocusTagComparer = New Comparers.ExtFeatureLocusTagComparer
        For Each Seq In Seqs
            For Each FeatureType In FeatureTypes
                Out.AddRange(GetExtFeaturesFromSeqByType(Seq, FeatureType))
            Next
        Next
        Return Out
    End Function
    Public Shared Function GetExtFeaturesFromSeqByTypes(Seq As Bio.Sequence, FeatureTypes As List(Of String)) As List(Of ExtFeature)
        Dim x As New List(Of String)
        Dim Out As New List(Of ExtFeature)
        Dim tmpExtFeature As New ExtFeature("")
        Dim LocusTagComparer = New Comparers.ExtFeatureLocusTagComparer

        For Each FeatureType In FeatureTypes
            Out.AddRange(GetExtFeaturesFromSeqByType(Seq, FeatureType))
        Next

        Return Out
    End Function
    Public Shared Function GetExtFeaturesFromSeqsByType(Seqs As List(Of Sequence), FeatureType As String) As List(Of ExtFeature)
        Dim x As New List(Of String)
        Dim Out As New List(Of ExtFeature)
        Dim tmpExtFeature As New ExtFeature("")
        Dim LocusTagComparer = New Comparers.ExtFeatureLocusTagComparer
        For Each Seq In Seqs
            Out.AddRange(GetExtFeaturesFromSeqByType(Seq, FeatureType))
        Next
        Return Out
    End Function



    Public Shared Function GetExtFeaturesFromSeqByType(Seq As Bio.Sequence, FeatureType As String) As List(Of ExtFeature)
        Dim out As New List(Of ExtFeature)
        Dim t1 = GetFeaturesByType.GetFeturesByTypeFromSeq(Seq, FeatureType)
        For Each Item In t1
            out.Add(New ExtFeature(Item, Seq))
        Next

        Return out
    End Function

#End Region

#Region "Get Ext_Features By LocusTag"
    Public Shared Function GetExtFeutureByLocusTags(extFeatureList As ExtFeatureList, locusTags As List(Of String)) As List(Of ExtFeature)
        Dim Out As New List(Of ExtFeature)

        For Each LocusTag In locusTags
            LocusTag = Split(LocusTag, " ").First
            If LocusTag.StartsWith("MTR") Then
                Dim alf As Int16 = 54
            End If
            Dim t = extFeatureList.FetauresByLocustag.BinarySearch(New ExtFeature(LocusTag), ExtFeatureLocusTagComparer)
            If t > -1 Then
                Out.Add(extFeatureList.FetauresByLocustag(t))
            End If
        Next

        Return Out
    End Function

    Public Shared Function GetExtFeutureByLocusTag(extFeatureLists As List(Of ExtFeatureList), locusTag As String) As ExtFeature
        locusTag = Chr(34) & locusTag & Chr(34)
        For Each extFeatureList In extFeatureLists
            Dim t = extFeatureList.FetauresByLocustag.BinarySearch(New ExtFeature(locusTag), ExtFeatureLocusTagComparer)
            If t > -1 Then
                Return extFeatureList.FetauresByLocustag(t)
            End If
        Next
        locusTag = locusTag.Replace(Chr(34), "")
        For Each extFeatureList In extFeatureLists
            Dim t = extFeatureList.FetauresByLocustag.BinarySearch(New ExtFeature(locusTag), ExtFeatureLocusTagComparer)
            If t > -1 Then
                Return extFeatureList.FetauresByLocustag(t)
            End If
        Next
        Return Nothing
    End Function

    Public Shared Function GetExtFeutureByLocusTag(extFeatures As List(Of ExtFeature), locusTag As String) As ExtFeature
        Dim Index = extFeatures.BinarySearch(New ExtFeature(locusTag), ExtFeatureLocusTagComparer)
        If Index > -1 Then
            Return extFeatures(Index)
        End If

        Return Nothing
    End Function

    Public Shared Function GetExtFeutureByLocusTag(extFeatureLists As ExtFeatureList, locusTag As String) As ExtFeature

        Dim t = extFeatureLists.FetauresByLocustag.BinarySearch(New ExtFeature(locusTag), New Comparers.ExtFeatureLocusTagComparer)
        If t > -1 Then
            Return extFeatureLists.FetauresByLocustag(t)
        End If

        Return Nothing
    End Function

    Public Shared Function GetExtFeatures(locusTags As List(Of String), featList As ExtFeatureList) As List(Of ExtFeature)
        Dim x As New List(Of String)
        Dim Out As New List(Of ExtFeature)
        Dim tmpExtFeature As New ExtFeature("")
        Dim LocusTagComparer = New Comparers.ExtFeatureLocusTagComparer
        For Each LocusTag In locusTags

            tmpExtFeature.LocusTag = LocusTag
            Dim i1 = featList.FetauresByLocustag.BinarySearch(tmpExtFeature, LocusTagComparer)
            If i1 >= 0 Then
                Out.Add(featList.FetauresByLocustag(i1))
                For i2 = i1 - 1 To 0 Step -1
                    If featList.FetauresByLocustag(i2).LocusTag = LocusTag Then
                        Out.Add(featList.FetauresByLocustag(i2))
                    Else
                        Exit For
                    End If
                Next
                For i2 = i1 + 1 To featList.FetauresByLocustag.Count - 1
                    If featList.FetauresByLocustag(i2).LocusTag = LocusTag Then
                        Out.Add(featList.FetauresByLocustag(i2))
                    Else
                        Exit For
                    End If
                Next
            End If
        Next
        Return Out
    End Function

#End Region

#Region "Create Ext_Features"
    Public Shared Function CreateExtFeatures(Ls As List(Of ILocation), ExtFeat As ExtFeature, Type As String) As IEnumerable(Of ExtFeature)
        Dim out As New List(Of ExtFeature)
        If IsNothing(Ls) = False Then
            For i1 = 0 To Ls.Count - 1
                out.Add(CreateExtFeature(Ls(i1), ExtFeat, Type, i1 + 1))
            Next

        End If

        Return out
    End Function
    Public Shared Function CreateExtFeature(Loc As ILocation, ExtFeat As ExtFeature, Type As String, Optional SubIndex As Integer = Nothing) As ExtFeature
        Dim t As New FeatureItem(Type, Loc)
        t = Szunyi.Features.MergeFeatures.Merge2Features(ExtFeat.Feature, t, True)
        Dim MdLocusTag As New List(Of String)
        If IsNothing(SubIndex) = False Then
            MdLocusTag = Szunyi.Features.Common.GetName(t)
            MdLocusTag(0) = MdLocusTag.First.Replace(Chr(34), "") & "_" & SubIndex

        End If

        Dim x As New ExtFeature(t, ExtFeat.Seq)
        x.LocusTag = MdLocusTag.First
        Return x
    End Function
#End Region


#Region "Get LocusTags"
    Public Shared Function GetLocusTags(Ext_Features As List(Of ExtFeature)) As List(Of String)
        Dim out As New List(Of String)
        If IsNothing(Ext_Features) = True Then Return out
        For Each Ext_Feature In Ext_Features
            out.Add(GetLocusTag(Ext_Feature))
        Next
        Return out
    End Function
    Public Shared Function GetLocusTag(Ext_feature As ExtFeature) As String
        If Ext_feature.Feature.Qualifiers.ContainsKey(StandardQualifierNames.LocusTag) = False Then Return String.Empty
        Return Ext_feature.Feature.Qualifiers(StandardQualifierNames.LocusTag).First

    End Function

    Public Shared Function GetShortLocusTags(Ext_Features As List(Of ExtFeature)) As List(Of String)
        Dim out As New List(Of String)
        If IsNothing(Ext_Features) = True Then Return out
        For Each Ext_Feature In Ext_Features
            out.Add(GetShortLocusTag(Ext_Feature))
        Next
        Return out
    End Function
    Public Shared Function GetShortLocusTag(Ext_feature As ExtFeature) As String
        If Ext_feature.Feature.Qualifiers.ContainsKey(StandardQualifierNames.LocusTag) = False Then Return String.Empty

        Return Split(Ext_feature.Feature.Qualifiers(StandardQualifierNames.LocusTag).First, ".").First
    End Function

    Public Shared Function GetPureLocusTags(Ext_Features As List(Of ExtFeature)) As List(Of String)
        Dim out As New List(Of String)
        If IsNothing(Ext_Features) = True Then Return out
        For Each Ext_Feature In Ext_Features
            out.Add(GetPureLocusTag(Ext_Feature))
        Next
        Return out
    End Function
    Public Shared Function GetPureLocusTag(Ext_feature As ExtFeature) As String

        Return Split(Ext_feature.Feature.Qualifiers(StandardQualifierNames.LocusTag).First, " ").First.Replace(Chr(34), "")

    End Function

#End Region

#Region "Get Extended Keys = LocusTag & Type"
    ''' <summary>
    '''  LocusTag Feature_Key Product Strand  Separated By Tab
    ''' </summary>
    ''' <param name="Ext_Features"></param>
    ''' <returns></returns>
    Public Shared Function Get_Extended_Keys(Ext_Features As List(Of ExtFeature)) As List(Of String)
        Dim out As New List(Of String)
        For Each Ext_Feature In Ext_Features
            out.Add(Get_Extended_Key(Ext_Feature))
        Next
        Return out

    End Function
    ''' <summary>
    ''' LocusTag Feature_Key Product Strand  Separated By Tab
    ''' </summary>
    ''' <param name="Ext_Feature"></param>
    ''' <returns></returns>
    Public Shared Function Get_Extended_Key(Ext_Feature As ExtFeature) As String
        Dim out As New System.Text.StringBuilder
        out.Append(Ext_Feature.Feature.Qualifiers(StandardQualifierNames.LocusTag).First).Append(vbTab)
        out.Append(Ext_Feature.Feature.Key).Append(vbTab)
        If Ext_Feature.Feature.Qualifiers.ContainsKey(StandardQualifierNames.Product) Then
            out.Append(Ext_Feature.Feature.Qualifiers(StandardQualifierNames.Product).First).Append(vbTab)
        Else
            out.Append("Unknown").Append(vbTab)
        End If
        If Ext_Feature.Feature.Location.Operator = LocationOperator.Complement Then
            out.Append("+").Append(vbTab)
        Else
            out.Append("-").Append(vbTab)
        End If
        Return out.ToString
    End Function

#End Region

#Region "Get Feature Type"
    Public Shared Function Get_Features_ByExtended_Keys(Ext_Features As List(Of ExtFeature), Extended_Keys As List(Of String))
        Extended_Keys.Sort()
        Dim Out As New List(Of ExtFeature)
        For Each Ext_Feature In Ext_Features
            Dim Ext_Key = Szunyi.Features.ExtFeatureManipulation.Get_Extended_Key(Ext_Feature)
            Dim Index = Extended_Keys.BinarySearch(Ext_Key)
            If Index > -1 Then
                Out.Add(Ext_Feature)
            End If
        Next
        Return Out
    End Function

    Public Shared Function GetFeatureTypes(Ext_Features As List(Of ExtFeature)) As List(Of String)
        Dim out As New List(Of String)
        For Each Ext_Feature In Ext_Features
            out.Add(GetFeatureType(Ext_Feature))
        Next
        Return out
    End Function
    Public Shared Function GetFeatureType(Ext_Feature As ExtFeature) As String
        Return Ext_Feature.Feature.Key
    End Function
#End Region

#Region "Get Strands"
    Public Shared Function GetStrands(Ext_Features As List(Of ExtFeature)) As List(Of String)
        Dim out As New List(Of String)
        For Each Ext_Feature In Ext_Features
            out.Add(GetStrand(Ext_Feature))
        Next
        Return out
    End Function
    Public Shared Function GetStrand(Ext_feature As ExtFeature) As String
        If Ext_feature.Feature.Location.Operator = LocationOperator.Complement Then
            Return "-"
        Else
            Return "+"
        End If
    End Function

#End Region

End Class
