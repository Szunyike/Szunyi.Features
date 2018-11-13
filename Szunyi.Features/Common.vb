
Imports System.Text.RegularExpressions
Imports Bio
Imports Bio.IO.GenBank

Public Class Common
    Public Shared Function Clones(Feats As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out As New List(Of FeatureItem)
        For Each Item In Feats
            out.Add(Item.Clone)
        Next
        Return out
    End Function
    Public Shared Function GetName(Feat As FeatureItem) As List(Of String)
        Dim out As New List(Of String)

        If Feat.Qualifiers.ContainsKey(StandardQualifierNames.LocusTag) AndAlso Feat.Qualifiers(StandardQualifierNames.LocusTag).Count > 0 Then
            out.Add(Feat.Qualifiers(StandardQualifierNames.LocusTag).First)
        ElseIf Feat.Qualifiers.ContainsKey(StandardQualifierNames.GeneSymbol) AndAlso Feat.Qualifiers(StandardQualifierNames.GeneSymbol).Count > 0 Then
            out.Add(Feat.Qualifiers(StandardQualifierNames.GeneSymbol).First)
        ElseIf Feat.Qualifiers.ContainsKey(StandardQualifierNames.Note) AndAlso Feat.Qualifiers(StandardQualifierNames.Note).Count > 0 Then
            out.Add(Feat.Qualifiers(StandardQualifierNames.Note).First)
        ElseIf Feat.Label <> "" Then
            out.Add(Feat.Label)
        End If
        If out.Count = 0 Then out.Add("UnKnown")
        Return out
    End Function
    '    Public Shared LocusTagComparer As New 
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

    Public Shared Function GetLocusTags(ExtFeat As ExtFeature) As List(Of String)
        Dim LocusTags As New List(Of String) ' First is full, Second is Short LocusTag
        LocusTags.Add(ExtFeat.LocusTag)
        Dim tmp = Split(ExtFeat.LocusTag, ".").First
        If LocusTags.First <> tmp Then LocusTags.Add(tmp)
        Return LocusTags
    End Function

    Public Shared Function Get_ShortLocusTags(Feats As List(Of FeatureItem)) As List(Of String)
        Dim out As New List(Of String)
        For Each Feat In Feats
            out.Add(Get_ShortLocusTag(Feat))
        Next
        Return out
    End Function

    Public Shared Function Get_ShortLocusTag(Feat As FeatureItem) As String
        Dim out As New List(Of String)
        Dim Ltag = Get_LocusTag(Feat)
        Dim s = Split(Ltag, ".").First
        Return s.Trim(Chr(34))

    End Function

    Public Shared Function GetNofIntron(feature As FeatureItem) As Integer
        If feature.Location.Operator = LocationOperator.Complement Then
            If feature.Location.SubLocations.First.Operator = LocationOperator.Join Then
                Return feature.Location.SubLocations.First.SubLocations.Count - 1
            Else
                Return 0
            End If
        Else ' No Complement
            If feature.Location.Operator = LocationOperator.Join Then
                Return feature.Location.SubLocations.First.SubLocations.Count - 1
            Else
                Return 0
            End If
        End If
    End Function

    Public Shared Function GetNofExon(feature As FeatureItem) As Integer
        If feature.Location.Operator = LocationOperator.Complement Then
            If feature.Location.SubLocations.First.Operator = LocationOperator.Join Then
                Return feature.Location.SubLocations.First.SubLocations.Count
            Else
                Return 1
            End If
        Else ' No Complement
            If feature.Location.Operator = LocationOperator.Join Then
                Return feature.Location.SubLocations.Count
            Else
                Return 1
            End If
        End If
    End Function


    ''' <summary>
    ''' Return LocusTags as list Of String 
    ''' If no LocusTag founded Empty String is returned
    ''' </summary>
    ''' <param name="Feats"></param>
    ''' <returns></returns>
    Public Shared Function Get_LocusTags(Feats As List(Of FeatureItem)) As List(Of String)
        Dim Out As New List(Of String)
        For Each Feat In Feats
            Out.Add(Get_LocusTag(Feat))
        Next
        Return Out
    End Function
    ''' <summary>
    ''' Return LocusTag or Empty String
    ''' </summary>
    ''' <param name="feat"></param>
    ''' <returns></returns>
    Public Shared Function Get_LocusTag(feat As FeatureItem) As String
        If feat.Qualifiers.ContainsKey(StandardQualifierNames.LocusTag) = True AndAlso
            feat.Qualifiers(StandardQualifierNames.LocusTag).Count > 0 Then
            Return feat.Qualifiers(StandardQualifierNames.LocusTag).First.Replace(Chr(34), "")
        Else
            Return String.Empty
        End If

    End Function

    Public Shared Function Get_ByQualifier_First(Qualifier_Name As String, Feat As FeatureItem) As String
        If Feat.Qualifiers.ContainsKey(Qualifier_Name) = True Then
            Return Feat.Qualifiers(Qualifier_Name).First
        Else
            Return String.Empty
        End If
    End Function

    Public Shared Function Get_ByQualifier_All(Qualifier_Name As String, feat As FeatureItem) As List(Of String)
        If feat.Qualifiers.ContainsKey(Qualifier_Name) = True Then
            Return feat.Qualifiers(Qualifier_Name).ToList
        Else
            Return New List(Of String)
        End If
    End Function

    Friend Shared Function Get_Feature_ByLocusTag(cDss As IEnumerable(Of FeatureItem), locus_Tag As String) As Object
        Throw New NotImplementedException()
    End Function

    Public Shared Function Get_Feature_Types(cSeqs As List(Of ISequence)) As List(Of String)
        Dim out As New List(Of String)
        For Each Seq In cSeqs
            out.AddRange(Get_Feature_Types(Seq))
        Next
        Return out.Distinct.ToList
    End Function
    Public Shared Function Get_Feature_Types(Seq As ISequence) As List(Of String)
        Dim Md = Szunyi.Features.GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(Seq)
        If IsNothing(Md) = True Then Return New List(Of String)
        Dim out = (From x In Md.Features.All Select x.Key)
        If out.Count = 0 Then Return New List(Of String)
        Return out.Distinct.ToList

    End Function

    Public Shared Function Get_Feature_Near_5_Prime_End(cmRNA As FeatureItem, cDSs As List(Of FeatureItem)) As FeatureItem
        If cmRNA.Location.IsComplementer = False Then
            Dim res = From x In cDSs Where x.Location.LocationStart >= cmRNA.Location.LocationStart And x.Location.LocationEnd <= cmRNA.Location.LocationEnd And x.Location.IsComplementer = False Order By x.Location.LocationStart Ascending

            If res.Count > 0 Then Return res.First

            Return Nothing
        Else
            Dim res = From x In cDSs Where x.Location.LocationStart >= cmRNA.Location.LocationStart And x.Location.LocationEnd <= cmRNA.Location.LocationEnd And x.Location.IsComplementer = True Order By x.Location.LocationEnd Descending

            If res.Count > 0 Then Return res.First

            Return Nothing
        End If
    End Function

    Public Shared Function Get_Five_UTRwCDS(cmRNA As FeatureItem, cCDS As FeatureItem) As FeatureItem
        Dim l As Bio.IO.GenBank.ILocation
        If cmRNA.Location.IsComplementer = False Then
            l = Szunyi.BLs.Location.Common.GetLocation(cmRNA.Location.LocationStart, cCDS.Location.LocationEnd, False)

        Else
            l = Szunyi.BLs.Location.Common.GetLocation(cmRNA.Location.LocationEnd, cCDS.Location.LocationStart, True)
        End If
        Return New FeatureItem("tmp", l)
    End Function

    Public Shared Function Get_Five_UTR(cmRNA As FeatureItem, cCDS As FeatureItem) As FeatureItem
        Dim l As Bio.IO.GenBank.ILocation
        If cmRNA.Location.IsComplementer = False Then
            l = Szunyi.BLs.Location.Common.GetLocation(cmRNA.Location.LocationStart, cCDS.Location.LocationStart, False)

        Else
            l = Szunyi.BLs.Location.Common.GetLocation(cmRNA.Location.LocationEnd, cCDS.Location.LocationEnd, True)
        End If
        Return New FeatureItem("tmp", l)
    End Function

    Public Shared Sub Set_Features_Location_accession(Seqs As List(Of ISequence))
        For Each seq In Seqs
            For Each Feat In Szunyi.Features.Manipulation.GetFeaturesByType.Get_All_Features(seq)
                Feat.Location.Accession = seq.ID
            Next

        Next
    End Sub

    Public Shared Function Correct_Location(seqs As List(Of ISequence)) As List(Of ISequence)
        For Each Seq In seqs
            Dim Feats = Szunyi.Features.Manipulation.GetFeaturesByType.Get_All_Features(Seq)
            For Each Feat In Feats
                Dim Loci = Feat.Location
                If Feat.Location.Operator = LocationOperator.Join AndAlso Feat.Location.SubLocations.Count > 0 AndAlso Feat.Location.SubLocations.First.Operator = LocationOperator.Complement Then
                    Dim kj As Int16 = 54
                    Dim s = Szunyi.BLs.Location.Common.GetLocationString(Feat)
                    Dim s1 = s.Replace("complement", "")
                    Dim s2 = s1.Replace("join", "complement(join")
                    Dim s3 = s2.Replace("((", "(")
                    Dim ret = Regex.Split(s3, "[^0-9]")
                    Dim N As New List(Of Integer)
                    For Each item In ret
                        If item <> String.Empty Then
                            N.Add(item)
                        End If
                    Next
                    N.Sort()

                    Dim str As New System.Text.StringBuilder
                    str.Append("complement(join(")
                    For i1 = 0 To N.Count - 1 Step 2
                        str.Append(N(i1)).Append("..").Append(N(i1 + 1)).Append(",")
                        Dim h As Int16 = 54
                    Next
                    str.Length -= 1
                    str.Append("))")
                    Dim loc = Szunyi.BLs.Location.Common.Get_Location(str.ToString)
                    Dim f As New FeatureItem(Feat.Key, loc)
                    Feat = f
                End If
            Next
        Next
        Return seqs
    End Function
End Class