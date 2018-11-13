Imports Bio
Imports Bio.IO.GenBank
Imports Szunyi.Common
Imports Szunyi.Features.Manipulation

Public Class MergeFeatures
    Public Shared Function MergeByTypeAnsPositions(Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out As New List(Of FeatureItem)
        For Each FeatureType In Iterator.ByType(Features)
            For Each SamePosFeat In Iterator.ByLocation(FeatureType)
                If SamePosFeat.Count > 1 Then
                    Dim nFeat = MergeFeatures.MergeQualifiers(SamePosFeat)
                    out.Add(nFeat)
                Else
                    out.Add(SamePosFeat.First)
                End If
            Next
        Next
        Return out
    End Function

    ''' <summary>
    ''' Merge the Annotaion if different, but not The LocusTag
    ''' </summary>
    ''' <param name="FeatureForm"></param>
    ''' <param name="FeatureTo"></param>
    ''' <param name="WithLocusTag"></param>
    ''' <returns></returns>
    Public Shared Function Merge2Features(FeatureForm As FeatureItem,
                                          FeatureTo As FeatureItem,
                                          WithLocusTag As Boolean, Optional WithLocations As Boolean = False) As FeatureItem
        Dim Qulifiers = StandardQualifierNames.All
        For Each Qual In StandardQualifierNames.All
            If WithLocusTag = False And Qual = StandardQualifierNames.LocusTag Then

            Else

                If FeatureTo.Qualifiers.ContainsKey(Qual) = False AndAlso FeatureForm.Qualifiers.ContainsKey(Qual) = True Then
                    FeatureTo.Qualifiers(Qual) = FeatureForm.Qualifiers(Qual)
                ElseIf FeatureTo.Qualifiers.ContainsKey(Qual) AndAlso FeatureForm.Qualifiers.ContainsKey(Qual) = True Then
                    Dim sFeat1 = Szunyi.Common.Text.General.GetText(FeatureTo.Qualifiers(Qual), " ").Replace(Chr(34), "")
                    Dim sFeat2 = Szunyi.Common.Text.General.GetText(FeatureForm.Qualifiers(Qual), " ").Replace(Chr(34), "")
                    If sFeat1 <> sFeat2 Then
                        Dim x As New List(Of String)
                        x.Add(sFeat1)
                        x.Add(sFeat2)
                        FeatureTo.Qualifiers(Qual) = x
                    End If
                End If
            End If

        Next
        If WithLocations = True Then

        End If
        Return FeatureTo
    End Function

    ''' <summary>
    ''' Return the Connencted Qulifiers or Nothing
    ''' if dbcrossref Not "" then search in DatabaseCrossFiles
    ''' </summary>h
    ''' <param name="Feat"></param>
    ''' <param name="FirstQulifier"></param>
    ''' <param name="SecondQulifier"></param>
    ''' <param name="dbCrossRef"></param>
    ''' <returns></returns>
    Public Shared Function GetCorrespondingQulifierAndDbXref(Feat As FeatureItem, FirstQulifier As String, SecondQulifier As String, Optional dbCrossRef As String = "") As Other_Database.CrossRefs.CrossRefOneToOne
        If Feat.Qualifiers.ContainsKey(FirstQulifier) = False Then Return Nothing
        If Feat.Qualifiers.ContainsKey(SecondQulifier) = False Then Return Nothing
        Dim First = Text.General.GetText(Feat.Qualifiers(FirstQulifier), " ").Replace(Chr(34), "")
        If dbCrossRef <> "" Then
            Dim tmp = From x In Feat.Qualifiers(SecondQulifier) Where x.Contains(dbCrossRef)

            If tmp.Count = 0 Then Return Nothing
            Dim Second = Text.General.GetText(tmp.ToList).Replace(dbCrossRef, "").Replace(Chr(34), "")
            Return New Other_Database.CrossRefs.CrossRefOneToOne(First, Second)
        Else
            Dim Second = Text.General.GetText(Feat.Qualifiers(SecondQulifier), " ")
            Return New Other_Database.CrossRefs.CrossRefOneToOne(First, Second)
        End If

    End Function

    ''' <summary>
    ''' Return the Connencted Qulifiers or Nothing
    ''' if dbcrossref Not "" then search in DatabaseCrossFiles
    ''' </summary>
    ''' <param name="Feat"></param>
    ''' <param name="FirstQulifier"></param>
    ''' <param name="SecondQulifiers"></param>
    ''' <returns></returns>
    Public Shared Function GetCorrespondingQulifierAndDbXref(Feat As FeatureItem, FirstQulifier As String, SecondQulifiers() As String) As Other_Database.CrossRefs.CrossRefOneToOne
        If Feat.Qualifiers.ContainsKey(FirstQulifier) = False Then Return Nothing
        Dim Out As New List(Of String)
        For Each SecondQulifier In SecondQulifiers
            If Feat.Qualifiers.ContainsKey(SecondQulifier) = True Then
                Dim s = Text.General.GetText(Feat.Qualifiers(SecondQulifier), " ")
                If Out.Contains(s) = False Then Out.Add(s)
            End If
        Next
        If Out.Count = 0 Then Return Nothing
        Dim First = Text.General.GetText(Feat.Qualifiers(FirstQulifier), " ").Replace(Chr(34), "")


        Dim Second = Text.General.GetText(Out, " ").Replace(Chr(34), "")
        Return New Other_Database.CrossRefs.CrossRefOneToOne(First, Second)


    End Function

    Public Shared Sub MergeQualifiers(ByRef From As FeatureItem, ByRef Toa As FeatureItem)
        Toa.Qualifiers.Clear()
        For Each sg In From.Qualifiers
            Toa.Qualifiers.Add(sg.Key, sg.Value)
        Next

    End Sub
    Public Shared Function MergeQualifiers(Feats As List(Of FeatureItem)) As FeatureItem
        Dim out As New FeatureItem(Feats.First.Key, Feats.First.Location)
        For Each QulifierName In StandardQualifierNames.All
            Dim t As New List(Of String)
            For Each feat In Feats
                If feat.Qualifiers.ContainsKey(QulifierName) Then
                    t.AddRange(feat.Qualifiers(QulifierName))
                End If
            Next
            t = t.Distinct.ToList
            If t.Count > 1 Then
                Dim alf As Int16 = 43
                out.Qualifiers.Add(QulifierName, t)
            ElseIf t.Count = 1 Then
                out.Qualifiers.Add(QulifierName, t)
            End If
        Next
        Return out
    End Function
    Public Shared Function MergeQualifiers(Feats() As FeatureItem) As FeatureItem
        Dim out As New FeatureItem(Feats.First.Key, Feats.First.Location)
        For Each QulifierName In StandardQualifierNames.All
            Dim t As New List(Of String)
            For Each feat In Feats
                If feat.Qualifiers.ContainsKey(QulifierName) Then
                    t.AddRange(feat.Qualifiers(QulifierName))
                End If
            Next
            t = t.Distinct.ToList
            If t.Count > 1 Then
                Dim alf As Int16 = 43
                out.Qualifiers.Add(QulifierName, t)
            ElseIf t.Count = 1 Then
                out.Qualifiers.Add(QulifierName, t)
            End If
        Next
        Return out
    End Function
    Public Shared Function MergeQualifiers(BasicFeat As FeatureItem, Feats As List(Of FeatureItem)) As FeatureItem
        For Each QulifierName In StandardQualifierNames.All
            Dim t As New List(Of String)
            For Each feat In Feats
                If feat.Qualifiers.ContainsKey(QulifierName) Then
                    t.AddRange(feat.Qualifiers(QulifierName))
                End If
            Next
            t = t.Distinct.ToList
            If t.Count > 1 Then
                Dim alf As Int16 = 43
                BasicFeat.Qualifiers.Add(QulifierName, t)
            ElseIf t.Count = 1 Then
                BasicFeat.Qualifiers.Add(QulifierName, t)
            End If

        Next
        Return BasicFeat
    End Function
End Class