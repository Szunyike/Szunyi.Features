Imports Bio
Imports Bio.IO.GenBank
Imports Szunyi.Common

Public Class MergeFeatures


    ''' <summary>
    ''' Merge the Annotaion if different, but not The LocusTag
    ''' </summary>
    ''' <param name="FeatureForm"></param>
    ''' <param name="FeatureTo"></param>
    ''' <returns></returns>



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