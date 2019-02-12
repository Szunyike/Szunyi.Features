
Imports System.Text.RegularExpressions
Imports Bio
Imports Bio.IO.GenBank
Imports Szunyi.Common
Imports Szunyi.Location.bls


Public Class Common


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


    Public Shared Function Correct_Location(seqs As List(Of ISequence)) As List(Of ISequence)
        For Each Seq In seqs

            For Each Feat In Seq.Get_All_Features()
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