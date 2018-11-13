Public Class Comparers
    ''' <summary>
    ''' Sort/Find by SeqID, After Startposition, After EndPosition, After Location String
    ''' </summary>
    Public Class ExtFeatureLocationComparer
        Implements IComparer(Of ExtFeature)

        Public Function Compare(x As ExtFeature, y As ExtFeature) As Integer Implements IComparer(Of ExtFeature).Compare
            Return x.LocationString.CompareTo(y.LocationString)
        End Function

    End Class

    ''' <summary>
    ''' Sort/Find by LocusTag of ExtFeature
    ''' </summary>
    Public Class ExtFeatureLocusTagComparer
        Implements IComparer(Of ExtFeature)

        Public Function Compare(x As ExtFeature, y As ExtFeature) As Integer Implements IComparer(Of ExtFeature).Compare
            If IsNothing(x.LocusTag) = False AndAlso IsNothing(y.LocusTag) = False Then
                Return x.LocusTag.CompareTo(y.LocusTag)
            Else
                Return x.Seq.ID.CompareTo(y.Seq.ID)
            End If

        End Function

    End Class
End Class
