Imports Bio.IO.GenBank

Public Class Convert
    Public Shared Function ToBed(Feats As List(Of FeatureItem), seq As Bio.ISequence) As List(Of Bio.SequenceRange)
        Dim out As New List(Of Bio.SequenceRange)
        For Each f In Feats
            out.Add(ToBed(f, seq))
        Next
        Return out
    End Function
    Public Shared Function ToBed(Feat As FeatureItem, seq As Bio.ISequence) As Bio.SequenceRange
        Dim x As New Bio.SequenceRange(seq.ID, Feat.Location.LocationStart, Feat.Location.LocationEnd)
        x.Metadata.Add("Name", Feat.Label)
        x.Metadata.Add("Strand", Get_Strand(Feat.Location))
        Return x
    End Function

    ''' <summary>
    ''' Return + or -
    ''' </summary>
    ''' <param name="location"></param>
    ''' <returns></returns>
    Public Shared Function Get_Strand(location As ILocation) As String
        If location.Operator = LocationOperator.Complement Then
            Return "-"
        Else
            If location.SubLocations.Count > 0 AndAlso location.SubLocations.First.Operator = LocationOperator.Complement Then
                Return "-"
            Else
                Return "+"
            End If

        End If
    End Function
End Class
