Imports System.Runtime.CompilerServices
Imports Bio.IO.GenBank
Imports System.Windows.Forms

Module Location_Extension
    <Extension()>
    Public Function TSS(Location As Bio.IO.GenBank.Location) As Integer
        If Location.Operator = LocationOperator.Complement Then
            Return Location.LocationEnd
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return Location.LocationEnd
        Else
            Return Location.LocationStart
        End If
    End Function
    <Extension()>
    Public Function PAS(Location As Bio.IO.GenBank.Location) As Integer
        If Location.Operator = LocationOperator.Complement Then
            Return Location.LocationStart
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return Location.LocationStart
        Else
            Return Location.LocationEnd
        End If
    End Function
    <Extension()>
    Public Function IsComplementer(Location As Bio.IO.GenBank.Location) As Boolean
        If Location.Operator = LocationOperator.Complement Then
            Return True
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return True
        Else
            Return False
        End If
    End Function

    <Extension()>
    Public Function TSS(Location As Bio.IO.GenBank.ILocation) As Integer
        If Location.Operator = LocationOperator.Complement Then
            Return Location.LocationEnd
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return Location.LocationEnd
        Else
            Return Location.LocationStart
        End If
    End Function
    <Extension()>
    Public Function PAS(Location As Bio.IO.GenBank.ILocation) As Integer
        If Location.Operator = LocationOperator.Complement Then
            Return Location.LocationStart
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return Location.LocationStart
        Else
            Return Location.LocationEnd
        End If
    End Function
    <Extension()>
    Public Function IsComplementer(Location As Bio.IO.GenBank.ILocation) As Boolean
        If Location.Operator = LocationOperator.Complement Then
            Return True
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return True
        Else
            Return False
        End If
    End Function

End Module


