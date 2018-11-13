Imports System.Text
Imports Bio
Imports Bio.IO.GenBank
Imports Szunyi.Features.Manipulation

Public Class Manipulation
    Public Class Key
        Public Shared Function Get_All_Different_Keys(Seqs As List(Of Bio.ISequence)) As List(Of String)
            Dim out As New List(Of String)
            For Each Seq In Seqs
                out.AddRange(Get_All_Different_Keys(Seq))
            Next
            If out.Count > 0 Then
                Return out.Distinct.ToList
            Else
                Return out
            End If
        End Function
        Public Shared Function Get_All_Different_Keys(Seq As Bio.ISequence) As List(Of String)
            Dim md = GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(Seq)
            If IsNothing(md) = True Then Return New List(Of String)
            Dim res = From x In md.Features.All Select x.Key

            If res.Count > 0 Then
                Return res.Distinct.ToList
            Else
                Return New List(Of String)
            End If
        End Function

        Public Shared Function ReName_Keys(TRs As List(Of FeatureItem), Key As String) As List(Of FeatureItem)
            Dim nTRs As New List(Of FeatureItem)
            For Each Item In TRs
                Dim x As New FeatureItem(Key, Item.Location)
                nTRs.Add(MergeFeatures.Merge2Features(Item, x, True))
            Next
            Return nTRs
        End Function
    End Class
    Public Class Get_Features
        Public Shared Function All(seq As Bio.ISequence) As List(Of FeatureItem)
            Dim out As New List(Of FeatureItem)

            Dim md = GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(seq)
            If IsNothing(md) = True Then Return out
            out.AddRange((From x In md.Features.All).ToList)
            Return out

        End Function
        Public Shared Function All(seqs As List(Of Bio.ISequence)) As List(Of FeatureItem)
            Dim out As New List(Of FeatureItem)
            For Each Seq In seqs
                Dim md = GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(Seq)
                If IsNothing(md) = True Then Return out
                out.AddRange((From x In md.Features.All).ToList)
            Next
            Return out

        End Function
        Public Shared Function By_Type(Seqs As List(Of Bio.ISequence),
                                               SelectedFeature As String) As List(Of FeatureItem)

            Dim MDs = GenBankMetaDataManipulation.GetGenBankMetaDatasFromSeqs(Seqs)
            Return GetFeatureByTypeFromMetadata(SelectedFeature, MDs.First)

        End Function
        ''' <summary>
        ''' Return Ienumarable of FeatureItem or Nothing
        ''' </summary>
        ''' <param name="Type"></param>
        ''' <param name="md"></param>
        ''' <returns></returns>
        Private Shared Function GetFeatureByTypeFromMetadata(Type As String, md As Bio.IO.GenBank.GenBankMetadata) As IEnumerable(Of FeatureItem)
            If IsNothing(md) = True Then Return Nothing
            If IsNothing(md.Features) = True Then Return Nothing

            Select Case Type

                Case = StandardFeatureKeys.Attenuator
                    If md.Features.Attenuators.Count > 0 Then Return md.Features.Attenuators

                Case = StandardFeatureKeys.CaaTSignal
                    If md.Features.CAATSignals.Count > 0 Then Return md.Features.CAATSignals

                Case = StandardFeatureKeys.CodingSequence
                    If md.Features.CodingSequences.Count > 0 Then Return md.Features.CodingSequences

                Case = StandardFeatureKeys.DisplacementLoop
                    If md.Features.DisplacementLoops.Count > 0 Then Return md.Features.DisplacementLoops

                Case = StandardFeatureKeys.Enhancer
                    If md.Features.Enhancers.Count > 0 Then Return md.Features.Enhancers

                Case = StandardFeatureKeys.Exon
                    If md.Features.Exons.Count > 0 Then Return md.Features.Exons

                Case = StandardFeatureKeys.FivePrimeUtr
                    If md.Features.FivePrimeUTRs.Count > 0 Then Return md.Features.FivePrimeUTRs

                Case = StandardFeatureKeys.GcSingal
                    If md.Features.GCSignals.Count > 0 Then Return md.Features.GCSignals

                Case = StandardFeatureKeys.Gene
                    If md.Features.Genes.Count > 0 Then Return md.Features.Genes

                Case = StandardFeatureKeys.InterveningDna
                    If md.Features.InterveningDNAs.Count > 0 Then Return md.Features.InterveningDNAs

                Case = StandardFeatureKeys.Intron
                    If md.Features.Introns.Count > 0 Then Return md.Features.Introns

                Case = StandardFeatureKeys.LongTerminalRepeat
                    If md.Features.LongTerminalRepeats.Count > 0 Then Return md.Features.LongTerminalRepeats

                Case = StandardFeatureKeys.MaturePeptide
                    If md.Features.MaturePeptides.Count > 0 Then Return md.Features.MaturePeptides

                Case = StandardFeatureKeys.MessengerRna
                    If md.Features.MessengerRNAs.Count > 0 Then Return md.Features.MessengerRNAs

                Case = StandardFeatureKeys.Minus10Signal
                    If md.Features.Minus10Signals.Count > 0 Then Return md.Features.Minus10Signals

                Case = StandardFeatureKeys.Minus35Signal
                    If md.Features.Minus35Signals.Count > 0 Then Return md.Features.Minus35Signals

                Case = StandardFeatureKeys.MiscBinding
                    If md.Features.MiscBindings.Count > 0 Then Return md.Features.MiscBindings

                Case = StandardFeatureKeys.MiscDifference
                    If md.Features.MiscDifferences.Count > 0 Then Return md.Features.MiscDifferences

                Case = StandardFeatureKeys.MiscFeature
                    If md.Features.MiscFeatures.Count > 0 Then Return md.Features.MiscFeatures

                Case = StandardFeatureKeys.MiscRecombination
                    If md.Features.MiscRecombinations.Count > 0 Then Return md.Features.MiscRecombinations

                Case = StandardFeatureKeys.MiscRna
                    If md.Features.MiscRNAs.Count > 0 Then Return md.Features.MiscRNAs

                Case = StandardFeatureKeys.MiscSignal
                    If md.Features.MiscSignals.Count > 0 Then Return md.Features.MiscSignals

                Case = StandardFeatureKeys.MiscStructure
                    If md.Features.MiscStructures.Count > 0 Then Return md.Features.MiscStructures

                Case = StandardFeatureKeys.ModifiedBase
                    If md.Features.ModifiedBases.Count > 0 Then Return md.Features.ModifiedBases

                Case = StandardFeatureKeys.NonCodingRna
                    If md.Features.NonCodingRNAs.Count > 0 Then Return md.Features.NonCodingRNAs

                Case = StandardFeatureKeys.OperonRegion
                    If md.Features.OperonRegions.Count > 0 Then Return md.Features.OperonRegions

                Case = StandardFeatureKeys.PolyASignal
                    If md.Features.PolyASignals.Count > 0 Then Return md.Features.PolyASignals

                Case = StandardFeatureKeys.PolyASite
                    If md.Features.PolyASites.Count > 0 Then Return md.Features.PolyASites

                Case = StandardFeatureKeys.PrecursorRna
                    If md.Features.PrecursorRNAs.Count > 0 Then Return md.Features.PrecursorRNAs

                Case = StandardFeatureKeys.Promoter
                    If md.Features.Promoters.Count > 0 Then Return md.Features.Promoters

                Case = StandardFeatureKeys.ProteinBindingSite
                    If md.Features.ProteinBindingSites.Count > 0 Then Return md.Features.ProteinBindingSites

                Case = StandardFeatureKeys.RepeatRegion
                    If md.Features.RepeatRegions.Count > 0 Then Return md.Features.RepeatRegions

                Case = StandardFeatureKeys.ReplicationOrigin
                    If md.Features.ReplicationOrigins.Count > 0 Then Return md.Features.ReplicationOrigins

                Case = StandardFeatureKeys.RibosomalRna
                    If md.Features.RibosomalRNAs.Count > 0 Then Return md.Features.RibosomalRNAs

                Case = StandardFeatureKeys.RibosomeBindingSite
                    If md.Features.RibosomeBindingSites.Count > 0 Then Return md.Features.RibosomeBindingSites

                Case = StandardFeatureKeys.SignalPeptide
                    If md.Features.SignalPeptides.Count > 0 Then Return md.Features.SignalPeptides

                Case = StandardFeatureKeys.StemLoop
                    If md.Features.StemLoops.Count > 0 Then Return md.Features.StemLoops

                Case = StandardFeatureKeys.TataSignal
                    If md.Features.TATASignals.Count > 0 Then Return md.Features.TATASignals

                Case = StandardFeatureKeys.Terminator
                    If md.Features.Terminators.Count > 0 Then Return md.Features.Terminators

                Case = StandardFeatureKeys.ThreePrimeUtr
                    If md.Features.ThreePrimeUTRs.Count > 0 Then Return md.Features.ThreePrimeUTRs

                Case = StandardFeatureKeys.TransferMessengerRna
                    If md.Features.TransferMessengerRNAs.Count > 0 Then Return md.Features.TransferMessengerRNAs

                Case = StandardFeatureKeys.TransferRna
                    If md.Features.TransferRNAs.Count > 0 Then Return md.Features.TransferRNAs

                Case = StandardFeatureKeys.TransitPeptide
                    If md.Features.TransitPeptides.Count > 0 Then Return md.Features.TransitPeptides

                Case = StandardFeatureKeys.UnsureSequenceRegion
                    If md.Features.UnsureSequenceRegions.Count > 0 Then Return md.Features.UnsureSequenceRegions

                Case = StandardFeatureKeys.Variation
                    If md.Features.Variations.Count > 0 Then Return md.Features.Variations
                Case Else
                    Dim res = From x In md.Features.All Where x.Key = Type

                    If res.Count > 0 Then Return res.ToList

                    Return New List(Of FeatureItem)
            End Select
            Return Nothing
        End Function

    End Class
    Public Class GetFeaturesByType
        Public Shared Function Get_All_Features(seq As Bio.ISequence) As List(Of FeatureItem)
            Dim out As New List(Of FeatureItem)

            Dim md = GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(seq)
            If IsNothing(md) = True Then Return out
            out.AddRange((From x In md.Features.All).ToList)


            Return out

        End Function
        Public Shared Function Get_All_Features(seqs As List(Of Bio.ISequence)) As List(Of FeatureItem)
            Dim out As New List(Of FeatureItem)
            For Each Seq In seqs
                Dim md = GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(Seq)
                If IsNothing(md) = True Then Return out
                out.AddRange((From x In md.Features.All).ToList)
            Next
            Return out

        End Function
        Public Shared Function GetFeturesByTypeFromSeqStartWith(seq As Bio.ISequence, key As String) As List(Of FeatureItem)
            Dim out As New List(Of FeatureItem)
            Dim md = GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(seq)
            If IsNothing(md) = True Then Return out
            out = (From x In md.Features.All Where x.Key.StartsWith(key) = True).ToList

            Return out
        End Function
        Public Shared Function GetFeturesByTypeFromSeqContains(seq As Bio.ISequence, key As String) As List(Of FeatureItem)
            Dim out As New List(Of FeatureItem)
            Dim md = GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(seq)
            If IsNothing(md) = True Then Return out
            out = (From x In md.Features.All Where x.Key.Contains(key) = True).ToList

            Return out
        End Function
        Public Shared Function GetFeturesByTypesFromSeqs(Seqs As List(Of Bio.ISequence),
                                               SelectedFeatureTypes As List(Of String)) As List(Of FeatureItem)

            Dim MDs = GenBankMetaDataManipulation.GetGenBankMetaDatasFromSeqs(Seqs)
            Return GetFeturesByTypesFromMetaDatas(MDs, SelectedFeatureTypes)

        End Function
        Public Shared Function GetFeturesByTypesFromSeq(Seq As Bio.ISequence,
                                               SelectedFeatureTypes As List(Of String)) As List(Of FeatureItem)

            Dim MD = GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(Seq)
            Return GetFeturesByTypesFromMetadata(MD, SelectedFeatureTypes)

        End Function
        ''' <summary>
        ''' Return not Sorted List
        ''' </summary>
        ''' <param name="Seqs"></param>
        ''' <param name="SelectedFeatureType"></param>
        ''' <returns></returns>
        Public Shared Function GetFeturesByTypeFromSeqs(Seqs As List(Of Bio.ISequence),
                                               SelectedFeatureType As String) As List(Of FeatureItem)

            Dim MDs = GenBankMetaDataManipulation.GetGenBankMetaDatasFromSeqs(Seqs)
            Dim out As New List(Of FeatureItem)
            For Each Md In MDs
                Dim t = GetFeatureByTypeFromMetadata(SelectedFeatureType, Md)
                If IsNothing(t) = False Then out.AddRange(t)
            Next
            Return out

        End Function
        Public Shared Function GetFeturesSeqsByTypeFromSeqs(Seqs As List(Of Bio.ISequence),
                                               SelectedFeatureType As String,
                                               ByRef log As StringBuilder) As List(Of Bio.ISequence)

            Dim out As New List(Of Bio.ISequence)
            For Each Seq In Seqs
                out.AddRange(GetFeturesSeqByTypeFromSeqs(Seq, SelectedFeatureType, log))
            Next
            If log.Length > 0 Then
                Dim alf As Int16 = 54
            End If
            Return out

        End Function
        Public Shared Function GetFeturesSeqByTypeFromSeqs(Seq As Bio.ISequence,
                                               SelectedFeatureType As String, ByRef log As StringBuilder) As List(Of Bio.ISequence)


            Dim out As New List(Of Bio.ISequence)

            Dim MD = GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(Seq)
            Dim Features = GetFeatureByTypeFromMetadata(SelectedFeatureType, MD)
            If IsNothing(Features) = False Then
                For Each Feat In Features
                    Try
                        out.Add(Feat.GetSubSequence(Seq))
                        out.Last.ID = Feat.Qualifiers(StandardQualifierNames.LocusTag).First
                    Catch ex As Exception
                        log.Append(Seq.ID).AppendLine()
                    End Try

                Next
            End If

            Return out

        End Function

        ''' <summary>
        ''' Return Nothing Or Sorted list (By LocusTAg)
        ''' </summary>
        ''' <param name="Seq"></param>
        ''' <param name="SelectedFeatureType"></param>
        ''' <returns></returns>
        Public Shared Function GetFeturesByTypeFromSeq(Seq As Bio.ISequence,
                                               SelectedFeatureType As String) As List(Of FeatureItem)

            Dim Md = GenBankMetaDataManipulation.GetGenBankMetaDataFromSeq(Seq)
            Dim SelectedFeatures = GetFeatureByTypeFromMetadata(SelectedFeatureType, Md)


            If IsNothing(SelectedFeatures) = False Then
                Dim SelFeats = SelectedFeatures.ToList
                '   SelFeats.Sort(AllComparares.ByLocusTag)
                Return SelFeats
            End If
            Return New List(Of FeatureItem)
        End Function
        Public Shared Function GetFeturesByTypesFromMetadata(MD As Bio.IO.GenBank.GenBankMetadata,
                                               SelectedFeatureTypes As List(Of String)) As List(Of FeatureItem)

            Dim out As New List(Of FeatureItem)
            For Each SelectedFeature In SelectedFeatureTypes
                Dim t = GetFeatureByTypeFromMetadata(SelectedFeature, MD)
                If IsNothing(t) = False Then out.AddRange(t)
            Next
            Return out

        End Function
        ''' <summary>
        ''' Return list or empty list
        ''' </summary>
        ''' <param name="MDs"></param>
        ''' <param name="SelectedFeatureTypes"></param>
        ''' <returns></returns>
        Public Shared Function GetFeturesByTypesFromMetaDatas(MDs As List(Of Bio.IO.GenBank.GenBankMetadata),
                                                SelectedFeatureTypes As List(Of String)) As List(Of FeatureItem)

            Dim Out As New List(Of FeatureItem)
            For Each Md In MDs

                Dim t = GetFeturesByTypesFromMetadata(Md, SelectedFeatureTypes)
                If IsNothing(t) = False Then Out.AddRange(t)
            Next

            Return Out
        End Function
        Public Shared Function GetFeturesByTypesFromMetaDatas(MDs As List(Of Bio.IO.GenBank.GenBankMetadata),
                                               SelectedFeatureType As String) As List(Of FeatureItem)

            Dim Out As New List(Of FeatureItem)
            For Each Md In MDs

                Dim t = GetFeatureByTypeFromMetadata(SelectedFeatureType, Md)
                If IsNothing(t) = False Then Out.AddRange(t)
            Next

            Return Out
        End Function



        ''' <summary>
        ''' Return Ienumarable of FeatureItem or Nothing
        ''' </summary>
        ''' <param name="Type"></param>
        ''' <param name="md"></param>
        ''' <returns></returns>
        Public Shared Function GetFeatureByTypeFromMetadata(Type As String, md As Bio.IO.GenBank.GenBankMetadata) As IEnumerable(Of FeatureItem)
            If IsNothing(md) = True Then Return Nothing
            If IsNothing(md.Features) = True Then Return Nothing

            Select Case Type

                Case = StandardFeatureKeys.Attenuator
                    If md.Features.Attenuators.Count > 0 Then Return md.Features.Attenuators

                Case = StandardFeatureKeys.CaaTSignal
                    If md.Features.CAATSignals.Count > 0 Then Return md.Features.CAATSignals

                Case = StandardFeatureKeys.CodingSequence
                    If md.Features.CodingSequences.Count > 0 Then Return md.Features.CodingSequences

                Case = StandardFeatureKeys.DisplacementLoop
                    If md.Features.DisplacementLoops.Count > 0 Then Return md.Features.DisplacementLoops

                Case = StandardFeatureKeys.Enhancer
                    If md.Features.Enhancers.Count > 0 Then Return md.Features.Enhancers

                Case = StandardFeatureKeys.Exon
                    If md.Features.Exons.Count > 0 Then Return md.Features.Exons

                Case = StandardFeatureKeys.FivePrimeUtr
                    If md.Features.FivePrimeUTRs.Count > 0 Then Return md.Features.FivePrimeUTRs

                Case = StandardFeatureKeys.GcSingal
                    If md.Features.GCSignals.Count > 0 Then Return md.Features.GCSignals

                Case = StandardFeatureKeys.Gene
                    If md.Features.Genes.Count > 0 Then Return md.Features.Genes

                Case = StandardFeatureKeys.InterveningDna
                    If md.Features.InterveningDNAs.Count > 0 Then Return md.Features.InterveningDNAs

                Case = StandardFeatureKeys.Intron
                    If md.Features.Introns.Count > 0 Then Return md.Features.Introns

                Case = StandardFeatureKeys.LongTerminalRepeat
                    If md.Features.LongTerminalRepeats.Count > 0 Then Return md.Features.LongTerminalRepeats

                Case = StandardFeatureKeys.MaturePeptide
                    If md.Features.MaturePeptides.Count > 0 Then Return md.Features.MaturePeptides

                Case = StandardFeatureKeys.MessengerRna
                    If md.Features.MessengerRNAs.Count > 0 Then Return md.Features.MessengerRNAs

                Case = StandardFeatureKeys.Minus10Signal
                    If md.Features.Minus10Signals.Count > 0 Then Return md.Features.Minus10Signals

                Case = StandardFeatureKeys.Minus35Signal
                    If md.Features.Minus35Signals.Count > 0 Then Return md.Features.Minus35Signals

                Case = StandardFeatureKeys.MiscBinding
                    If md.Features.MiscBindings.Count > 0 Then Return md.Features.MiscBindings

                Case = StandardFeatureKeys.MiscDifference
                    If md.Features.MiscDifferences.Count > 0 Then Return md.Features.MiscDifferences

                Case = StandardFeatureKeys.MiscFeature
                    If md.Features.MiscFeatures.Count > 0 Then Return md.Features.MiscFeatures

                Case = StandardFeatureKeys.MiscRecombination
                    If md.Features.MiscRecombinations.Count > 0 Then Return md.Features.MiscRecombinations

                Case = StandardFeatureKeys.MiscRna
                    If md.Features.MiscRNAs.Count > 0 Then Return md.Features.MiscRNAs

                Case = StandardFeatureKeys.MiscSignal
                    If md.Features.MiscSignals.Count > 0 Then Return md.Features.MiscSignals

                Case = StandardFeatureKeys.MiscStructure
                    If md.Features.MiscStructures.Count > 0 Then Return md.Features.MiscStructures

                Case = StandardFeatureKeys.ModifiedBase
                    If md.Features.ModifiedBases.Count > 0 Then Return md.Features.ModifiedBases

                Case = StandardFeatureKeys.NonCodingRna
                    If md.Features.NonCodingRNAs.Count > 0 Then Return md.Features.NonCodingRNAs

                Case = StandardFeatureKeys.OperonRegion
                    If md.Features.OperonRegions.Count > 0 Then Return md.Features.OperonRegions

                Case = StandardFeatureKeys.PolyASignal
                    If md.Features.PolyASignals.Count > 0 Then Return md.Features.PolyASignals

                Case = StandardFeatureKeys.PolyASite
                    If md.Features.PolyASites.Count > 0 Then Return md.Features.PolyASites

                Case = StandardFeatureKeys.PrecursorRna
                    If md.Features.PrecursorRNAs.Count > 0 Then Return md.Features.PrecursorRNAs

                Case = StandardFeatureKeys.Promoter
                    If md.Features.Promoters.Count > 0 Then Return md.Features.Promoters

                Case = StandardFeatureKeys.ProteinBindingSite
                    If md.Features.ProteinBindingSites.Count > 0 Then Return md.Features.ProteinBindingSites

                Case = StandardFeatureKeys.RepeatRegion
                    If md.Features.RepeatRegions.Count > 0 Then Return md.Features.RepeatRegions

                Case = StandardFeatureKeys.ReplicationOrigin
                    If md.Features.ReplicationOrigins.Count > 0 Then Return md.Features.ReplicationOrigins

                Case = StandardFeatureKeys.RibosomalRna
                    If md.Features.RibosomalRNAs.Count > 0 Then Return md.Features.RibosomalRNAs

                Case = StandardFeatureKeys.RibosomeBindingSite
                    If md.Features.RibosomeBindingSites.Count > 0 Then Return md.Features.RibosomeBindingSites

                Case = StandardFeatureKeys.SignalPeptide
                    If md.Features.SignalPeptides.Count > 0 Then Return md.Features.SignalPeptides

                Case = StandardFeatureKeys.StemLoop
                    If md.Features.StemLoops.Count > 0 Then Return md.Features.StemLoops

                Case = StandardFeatureKeys.TataSignal
                    If md.Features.TATASignals.Count > 0 Then Return md.Features.TATASignals

                Case = StandardFeatureKeys.Terminator
                    If md.Features.Terminators.Count > 0 Then Return md.Features.Terminators

                Case = StandardFeatureKeys.ThreePrimeUtr
                    If md.Features.ThreePrimeUTRs.Count > 0 Then Return md.Features.ThreePrimeUTRs

                Case = StandardFeatureKeys.TransferMessengerRna
                    If md.Features.TransferMessengerRNAs.Count > 0 Then Return md.Features.TransferMessengerRNAs

                Case = StandardFeatureKeys.TransferRna
                    If md.Features.TransferRNAs.Count > 0 Then Return md.Features.TransferRNAs

                Case = StandardFeatureKeys.TransitPeptide
                    If md.Features.TransitPeptides.Count > 0 Then Return md.Features.TransitPeptides

                Case = StandardFeatureKeys.UnsureSequenceRegion
                    If md.Features.UnsureSequenceRegions.Count > 0 Then Return md.Features.UnsureSequenceRegions

                Case = StandardFeatureKeys.Variation
                    If md.Features.Variations.Count > 0 Then Return md.Features.Variations
                Case Else
                    Dim res = From x In md.Features.All Where x.Key = Type

                    If res.Count > 0 Then Return res.ToList

                    Return New List(Of FeatureItem)
            End Select
            Return Nothing
        End Function

    End Class

End Class

Public Class UniqueDistict
    Public Shared Property LociBuilder As New LocationBuilder

    Public Shared Function Get_Same_Features_By_Locations(First As List(Of FeatureItem), Second As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out As New List(Of FeatureItem)
        For Each f In First
            Dim Sames = From x In Second Where f.Location.LocationStart = x.Location.LocationStart And
                                                         f.Location.LocationEnd = x.Location.LocationEnd And
                                                         f.Location.Operator = x.Location.Operator And
                                             f.Location.SubLocations.Count = x.Location.SubLocations.Count

            If Sames.Count > 0 Then
                out.Add(f)
            End If
        Next
        Return out
    End Function
    Public Shared Function Get_Unique_Features_By_Locations(First As List(Of FeatureItem), Second As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out As New List(Of FeatureItem)
        For Each f In First
            Dim Sames = From x In Second Where f.Location.LocationStart = x.Location.LocationStart And
                                                         f.Location.LocationEnd = x.Location.LocationEnd And
                                                          f.Location.Operator = x.Location.Operator And
                                             f.Location.SubLocations.Count = x.Location.SubLocations.Count

            If Sames.Count = 0 Then
                out.Add(f)
            End If
        Next
        Return out
    End Function

    Public Shared Iterator Function GetDuplicatedFeaturesByLocusTagAndLocation(Features As List(Of FeatureItem)) As IEnumerable(Of List(Of FeatureItem))
        Dim LociBuilder As New LocationBuilder
        If IsNothing(Features) = False Then
            Dim t = From x In Features Select x Group By Loc = LociBuilder.GetLocationString(x.Location) & x.Qualifiers(StandardQualifierNames.LocusTag).First Into Group
            For Each g In t
                If g.Group.Count > 1 Then
                    Yield g.Group.ToList
                End If

            Next
        End If


    End Function

    Public Shared Iterator Function GetDuplicatedFeaturesByLocusTag(Features As List(Of FeatureItem)) As IEnumerable(Of List(Of FeatureItem))
        If IsNothing(Features) = False Then
            Dim t = From x In Features Select x Group By x.Qualifiers(StandardQualifierNames.LocusTag).First Into Group
            For Each g In t
                If g.Group.Count > 1 Then
                    Yield g.Group.ToList
                End If

            Next
        End If

    End Function


End Class

Public Class Qulifiers
    Public Shared Sub Add()

    End Sub
    Public Shared Function Get_Distinct_Qulifiers(seqs As List(Of Bio.ISequence)) As List(Of String)
        Dim Feats = GetFeaturesByType.Get_All_Features(seqs)
        Dim out As New List(Of String)
        For Each f In Feats
            For Each q In f.Qualifiers
                If out.Contains(q.Key) = False Then out.Add(q.Key)
            Next
        Next
        Return out
    End Function
    Public Shared Function Get_All_Values(Feat As FeatureItem) As String
        Dim satr As New System.Text.StringBuilder
        For Each Q In StandardQualifierNames.All
            If Feat.Qualifiers.ContainsKey(Q) = True AndAlso Feat.Qualifiers(Q).Count > 0 Then
                satr.Append(Feat.Qualifiers(Q).First)
            End If
            satr.Append(vbTab)
        Next
        satr.Length -= 1
        Return satr.ToString
    End Function
    Public Shared Function Get_Values_From_Features(Feats As List(Of FeatureItem), Type As String) As List(Of String)
        Dim out As New List(Of String)
        For Each Feat In Feats
            If Feat.Qualifiers.ContainsKey(Type) Then
                out.AddRange(Feat.Qualifiers(Type))
            Else
                Dim alf As Int16 = 54
            End If
        Next
        Return out
    End Function
    Public Shared Function Get_Values_From_Features(Feats As List(Of FeatureItem), Types As List(Of String)) As List(Of String)
        Dim out As New List(Of String)
        For Each Feat In Feats
            out.Add(Get_Values_From_Feature(Feat, Types))
        Next
        Return out
    End Function
    Public Shared Function Get_Values_From_Feature(Feat As FeatureItem, Type As String, Optional OnlyFirst As Boolean = False) As String
        Dim out As New List(Of String)
        If Feat.Qualifiers.ContainsKey(Type) Then
            If OnlyFirst = True Then
                out.Add(Feat.Qualifiers(Type).First)
            Else
                out.AddRange(Feat.Qualifiers(Type))
            End If

        End If
        Return Szunyi.Common.Text.General.GetText(out, " ")
    End Function

    Public Shared Function Get_Values_From_Feature(Feat As FeatureItem, Types As List(Of String), Optional OnlyFirst As Boolean = False) As String
        Dim out As New List(Of String)
        For Each Type In Types
            If Feat.Qualifiers.ContainsKey(Type) Then
                If OnlyFirst = True Then
                    out.Add(Feat.Qualifiers(Type).First)
                Else
                    out.AddRange(Feat.Qualifiers(Type))
                End If
            End If
        Next

        Return Szunyi.Common.Text.General.GetText(out, " ")
    End Function

    Public Shared Sub Rename_Qulifier_Value(Feat As FeatureItem, Key As String, original_String As String, new_String As String)
        If Feat.Qualifiers.ContainsKey(Key) = True Then
            Dim ls = Feat.Qualifiers(Key).ToList
            Dim nLS As New List(Of String)
            For Each s In ls
                s = s.Replace(original_String, new_String)
                nLS.Add(s)
            Next
            Feat.Qualifiers(Key) = nLS
        End If
    End Sub

    Public Shared Function Get_Description(feat As FeatureItem, get_Description_Gene_Note() As String, OnlyFirst As Boolean, wKey As Boolean, wLabel As Boolean) As String
        Dim str As New System.Text.StringBuilder
        If wKey = True Then str.Append(feat.Key).Append(vbTab)
        For Each item In get_Description_Gene_Note
            If feat.Qualifiers.ContainsKey(item) AndAlso feat.Qualifiers(item).Count > 0 Then
                str.Append(feat.Qualifiers(item).First).Append(vbTab)
                If OnlyFirst = True Then
                    str.Length -= 1
                    Dim s1 As String = str.ToString.Replace(Chr(34), "")
                    Return s1
                    Return str.ToString
                End If
            End If
        Next
        If wLabel = True Then str.Append(feat.Label)

        Dim s As String = str.ToString.Replace(Chr(34), "")
        Return s
    End Function

    Friend Shared Function Get_Common_Values(feature As FeatureItem, Separator As String) As String
        Dim str As New System.Text.StringBuilder
        If feature.Qualifiers.ContainsKey(StandardQualifierNames.GeneSymbol) Then
            str.Append(feature.Qualifiers(StandardQualifierNames.GeneSymbol).First).Append(",")
        End If
        If feature.Qualifiers.ContainsKey(StandardQualifierNames.Note) Then
            'str.Append(feature.Qualifiers(StandardQualifierNames.Note).First).Append(",")
        End If
        If feature.Qualifiers.ContainsKey(StandardQualifierNames.LocusTag) Then
            str.Append(feature.Qualifiers(StandardQualifierNames.LocusTag).First).Append(",")
        End If
        Dim s As String = str.ToString.Replace(Chr(34), "")
        s = s.Replace(",,", ",").Replace(vbTab, " ").Replace(vbCrLf, " ")

        Return s
    End Function

    Public Shared Sub Add(f As FeatureItem, Qual_Name As String, Qual_Value As String)
        Dim ls As New List(Of String)
        ls.Add(Qual_Value)
        If f.Qualifiers.ContainsKey(Qual_Name) = False Then
            f.Qualifiers.Add(Qual_Name, ls)
        Else
            f.Qualifiers(Qual_Name).Add(Qual_Value)
        End If
    End Sub
    Public Shared Sub Add(f As FeatureItem, Qual_Name As String, Qual_Values As List(Of String))

        If f.Qualifiers.ContainsKey(Qual_Name) = False Then
            f.Qualifiers.Add(Qual_Name, Qual_Values)
        Else
            f.Qualifiers(Qual_Name) = Qual_Values
        End If
    End Sub
End Class

Public Class SettingForSearchInQulifier
    Public Property QulifierName As String
    Public Property Type As Szunyi.Common.Enums.SearchType
    Public Property InterestingStrings As New List(Of String)


    Public Sub New(qualName As String, Type As String, IntStrings As List(Of String))
        Me.QulifierName = qualName
        Select Case Type
            Case "Contains"
                Me.Type = Szunyi.Common.Enums.SearchType.Contains
            Case "Exact"
                Me.Type = Szunyi.Common.Enums.SearchType.Exact
            Case "No Value"
                Me.Type = Szunyi.Common.Enums.SearchType.NoValue
        End Select
        Me.InterestingStrings = IntStrings
    End Sub


End Class

Public Class GetFeatureByQualifier

    Public Shared Function GetFeaturesByQulifiersPerfect(t As List(Of FeatureItem),
                                                                             searchSetting As SettingForSearchInQulifier)
        Dim out As New List(Of FeatureItem)

        For Each s In searchSetting.InterestingStrings
            Dim g = From x In t Where x.Qualifiers.ContainsKey(searchSetting.QulifierName) AndAlso
                                                    String.Compare(x.Qualifiers(searchSetting.QulifierName).First, s, StringComparison.InvariantCultureIgnoreCase)
            If g.Count > 0 Then out.AddRange(g.ToList)
        Next


        Return out
    End Function
    Public Shared Function GetFeaturesByQulifiersContains(t As List(Of FeatureItem),
                                                                      searchSetting As SettingForSearchInQulifier)
        Dim out As New List(Of FeatureItem)

        For Each s In searchSetting.InterestingStrings
            Dim g = From x In t Where x.Qualifiers.ContainsKey(searchSetting.QulifierName) AndAlso
                                                    x.Qualifiers(searchSetting.QulifierName).First.IndexOf(s, StringComparison.InvariantCultureIgnoreCase) > -1
            If g.Count > 0 Then out.AddRange(g.ToList)
        Next


        Return out
    End Function
    Public Shared Function GetFeaturesByNoValues(t As List(Of FeatureItem),
                                                                      searchSetting As SettingForSearchInQulifier)
        Dim out As New List(Of FeatureItem)


        Dim g = From x In t Where x.Qualifiers.ContainsKey(searchSetting.QulifierName) = False OrElse
                                                    x.Qualifiers(searchSetting.QulifierName).Count = 0
        If g.Count > 0 Then out.AddRange(g.ToList)

        Return out
    End Function

    Public Shared Function GetShortLocusTag(FeatItem As FeatureItem) As String
        Return Split(GetLocusTag(FeatItem), ".").First
    End Function
    Public Shared Function GetLocusTag(FeatItem As FeatureItem) As String
        If FeatItem.Qualifiers.ContainsKey(StandardQualifierNames.LocusTag) Then
            Return FeatItem.Qualifiers(StandardQualifierNames.LocusTag).First
        Else
            Return String.Empty
        End If

    End Function
End Class


