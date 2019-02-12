Imports System.Runtime.CompilerServices
Imports Bio.IO.GenBank
Imports Szunyi.Common
Imports Szunyi.Common.Extensions
Imports Szunyi.Location
Imports Szunyi.Sequences.Extensions
Public Module Extensions
    Dim LociBuilder As New Bio.IO.GenBank.LocationBuilder
#Region "GenBankMetadata"
    ''' <summary>
    ''' Create Default GenBankMetaData as unknown Organism and FeatureList
    ''' </summary>
    ''' <param name="Seq"></param>
    <Extension()>
    Public Sub CreateGenBankMetadata(Seq As Bio.ISequence)
        If Seq.Has_GenBank_Metadata = False Then
            Dim x As New Bio.IO.GenBank.GenBankMetadata
            x.Locus = New Bio.IO.GenBank.GenBankLocusInfo()
            x.Locus.Date = Now
            If Seq.Alphabet Is Bio.Alphabets.DNA Then
                x.Locus.MoleculeType = Bio.IO.GenBank.MoleculeType.DNA
            ElseIf Seq.Alphabet Is Bio.Alphabets.AmbiguousDNA Then
                x.Locus.MoleculeType = Bio.IO.GenBank.MoleculeType.DNA
            ElseIf Seq.Alphabet Is Bio.Alphabets.RNA Then
                x.Locus.MoleculeType = Bio.IO.GenBank.MoleculeType.RNA
            ElseIf Seq.Alphabet Is Bio.Alphabets.AmbiguousRNA Then
                x.Locus.MoleculeType = Bio.IO.GenBank.MoleculeType.RNA
            ElseIf Seq.Alphabet Is Bio.Alphabets.Protein Then
                x.Locus.MoleculeType = Bio.IO.GenBank.MoleculeType.Protein
            ElseIf Seq.Alphabet Is Bio.Alphabets.AmbiguousProtein Then
                x.Locus.MoleculeType = Bio.IO.GenBank.MoleculeType.Protein
            End If

            x.Locus.Name = Seq.ID
            x.Locus.SequenceLength = Seq.Count
            x.Locus.StrandTopology = Bio.IO.GenBank.SequenceStrandTopology.Linear
            x.Accession = New GenBankAccession()
            x.Accession.Primary = Seq.ID
            x.Source = New Bio.IO.GenBank.SequenceSource
            x.Source.CommonName = "Unknown."
            x.Source.Organism = New Bio.IO.GenBank.OrganismInfo
            x.Source.Organism.Species = "Unknown"
            x.Source.Organism.Genus = "Unknown."
            x.Features = New SequenceFeatures
            Seq.Metadata.Add(Bio.Util.Helper.GenBankMetadataKey, x)
        End If
    End Sub
    ''' <summary>
    ''' Return True If Seq has GenBanMetaDataKey and that is not Nothing
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function Has_GenBank_Metadata(Seq As Bio.ISequence) As Boolean
        If Seq.Metadata.ContainsKey(Bio.Util.Helper.GenBankMetadataKey) = True Then
            If IsNothing(Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey)) = False Then
                Return True
            Else
                Return False
            End If
        Else
            Return False
        End If
    End Function
    ''' <summary>
    ''' Return True If Seq has GenBanMetaDataKey and that is not Nothing and metadata.Features is not nothing
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function Has_GenBank_Features(Seq As Bio.ISequence) As Boolean
        If Seq.Has_GenBank_Metadata = False Then Return False
        Dim md As Bio.IO.GenBank.GenBankMetadata = Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey)
        If IsNothing(md.Features) = True Then Return Nothing
        Return True
    End Function
    ''' <summary>
    ''' Return GenBankMetaData or Nothing
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function GenBankMetaData(Seq As Bio.ISequence) As Bio.IO.GenBank.GenBankMetadata
        If Seq.Has_GenBank_Metadata = False Then Return Nothing
        Dim md As Bio.IO.GenBank.GenBankMetadata = Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey)
        Return md
    End Function
    ''' <summary>
    ''' Itarate Every GenBnakMetaData From Seqs, somteimes nothing
    ''' </summary>
    ''' <param name="Seqs"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function GenBankMetaData(Seqs As IEnumerable(Of Bio.ISequence)) As IEnumerable(Of Bio.IO.GenBank.GenBankMetadata)
        For Each Seq In Seqs
            Yield Seq.GenBankMetaData
        Next
    End Function

    ''' <summary>
    ''' Return Name, Length, Sequence Type, Strand Type
    ''' </summary>
    ''' <param name="Seqs"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function GenBankBasicData(Seqs As IEnumerable(Of Bio.ISequence)) As String

        Dim out As New List(Of String)
        Dim Header As New System.Text.StringBuilder
        Header.Append("Name").Append(vbTab)
        Header.Append("Length").Append(vbTab)
        Header.Append("Seq Type").Append(vbTab)
        Header.Append("Seq strand").Append(vbTab)
        Header.Append("Common Name").Append(vbTab)
        Header.Append("Strain").Append(vbTab)
        For Each Seq In Seqs
            Dim md = Seq.GenBankMetaData
            If IsNothing(md) = False Then
                Dim str As New System.Text.StringBuilder
                str.Append(md.Locus.Name).Append(vbTab)
                str.Append(md.Locus.SequenceLength).Append(vbTab)
                str.Append(Bio.IO.GenBank.GenBankLocusTokenParser.LocusConstants.AlphabetTypes(md.Locus.MoleculeType)).Append(vbTab)
                str.Append(Bio.IO.GenBank.GenBankLocusTokenParser.LocusConstants.SequenceStrandTypes(md.Locus.Strand)).Append(vbTab)
                str.Append(Seq.CommonName).Append(vbTab)
                str.Append(Seq.Strain)

                Dim s = md.Source
                str.Append(s.CommonName).Append(vbTab)

                Dim feats = Seq.Get_Features("source")
                If feats.Count <> 0 Then
                    Dim Q = feats.First.Get_All_Qualifier_Values()
                    str.Append(Q)
                End If
                out.Add(str.ToString)
            End If
        Next
        Return out.GetText(vbCrLf)
    End Function


#End Region

#Region "Exon Intron"
    ''' <summary>
    ''' Iterator Exon Locations From Feature
    ''' </summary>
    ''' <param name="Feat"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_Exons(Feat As Bio.IO.GenBank.FeatureItem) As IEnumerable(Of ILocation)
        Dim LociBuilder As New Bio.IO.GenBank.LocationBuilder

        If Feat.Location.SubLocations.Count = 0 Then ' No complement no join
            Yield Feat.Location
        ElseIf Feat.Location.Operator <> LocationOperator.Complement Then ' no complement join
            For Each exon In Feat.Location.SubLocations
                Yield exon
            Next
        ElseIf Feat.Location.SubLocations.First.Operator = LocationOperator.Join Then ' complement join
            For Each loci As Bio.IO.GenBank.Location In Feat.Location.SubLocations.First.SubLocations
                Dim s = "complement(" & LociBuilder.GetLocationString(loci) & ")"
                Yield LociBuilder.GetLocation(s)
            Next

        Else ' complement no join
            Yield Feat.Location
        End If


    End Function
    ''' <summary>
    ''' Iterator Intron Locations From Feature
    ''' </summary>
    ''' <param name="Feat"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_Introns(Feat As FeatureItem) As IEnumerable(Of ILocation)
        Dim Exons = Feat.Get_Exons.ToList
        For Each Intron In GetIntronLocationsFromExonLOcations(Exons)
            Yield Intron
        Next
    End Function
    ''' <summary>
    ''' Iterator Exon Locations From Sequnce 
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_Exons(Seq As Bio.ISequence) As IEnumerable(Of ILocation)
        Dim LociBuilder As New Bio.IO.GenBank.LocationBuilder
        For Each Feat In Seq.Get_All_Features()
            For Each Exon In Feat.Get_Exons
                Yield Exon
            Next
        Next
    End Function
    ''' <summary>
    ''' Iterator Intron Locations From Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_Introns(Seq As Bio.ISequence) As IEnumerable(Of ILocation)
        For Each Feat In Seq.Get_All_Features()
            For Each Intron In Feat.Get_Introns
                Yield Intron
            Next
        Next
    End Function

    Private Iterator Function GetIntronLocationsFromExonLOcations(Ls As List(Of Bio.IO.GenBank.ILocation)) As IEnumerable(Of Bio.IO.GenBank.ILocation)
        For i1 = 0 To Ls.Count - 2
            If Ls.First.IsComplementer = True Then
                '   Change_Strand(out.Last)
                If Ls(i1).LocationEnd > Ls(i1 + 1).LocationStart Then
                    Yield (LociBuilder.GetLocation("complement(" & Ls(i1 + 1).LocationEnd + 1 & ".." & Ls(i1).LocationStart - 1 & ")"))
                Else
                    Yield (LociBuilder.GetLocation("complement(" & Ls(i1).LocationEnd + 1 & ".." & Ls(i1 + 1).LocationStart - 1 & ")"))
                End If
            Else
                If Ls(i1).LocationEnd > Ls(i1 + 1).LocationStart Then
                    Yield (LociBuilder.GetLocation(Ls(i1 + 1).LocationEnd + 1 & ".." & Ls(i1).LocationStart - 1))
                Else
                    Yield (LociBuilder.GetLocation(Ls(i1).LocationEnd + 1 & ".." & Ls(i1 + 1).LocationStart - 1))
                End If

            End If
        Next

    End Function

    ''' <summary>
    ''' Return True If passed the GT-AG rule
    ''' </summary>
    ''' <param name="loci"></param>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function IsIntronGTAT(loci As Bio.IO.GenBank.ILocation, Seq As Bio.ISequence) As Boolean
        If loci.IsComplementer = True Then
            If Bio.Alphabets.DNA.CompareSymbols(Seq(loci.TSS - 1), Bio.Alphabets.DNA.C) = True AndAlso
                    Bio.Alphabets.DNA.CompareSymbols(Seq(loci.TSS - 2), Bio.Alphabets.DNA.A) = True AndAlso
                    Bio.Alphabets.DNA.CompareSymbols(Seq(loci.PAS - 1), Bio.Alphabets.DNA.C) = True AndAlso
                    Bio.Alphabets.DNA.CompareSymbols(Seq(loci.PAS), Bio.Alphabets.DNA.T) = True Then
                Return True

            Else
                Return False
            End If
        Else
            If Bio.Alphabets.DNA.CompareSymbols(Seq(loci.TSS - 1), Bio.Alphabets.DNA.G) = True AndAlso
                    Bio.Alphabets.DNA.CompareSymbols(Seq(loci.TSS), Bio.Alphabets.DNA.T) = True AndAlso
                    Bio.Alphabets.DNA.CompareSymbols(Seq(loci.PAS - 2), Bio.Alphabets.DNA.A) = True AndAlso
                    Bio.Alphabets.DNA.CompareSymbols(Seq(loci.PAS - 1), Bio.Alphabets.DNA.G) = True Then
                Return True
            Else
                Return False
            End If
        End If
    End Function
    ''' <summary>
    ''' Return Number Of Exon in a Feature
    ''' </summary>
    ''' <param name="Feat"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function NofExon(Feat As FeatureItem)
        Return Feat.Location.NofExon
    End Function
    ''' <summary>
    ''' Return Number Of Exon in a Location
    ''' </summary>
    ''' <param name="Loci"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function NofExon(Loci As Bio.IO.GenBank.ILocation)
        If Loci.Operator = LocationOperator.Complement Then
            If Loci.SubLocations.First.Operator = LocationOperator.Join Then
                Return Loci.SubLocations.First.SubLocations.Count
            Else
                Return 1
            End If
        Else ' No Complement
            If Loci.Operator = LocationOperator.Join Then
                Return Loci.SubLocations.Count
            Else
                Return 1
            End If
        End If
    End Function
    ''' <summary>
    ''' Return Number of Intron in a Featue
    ''' </summary>
    ''' <param name="Feat"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function NofIntron(Feat As FeatureItem)
        Return Feat.Location.NofIntron
    End Function
    '''<summary>
    ''' Return Number of Intron in a Loacation
    ''' </summary>
    ''' <param name="Loci"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function NofIntron(Loci As Bio.IO.GenBank.ILocation)
        Return Loci.NofExon - 1
    End Function

#End Region

    ''' <summary>
    ''' Return in order LocusTag,GeneSymbol,Product,Note,Label or Empty String chr(34) is trimmed
    ''' </summary>
    ''' <param name="Feat"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function CommonName(Feat As FeatureItem) As String
        Dim s As String = ""
        If Feat.Qualifiers.ContainsKey(StandardQualifierNames.LocusTag) AndAlso Feat.Qualifiers(StandardQualifierNames.LocusTag).Count > 0 Then
            s = (Feat.Qualifiers(StandardQualifierNames.LocusTag).First)
        ElseIf Feat.Qualifiers.ContainsKey(StandardQualifierNames.GeneSymbol) AndAlso Feat.Qualifiers(StandardQualifierNames.GeneSymbol).Count > 0 Then
            s = Feat.Qualifiers(StandardQualifierNames.GeneSymbol).First
        ElseIf Feat.Qualifiers.ContainsKey(StandardQualifierNames.Product) AndAlso Feat.Qualifiers(StandardQualifierNames.Product).Count > 0 Then
            s = Feat.Qualifiers(StandardQualifierNames.Product).First
        ElseIf Feat.Qualifiers.ContainsKey(StandardQualifierNames.Note) AndAlso Feat.Qualifiers(StandardQualifierNames.Note).Count > 0 Then
            s = Feat.Qualifiers(StandardQualifierNames.Note).First
        ElseIf Feat.Label <> "" Then
            s = Feat.Label
        End If

        Return s.Trim(Chr(34))
    End Function

#Region "Get Features"
    ''' <summary>
    ''' Yield every Canonical Features using StandardQulifiers From Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_All_Canonical_Features(Seq As Bio.ISequence) As IEnumerable(Of FeatureItem)
        If Seq.Has_GenBank_Features = True Then
            For Each Feat In Seq.Get_Features(StandardFeatureKeys.All)
                Yield Feat
            Next
        End If
    End Function
    ''' <summary>
    ''' Yield every Canonical Features using StandardQulifiers From Sequences
    ''' </summary>
    ''' <param name="Seqs"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_All_Canonical_Features(Seqs As IEnumerable(Of Bio.ISequence)) As IEnumerable(Of FeatureItem)
        For Each Seq In Seqs
            For Each Feat In Seq.Get_All_Canonical_Features
                Yield Feat
            Next
        Next

    End Function
    ''' <summary>
    ''' Yield every Features also non-canonicals from Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_All_Features(Seq As Bio.ISequence) As IEnumerable(Of FeatureItem)
        If Seq.Has_GenBank_Features = True Then
            For Each Feat In Seq.GenBankMetaData.Features.All
                Yield Feat
            Next
        End If
    End Function
    ''' <summary>
    ''' Yield every Features also non-canonicals from Sequences
    ''' </summary>
    ''' <param name="Seqs"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_All_Features(Seqs As IEnumerable(Of Bio.ISequence)) As IEnumerable(Of FeatureItem)
        For Each Seq In Seqs
            For Each Feat In Seq.Get_All_Features
                Yield Feat
            Next
        Next
    End Function

    ''' <summary>
    ''' Yield Every Features with User-Defined Keys from Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_Features(Seq As Bio.ISequence, Keys As IEnumerable(Of String)) As IEnumerable(Of FeatureItem)
        For Each Key In Keys
            For Each Feat In Get_Features(Seq, Key)
                Yield Feat
            Next
        Next
    End Function
    ''' <summary>
    ''' Yield Every Features with User-Defined Keys from Sequences
    ''' </summary>
    ''' <param name="Seqs"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_Features(Seqs As IEnumerable(Of Bio.ISequence), Keys As IEnumerable(Of String)) As IEnumerable(Of FeatureItem)
        For Each Seq In Seqs
            For Each Key In Keys
                For Each Feat In Get_Features(Seq, Key)
                    Yield Feat
                Next
            Next
        Next
    End Function
    ''' <summary>
    ''' Yield Every Features with User-Defined Key from Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function Get_Features(Seq As Bio.ISequence, Key As String) As IEnumerable(Of FeatureItem)
        If Seq.Has_GenBank_Features = True Then
            Dim md = Seq.GenBankMetaData
            Select Case Key

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
                    Dim res = From x In md.Features.All Where x.Key = Key

                    If res.Count > 0 Then Return res.ToList

                    Return New List(Of FeatureItem)
            End Select
        End If
        Return New List(Of FeatureItem)
    End Function

    ''' <summary>
    ''' Yield Every Features with User-Defined Key from Sequences
    ''' </summary>
    ''' <param name="Seqs"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function Get_Features(Seqs As IEnumerable(Of Bio.ISequence), Key As String) As IEnumerable(Of FeatureItem)
        For Each Seq In Seqs
            For Each Feat In Seq.Get_Features(Key)
                Yield Feat
            Next
        Next
    End Function

#End Region

#Region "Qualifiers"
    ''' <summary>
    ''' Maintain only distinct lines in Qualifiers
    ''' </summary>
    ''' <param name="Feat"></param>
    <Extension()>
    Public Sub Distinct(Feat As FeatureItem)
        Dim Keys = Feat.QualifierKeys.ToList
        For Each Key In Keys
            Dim s = Feat.Qualifiers(Key).Distinct
            If s.Count <> Feat.Qualifiers(Key).Count Then
                Feat.Qualifiers(Key) = s.ToList
            End If
        Next
    End Sub



    ''' <summary>
    ''' Enumerate all Feature Key where Feature Has Not Nothing andAlso Contain Values
    ''' </summary>
    ''' <param name="Feat"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function QualifierKeys(Feat As FeatureItem) As IEnumerable(Of String)
        For Each Item In From x In Feat.Qualifiers Select x Where IsNothing(x.Value) = False AndAlso x.Value.Count > 0
            Yield Item.Key
        Next
    End Function
    <Extension()>
    Public Function Get_All_Qualifier_Values(Feat As FeatureItem, Optional Separator As String = vbTab) As String
        Dim str As New System.Text.StringBuilder
        For Each Q In StandardQualifierNames.All
            If Feat.Qualifiers.ContainsKey(Q) = True AndAlso Feat.Qualifiers(Q).Count > 0 Then
                str.Append(Feat.Qualifiers(Q).GetText(" "))
            End If
            str.Append(Separator)
        Next
        str.Length -= Separator.Length
        Return str.ToString
    End Function

    <Extension()>
    Public Function Get_Qulifier_Values(Feat As FeatureItem, Type As String, Optional Separator As String = vbTab) As String
        If Feat.Qualifiers.ContainsKey(Type) = True AndAlso Feat.Qualifiers(Type).Count > 0 Then
            Return Feat.Qualifiers(Type).GetText(" ")
        Else
            Return String.Empty
        End If
    End Function

    <Extension()>
    Public Function Get_Qulifier_Values(Feat As FeatureItem, Types As IEnumerable(Of String), Optional Separator As String = vbTab) As String
        Dim str As New System.Text.StringBuilder
        For Each Type In Types
            If Feat.Qualifiers.ContainsKey(Type) = True AndAlso Feat.Qualifiers(Type).Count > 0 Then
                str.Append(Feat.Qualifiers(Type).GetText(" "))
            End If
            str.Append(Separator)

        Next
        str.Length -= Separator.Length
        Return str.ToString
    End Function

    <Extension()>
    Public Iterator Function Get_All_Qulifier_Values(Feats As IEnumerable(Of FeatureItem), Optional Separator As String = vbTab) As IEnumerable(Of String)
        Dim str As New System.Text.StringBuilder
        For Each Feat In Feats
            For Each Q In StandardQualifierNames.All
                If Feat.Qualifiers.ContainsKey(Q) = True AndAlso Feat.Qualifiers(Q).Count > 0 Then
                    str.Append(Feat.Qualifiers(Q).GetText(" "))
                End If
                str.Append(Separator)
            Next
            str.Length -= Separator.Length
            Yield str.ToString
        Next

    End Function

    <Extension()>
    Public Iterator Function Get_Qulifier_Values(Feats As IEnumerable(Of FeatureItem), Type As String, Optional Separator As String = vbTab) As IEnumerable(Of String)
        For Each Feat In Feats
            If Feat.Qualifiers.ContainsKey(Type) = True AndAlso Feat.Qualifiers(Type).Count > 0 Then
                Yield Feat.Qualifiers(Type).GetText(Separator)
            Else
                Yield String.Empty
            End If
        Next
    End Function

    <Extension()>
    Public Iterator Function Get_Qulifier_Values(Feats As IEnumerable(Of FeatureItem), Types As IEnumerable(Of String), Optional Separator As String = vbTab) As IEnumerable(Of String)

        For Each Feat In Feats
            Dim str As New System.Text.StringBuilder
            For Each Type In Types
                If Feat.Qualifiers.ContainsKey(Type) = True AndAlso Feat.Qualifiers(Type).Count > 0 Then
                    str.Append(Feat.Qualifiers(Type).GetText(" "))
                End If
                str.Append(Separator)
            Next
            str.Length -= Separator.Length
            Yield str.ToString
        Next

    End Function

    <Extension()>
    Public Iterator Function DistinctQualifiers(Seq As Bio.ISequence) As IEnumerable(Of String)
        Dim out As New List(Of String)
        For Each f In Seq.Get_All_Features
            For Each q In f.Qualifiers
                If out.Contains(q.Key) = False Then
                    out.Add(q.Key)
                    Yield q.Key
                End If
            Next
        Next
    End Function
    <Extension()>
    Public Iterator Function DistinctQualifiers(Seqs As IEnumerable(Of Bio.ISequence)) As IEnumerable(Of String)
        Dim out As New List(Of String)
        For Each f In Seqs.Get_All_Features
            For Each q In f.Qualifiers
                If out.Contains(q.Key) = False Then
                    out.Add(q.Key)
                    Yield q.Key
                End If
            Next
        Next

    End Function

    <Extension()>
    Public Sub Add_Qualfier(Feat As FeatureItem, QualifierKey As String, QualifierValue As String)
        Dim ls As New List(Of String)
        ls.Add(QualifierValue)
        If Feat.Qualifiers.ContainsKey(QualifierKey) = False Then
            Feat.Qualifiers.Add(QualifierKey, ls)
        Else
            Feat.Qualifiers(QualifierKey).AddRange(ls)
        End If
    End Sub
    <Extension()>
    Public Sub Add_Or_Replace_Qualfier(Feat As FeatureItem, QualifierKey As String, QualifierValue As String)
        Dim ls As New List(Of String)
        ls.Add(QualifierValue)
        If Feat.Qualifiers.ContainsKey(QualifierKey) = False Then
            Feat.Qualifiers.Add(QualifierKey, ls)
        Else
            Feat.Qualifiers(QualifierKey) = ls
        End If
    End Sub
    <Extension()>
    Public Sub Add_Qualfier(Feat As FeatureItem, QualifierKey As String, QualifierValues As IEnumerable(Of String))
        Dim ls = QualifierValues.RemoveEmpty
        If Feat.Qualifiers.ContainsKey(QualifierKey) = False Then
            Feat.Qualifiers.Add(QualifierKey, ls.ToList)
        Else
            Feat.Qualifiers(QualifierKey).AddRange(ls)
        End If
    End Sub
    <Extension()>
    Public Sub ReName_Qualfier_Value(Feat As FeatureItem, QualifierKey As String, Original_String As String, New_String As String)
        If Feat.Qualifiers.ContainsKey(QualifierKey) = True Then
            Dim ls = Feat.Qualifiers(QualifierKey).ToList
            Dim nLS As New List(Of String)
            For Each s In ls
                s = s.Replace(Original_String, New_String)
                nLS.Add(s)
            Next
            Feat.Qualifiers(QualifierKey) = nLS
        End If
    End Sub
#End Region

#Region "Delete Keys, Remove Features"
    ''' <summary>
    ''' Remove a User-Defined Feature From Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <param name="Feat"></param>
    <Extension()>
    Public Sub Delete(Seq As Bio.ISequence, Feat As FeatureItem)
        If Seq.Has_GenBank_Metadata = True Then
            Dim Md = Seq.GenBankMetaData
            Md.Features.All.Remove(Feat)
        End If
    End Sub
    ''' <summary>
    ''' Remove User-Defined Features From Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <param name="Feats"></param>
    <Extension()>
    Public Sub Delete(Seq As Bio.ISequence, Feats As IEnumerable(Of FeatureItem))
        If Seq.Has_GenBank_Metadata = True Then
            Dim Md = Seq.GenBankMetaData
            For Each Feat In Feats
                Md.Features.All.Remove(Feat)
            Next
        End If
    End Sub
    ''' <summary>
    ''' Remove All Features from a Sequence With User-Defined Key
    ''' </summary>
    ''' <param name="seq"></param>
    ''' <param name="Key"></param>
    <Extension()>
    Public Sub Delete(seq As Bio.ISequence, Key As String)
        Dim Feats = seq.Get_Features(Key)
        Delete(seq, Feats)
    End Sub
    ''' <summary>
    ''' Remove All Features from a Sequence With User-Defined Keys
    ''' </summary>
    ''' <param name="seq"></param>
    ''' <param name="Keys"></param>
    <Extension()>
    Public Sub Delete(seq As Bio.ISequence, Keys As List(Of String))
        Dim Feats = seq.Get_Features(Keys)
        Delete(seq, Feats)
    End Sub
    ''' <summary>
    ''' Remove All Features from a Sequences With User-Defined Key
    ''' </summary>
    ''' <param name="Seqs"></param>
    ''' <param name="Key"></param>
    <Extension()>
    Public Sub Delete(seqs As IEnumerable(Of Bio.ISequence), Key As String)
        For Each Seq In seqs
            Dim Feats = Seq.Get_Features(Key)
            Delete(Seq, Feats)
        Next
    End Sub

    ''' <summary>
    ''' Remove All Features from a Sequences With User-Defined Keys
    ''' </summary>
    ''' <param name="seqs"></param>
    ''' <param name="Keys"></param>
    <Extension()>
    Public Sub Delete(seqs As IEnumerable(Of Bio.ISequence), Keys As List(Of String))
        For Each Seq In seqs
            Dim Feats = Seq.Get_Features(Keys)
            Delete(Seq, Feats)
        Next
    End Sub
#End Region

#Region "Add Feature"
    ''' <summary>
    ''' If there is No  GenBank Meta-data it will be created. 
    ''' Add a User-Defined Feature to a Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <param name="Feat"></param>
    <Extension()>
    Public Sub Add_Feature(Seq As Bio.ISequence, Feat As FeatureItem)
        If IsNothing(Seq.GenBankMetaData) = True Then Seq.CreateGenBankMetadata
        Seq.GenBankMetaData.Features.All.Add(Feat)
    End Sub
    ''' <summary>
    ''' If there is No  GenBank Meta-data it will be created. 
    ''' Add a User-Defined Features to a Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <param name="Feats"></param>
    <Extension()>
    Public Sub Add_Feature(ByRef Seq As Bio.ISequence, Feats As IEnumerable(Of FeatureItem))
        If IsNothing(Seq.GenBankMetaData) = True Then Seq.CreateGenBankMetadata
        Dim Md = Seq.GenBankMetaData
        For Each Feat In Feats
            Seq.Add_Feature(Feat)
        Next
    End Sub

    ''' <summary>
    ''' If there is No  GenBank Meta-data it will be created. 
    ''' Add a User-Defined Locations with User-Defined Feature Key to a Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <param name="Locis"></param>
    ''' <param name="Feat_key"></param>
    <Extension()>
    Public Sub Add_Feature(ByRef Seq As Bio.ISequence, Locis As IEnumerable(Of Bio.IO.GenBank.ILocation), Feat_key As String)
        If IsNothing(Seq.GenBankMetaData) = True Then Seq.CreateGenBankMetadata
        Dim Md = Seq.GenBankMetaData
        For Each Loci In Locis
            Seq.Add_Feature(Loci, Feat_key)
        Next
    End Sub
    ''' <summary>
    ''' If there is No  GenBank Meta-data it will be created. 
    ''' Add a User-Defined Location with User-Defined Feature Key to a Sequence
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <param name="Loci"></param>
    ''' <param name="Feat_key"></param>
    <Extension()>
    Public Sub Add_Feature(ByRef Seq As Bio.ISequence, Loci As Bio.IO.GenBank.ILocation, Feat_key As String)
        If IsNothing(Seq.GenBankMetaData) = True Then Seq.CreateGenBankMetadata
        Dim f As New FeatureItem(Feat_key, Loci)
        Seq.Add_Feature(f)
    End Sub
#End Region

#Region "FeatureSeq"
    ''' <summary>
    ''' Return the Sequence Of Feature in a Sequence + Orientation
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <param name="Feat"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function FeatureSeq(Seq As Bio.ISequence, Feat As FeatureItem) As Bio.ISequence
        Dim seqFeat = Feat.GetSubSequence(Seq)
        If Feat.Location.Operator = LocationOperator.Complement Then seqFeat = seqFeat.GetReversedSequence
        seqFeat.ID = Feat.CommonName.Replace(".", "_")
        Return seqFeat
    End Function

    ''' <summary>
    ''' Return the Sequences Of Features in a Sequence + Orientation
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <param name="Feats"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function FeatureSeq(Seq As Bio.ISequence, Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of Bio.ISequence)
        For Each Feat In Feats
            Yield Seq.FeatureSeq(Feat)
        Next
    End Function
    ''' <summary>
    ''' Return All Feature Sequences in + Orientation has a User-Defined Feature Key
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <param name="Key"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function FeatureSeq(Seq As Bio.ISequence, Key As String) As IEnumerable(Of Bio.ISequence)
        For Each Feat In Seq.Get_Features(Key)
            Yield Feat.GetSubSequence(Seq)
        Next
    End Function

    ''' <summary>
    ''' Return All Feature Sequences in a Sequence + Orientation has a User-Defined Feature Keys
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <param name="Keys"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function FeatureSeq(Seq As Bio.ISequence, Keys As IEnumerable(Of String)) As IEnumerable(Of Bio.ISequence)
        For Each Feat In Seq.Get_Features(Keys)
            Yield Feat.GetSubSequence(Seq)
        Next
    End Function
    ''' <summary>
    ''' Return All Feature Sequences in Sequences + Orientation has a User-Defined Feature Key
    ''' </summary>
    ''' <param name="Seqs"></param>
    ''' <param name="Key"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function FeatureSeq(Seqs As IEnumerable(Of Bio.ISequence), Key As String) As IEnumerable(Of Bio.ISequence)
        For Each Seq In Seqs
            For Each Feat In Seq.Get_Features(Key)
                Yield Feat.GetSubSequence(Seq)
            Next
        Next
    End Function
    ''' <summary>
    ''' Return All Feature Sequences in Sequences + Orientation has a User-Defined Feature Keys
    ''' </summary>
    ''' <param name="Seqs"></param>
    ''' <param name="Keys"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function FeatureSeq(Seqs As IEnumerable(Of Bio.ISequence), Keys As IEnumerable(Of String)) As IEnumerable(Of Bio.ISequence)
        For Each Seq In Seqs
            For Each Feat In Seq.Get_Features(Keys)
                Yield Feat.GetSubSequence(Seq)
            Next
        Next
    End Function
#End Region

#Region "Unique Distinct Common 1Copy Features Location,Locustag,LocationAndLocusTag"
    <Extension()>
    Public Iterator Function Group_ByLocation(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of IEnumerable(Of FeatureItem))
        Dim LociBuilder As New LocationBuilder
        Dim t = From x In Feats Select x Group By Loc = LociBuilder.GetLocationString(x.Location) Into Group

        For Each g In t
            Yield g.Group
        Next
    End Function
    <Extension()>
    Public Iterator Function Group_ByLocation_ByLocusTag(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of IEnumerable(Of FeatureItem))
        Dim LociBuilder As New LocationBuilder
        Dim t = From x In Feats Select x Group By Loc = LociBuilder.GetLocationString(x.Location) & x.Get_Qulifier_Values(StandardQualifierNames.LocusTag) Into Group
        For Each g In t
            Yield g.Group
        Next
    End Function
    <Extension()>
    Public Iterator Function Group_ByLocusTag(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of IEnumerable(Of FeatureItem))
        Dim LociBuilder As New LocationBuilder
        Dim t = From x In Feats Select x Group By Loc = x.Get_Qulifier_Values(StandardQualifierNames.LocusTag) Into Group
        For Each g In t
            Yield g.Group
        Next
    End Function

    <Extension()>
    Public Iterator Function Unique_ByLocation(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of FeatureItem)
        For Each g In Feats.Group_ByLocation
            If g.Count = 1 Then Yield g.First
        Next
    End Function
    <Extension()>
    Public Iterator Function Unique_ByLocation_ByLocusTag(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of FeatureItem)
        For Each g In Feats.Group_ByLocation_ByLocusTag
            If g.Count = 1 Then Yield g.First
        Next
    End Function
    <Extension()>
    Public Iterator Function Unique_ByLocusTag(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of FeatureItem)
        For Each g In Feats.Group_ByLocusTag
            If g.Count = 1 Then Yield g.First
        Next
    End Function

    <Extension()>
    Public Iterator Function OneCopy_ByLocation(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of FeatureItem)
        Dim LociBuilder As New LocationBuilder

        Dim t = From x In Feats Select x Group By Loc = LociBuilder.GetLocationString(x.Location) Into Group
        For Each g In t
            Yield g.Group.First
        Next
    End Function
    <Extension()>
    Public Iterator Function OneCopy_ByLocation_ByLocusTag(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of FeatureItem)
        Dim LociBuilder As New LocationBuilder

        Dim t = From x In Feats Select x Group By Loc = LociBuilder.GetLocationString(x.Location) & x.Get_Qulifier_Values(StandardQualifierNames.LocusTag) Into Group
        For Each g In t
            Yield g.Group.First
        Next
    End Function
    <Extension()>
    Public Iterator Function OneCopy_ByLocusTag(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of FeatureItem)
        Dim LociBuilder As New LocationBuilder

        Dim t = From x In Feats Select x Group By Loc = x.Get_Qulifier_Values(StandardQualifierNames.LocusTag) Into Group
        For Each g In t
            Yield g.Group.First
        Next
    End Function

    <Extension()>
    Public Iterator Function Duplicated_ByLocation(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of FeatureItem)
        For Each g In Feats.Group_ByLocation
            If g.Count > 1 Then Yield g.First
        Next
    End Function
    <Extension()>
    Public Iterator Function Duplicated_ByLocation_ByLocusTag(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of FeatureItem)
        For Each g In Feats.Group_ByLocation_ByLocusTag
            If g.Count > 1 Then Yield g.First
        Next
    End Function
    <Extension()>
    Public Iterator Function Duplicated_ByLocusTag(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of FeatureItem)
        For Each g In Feats.Group_ByLocusTag
            If g.Count > 1 Then Yield g.First
        Next
    End Function


#End Region

#Region "Convert"
    ''' <summary>
    ''' Convert FeatureItem into 4 column Bed Format
    ''' </summary>
    ''' <param name="Feats"></param>
    ''' <param name="seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Iterator Function ToBed(Feats As IEnumerable(Of FeatureItem), seq As Bio.ISequence) As IEnumerable(Of Bio.SequenceRange)
        Dim out As New List(Of Bio.SequenceRange)
        For Each f In Feats
            Yield ToBed(f, seq)
        Next
    End Function
    ''' <summary>
    ''' Convert FeatureItem into 4 column Bed Format
    ''' </summary>
    ''' <param name="Feat"></param>
    ''' <param name="seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function ToBed(Feat As FeatureItem, seq As Bio.ISequence) As Bio.SequenceRange
        Dim x As New Bio.SequenceRange(seq.ID, Feat.Location.LocationStart, Feat.Location.LocationEnd)
        x.Metadata.Add("Name", Feat.Label)
        x.Metadata.Add("Strand", Feat.Location.Get_Strand_PlusOrMinus)
        Return x
    End Function
    ''' <summary>
    ''' Return + or -
    ''' </summary>
    ''' <param name="location"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function Get_Strand_PlusOrMinus(location As ILocation) As String
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
    ''' <summary>
    ''' Return a Sequence in NCBI 5 table Feature Format
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function Convert_To5table(Seq As Bio.ISequence) As String

        Dim str As New System.Text.StringBuilder
        str.Append(">").Append(Seq.ID).AppendLine()
        For Each Feat In Seq.Get_All_Canonical_Features()
            Dim Exons = Feat.Get_Exons
            For Each Exon In Exons
                str.Append(Exon.StartData).Append(vbTab).Append(Exon.EndData)
                If Exon Is Exons.First Then str.Append(vbTab).Append(Feat.Key)
                str.AppendLine()
            Next
            For Each Qual In Feat.Qualifiers

                Dim t = Qual.Value.GetText(" ").Trim(Chr(34))
                If t <> String.Empty Then
                    str.Append(vbTab).Append(vbTab).Append(vbTab).Append(Qual.Key).Append(vbTab)
                    str.Append(t).AppendLine()
                End If
            Next
        Next
        Return str.ToString
    End Function


#End Region



#Region "Annotation"
    ''' <summary>
    ''' Merge Two Features Canonical Qualifiers
    ''' </summary>
    ''' <param name="FeatureForm"></param>
    ''' <param name="FeatureTo"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function Merge_Canonical_Qulifiers(FeatureForm As FeatureItem,
                                          FeatureTo As FeatureItem) As FeatureItem
        Dim Qulifiers = StandardQualifierNames.All
        For Each Qual In StandardQualifierNames.All

            If FeatureTo.Qualifiers.ContainsKey(Qual) = False AndAlso FeatureForm.Qualifiers.ContainsKey(Qual) = True Then
                FeatureTo.Qualifiers(Qual) = FeatureForm.Qualifiers(Qual)
            ElseIf FeatureTo.Qualifiers.ContainsKey(Qual) AndAlso FeatureForm.Qualifiers.ContainsKey(Qual) = True Then
                Dim sFeat1 = FeatureTo.Qualifiers(Qual).GetText(" ").Replace(Chr(34), "")
                Dim sFeat2 = FeatureForm.Qualifiers(Qual).GetText(" ").Replace(Chr(34), "")
                If sFeat1 <> sFeat2 Then
                    Dim x As New List(Of String)
                    x.Add(sFeat1)
                    x.Add(sFeat2)
                    FeatureTo.Qualifiers(Qual) = x
                End If
            End If


        Next

        Return FeatureTo
    End Function
    ''' <summary>
    ''' Merge Two Features Canonical Qualifiers
    ''' </summary>
    ''' <param name="FeatureForm"></param>
    ''' <param name="FeatureTo"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function Merge_All_Qulifiers(FeatureForm As FeatureItem,
                                          FeatureTo As FeatureItem) As FeatureItem
        Dim Qualifiers = FeatureTo.QualifierKeys.Union(FeatureForm.QualifierKeys).Distinct.ToList
        For Each Qual In Qualifiers

            If FeatureTo.Qualifiers.ContainsKey(Qual) = False AndAlso FeatureForm.Qualifiers.ContainsKey(Qual) = True Then
                FeatureTo.Qualifiers(Qual) = FeatureForm.Qualifiers(Qual)
            ElseIf FeatureTo.Qualifiers.ContainsKey(Qual) AndAlso FeatureForm.Qualifiers.ContainsKey(Qual) = True Then
                Dim sFeat1 = FeatureTo.Qualifiers(Qual).GetText(" ").Replace(Chr(34), "")
                Dim sFeat2 = FeatureForm.Qualifiers(Qual).GetText(" ").Replace(Chr(34), "")
                If sFeat1 <> sFeat2 Then
                    Dim x As New List(Of String)
                    x.Add(sFeat1)
                    x.Add(sFeat2)
                    FeatureTo.Qualifiers(Qual) = x
                End If
            End If


        Next

        Return FeatureTo
    End Function
    <Extension()>
    Public Iterator Function MergeByTypeAnsPositions(Features As IEnumerable(Of FeatureItem)) As IEnumerable(Of FeatureItem)
        Dim out As New List(Of FeatureItem)
        For Each FeatureType In Iterator.ByType(Features)
            For Each SamePosFeat In Iterator.ByLocation(FeatureType)
                If SamePosFeat.Count > 1 Then
                    Dim nFeat = MergeFeatures.MergeQualifiers(SamePosFeat)
                    Yield nFeat
                Else
                    Yield SamePosFeat.First
                End If
            Next
        Next
    End Function
#End Region

End Module
