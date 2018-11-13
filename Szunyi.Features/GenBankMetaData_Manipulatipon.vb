Imports Bio
Imports Bio.IO.GenBank

Public Class GenBankMetaDataManipulation

    Public Shared Sub SortFeatures(Seqs As List(Of Bio.ISequence))
        For Each Seq In Seqs
            SortFeatures(Seq)
        Next
    End Sub

    ''' <summary>
    ''' Create Default GenBankMetaData as unknown Organism and FeatureList
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    Public Shared Function CreateNAGenBankMetaData(Seq As Sequence) As GenBankMetadata
        Dim x As New Bio.IO.GenBank.GenBankMetadata
        x.Locus = New Bio.IO.GenBank.GenBankLocusInfo()
        x.Locus.Date = Now
        x.Locus.MoleculeType = MoleculeType.DNA
        x.Locus.Name = Seq.ID
        x.Locus.SequenceLength = Seq.Count
        x.Locus.StrandTopology = SequenceStrandTopology.Linear
        x.Accession = New GenBankAccession()
        x.Accession.Primary = Seq.ID
        x.Source = New Bio.IO.GenBank.SequenceSource
        x.Source.CommonName = "Unknown."
        x.Source.Organism = New Bio.IO.GenBank.OrganismInfo
        x.Source.Organism.Species = "Unknown"
        x.Source.Organism.Genus = "Unknown."
        x.Features = New SequenceFeatures
        Return x
    End Function

    ''' <summary>
    ''' Return the List of GenBankMetadatas or Empty List
    ''' </summary>
    ''' <param name="Seqs"></param>
    ''' <returns></returns>
    Public Shared Function GetGenBankMetaDatasFromSeqs(Seqs As List(Of Bio.ISequence)) As List(Of Bio.IO.GenBank.GenBankMetadata)
        Dim Out As New List(Of Bio.IO.GenBank.GenBankMetadata)
        If IsNothing(Seqs) = True Then Return Out
        For Each Seq In Seqs
            If Seq.Metadata.ContainsKey(Bio.Util.Helper.GenBankMetadataKey) Then
                Out.Add(Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey))
            End If
        Next
        Return Out
    End Function
    ''' <summary>
    ''' Return The GenBankMetaData or Nothing
    ''' </summary>
    ''' <param name="Seq"></param>
    ''' <returns></returns>
    Public Shared Function GetGenBankMetaDataFromSeq(Seq As Bio.ISequence) As Bio.IO.GenBank.GenBankMetadata
        If IsNothing(Seq) = True Then Return Nothing
        If Seq.Metadata.ContainsKey(Bio.Util.Helper.GenBankMetadataKey) Then
            Return (Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey))
        End If
        Return Nothing
    End Function
    Public Shared Function GetOrCreateGenBankMetaDataFromSeq(Seq As Bio.ISequence) As Bio.IO.GenBank.GenBankMetadata
        If IsNothing(Seq) = True Then Return Nothing
        If Seq.Metadata.ContainsKey(Bio.Util.Helper.GenBankMetadataKey) Then
            Return (Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey))
        Else
            Dim md As New Bio.IO.GenBank.GenBankMetadata
            md.Accession = New GenBankAccession()
            md.Source = New Bio.IO.GenBank.SequenceSource()
            md.Source.Organism = New OrganismInfo
            md.Locus = New Bio.IO.GenBank.GenBankLocusInfo
            md.Features = New SequenceFeatures
            Seq.Metadata.Add(Bio.Util.Helper.GenBankMetadataKey, md)
            Return Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey)
        End If

    End Function

    Public Shared Sub AddFeature(ByRef Seq As Sequence, Loci As ILocation, FeatureKey As String, Optional LocusTags As List(Of String) = Nothing)
        Dim x As New FeatureItem(FeatureKey, Loci)
        If IsNothing(LocusTags) = False Then x.Qualifiers(StandardQualifierNames.LocusTag) = LocusTags
        Dim Md = GenBankMetaDataManipulation.GetOrCreateGenBankMetaDataFromSeq(Seq)
        Md.Features.All.Add(x)

    End Sub
    Public Shared Sub AddFeature(ByRef Seq As Sequence, Feat As FeatureItem)
        Dim Md = GenBankMetaDataManipulation.GetOrCreateGenBankMetaDataFromSeq(Seq)
        If IsNothing(Md) = False Then
            AddFeature(Md, Feat, Feat.Key)
        End If
    End Sub
    Public Shared Sub AddFeatures(ByRef Seq As Sequence, Feats As List(Of FeatureItem))
        Dim Md = GenBankMetaDataManipulation.GetOrCreateGenBankMetaDataFromSeq(Seq)
        For Each Feat In Feats
            AddFeature(Md, Feat, Feat.Key)
        Next

    End Sub

    Public Shared Sub AddFeature(Md As GenBankMetadata, x As FeatureItem, FeatureKey As String)
        Select Case FeatureKey
            Case StandardFeatureKeys.CodingSequence
                Dim l As New CodingSequence(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.MessengerRna
                Dim l As New MessengerRna(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
                Dim alf As Int16 = 65
            Case StandardFeatureKeys.Gene
                Dim l As New Gene(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.NonCodingRna
                Dim l As New NonCodingRna(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.MiscFeature
                Dim l As New MiscFeature(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.MiscRecombination
                Dim l As New MiscRecombination(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.MiscSignal
                Dim l As New MiscSignal(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.Intron
                Dim l As New Intron(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.Promoter
                Dim l As New Promoter(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.Exon
                Dim l As New Exon(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.Terminator
                Dim l As New Terminator(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.PolyASignal
                Dim l As New PolyASignal(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.MaturePeptide
                Dim l As New MaturePeptide(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.OperonRegion
                Dim l As New OperonRegion(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.FivePrimeUtr
                Dim l As New FivePrimeUtr(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case StandardFeatureKeys.ThreePrimeUtr
                Dim l As New ThreePrimeUtr(x.Location)
                MergeFeatures.Merge2Features(x, l, True)
                Md.Features.All.Add(l)
            Case Else
                Md.Features.All.Add(x)
                Dim alf As Int16 = 65
        End Select


    End Sub

    Public Shared Sub Remove_Feature(seq As Sequence, x As FeatureItem)
        Dim Md = GenBankMetaDataManipulation.GetOrCreateGenBankMetaDataFromSeq(seq)
        Md.Features.All.Remove(x)
    End Sub
    Public Shared Sub Remove_Feature(seq As Sequence, Feats As List(Of FeatureItem))
        If IsNothing(Feats) = True Then Exit Sub
        Dim Md = GenBankMetaDataManipulation.GetOrCreateGenBankMetaDataFromSeq(seq)
        For Each x In Feats
            Md.Features.All.Remove(x)
        Next
    End Sub

    Public Shared Function GetHeaders(seqs As List(Of ISequence)) As List(Of String)
        Dim out As New List(Of String)
        Dim Header As New System.Text.StringBuilder
        Header.Append("Name").Append(vbTab)
        Header.Append("Length").Append(vbTab)
        Header.Append("GC%").Append(vbTab)
        Header.Append("Seq Type").Append(vbTab)
        Header.Append("Molecula Type").Append(vbTab)
        Header.Append("Seq strand").Append(vbTab)
        Header.Append("Common Name").Append(vbTab)
        Header.Append("Strain").Append(vbTab)
        For Each Seq In seqs
            Dim md = GetGenBankMetaDataFromSeq(Seq)
            If IsNothing(md) = False Then
                Dim str As New System.Text.StringBuilder
                str.Append(md.Locus.Name).Append(vbTab)
                str.Append(md.Locus.SequenceLength).Append(vbTab)
                '     str.Append(Szunyi.DNA.Common.Get_Percents_GC(Seq)).Append(vbTab)

                str.Append(Bio.IO.GenBank.GenBankLocusTokenParser.LocusConstants.AlphabetTypes(md.Locus.MoleculeType)).Append(vbTab)
                str.Append(Bio.IO.GenBank.GenBankLocusTokenParser.LocusConstants.SequenceStrandTypes(md.Locus.Strand)).Append(vbTab)
                str.Append(md.Locus.Strand).Append(vbTab)
                '  str.Append(md.Locus.StrandTopology).Append(vbTab)
                '  str.Append(md.Locus.Date.ToString).Append(vbTab)
                ' str.Append(md.Accession.Primary).Append(vbTab)
                ' str.Append(md.Accession.Secondary).Append(vbTab)
                ' str.Append(md.Definition).Append(vbTab)
                Dim s = md.Source
                str.Append(s.CommonName).Append(vbTab)

                Dim feats = Manipulation.GetFeaturesByType.GetFeatureByTypeFromMetadata("source", md)
                If feats.Count <> 0 Then
                    Dim Q = Qulifiers.Get_All_Values(feats.First)
                    str.Append(Q)
                End If
                out.Add(str.ToString)
            End If
        Next
        Return out
    End Function

    Public Shared Function Get_Features_Keys(seqs As List(Of ISequence)) As List(Of String)
        Dim out As New List(Of String)
        Dim Feat = Manipulation.GetFeaturesByType.Get_All_Features(seqs)

        For Each f In Feat
            If out.Contains(f.Key) = False Then out.Add(f.Key)
        Next
        Return out

    End Function
    Public Shared Function Get_Accesion(Seq As Bio.Sequence) As String
        Dim md = GetGenBankMetaDataFromSeq(Seq)
        If IsNothing(md) = True Then Return String.Empty
        Return md.Accession.Primary
    End Function
    Public Shared Function Get_Locus(Seq As ISequence) As String
        Dim md As Bio.IO.GenBank.GenBankMetadata = Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey)
        Return md.Locus.Name
    End Function
    Public Shared Function Get_Common_Name(seq As ISequence) As String
        Dim Source = Get_Source(seq)
        If IsNothing(Source) = True Then Return String.Empty
        If IsNothing(Source.Organism) = True Then Return String.Empty
        Return Source.Organism.Genus & " " & Source.Organism.Species
    End Function
    Public Shared Function Get_Source(Seq As Bio.ISequence) As Bio.IO.GenBank.SequenceSource
        Dim md As Bio.IO.GenBank.GenBankMetadata = Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey)
        If IsNothing(md) = True Then Return Nothing
        Return md.Source
    End Function
    Public Shared Function Get_Mol_Type(Seq As Bio.ISequence) As Bio.IO.GenBank.MoleculeType
        Dim md As Bio.IO.GenBank.GenBankMetadata = Seq.Metadata(Bio.Util.Helper.GenBankMetadataKey)
        If IsNothing(md) = True Then Return Nothing
        Return md.Locus.MoleculeType
    End Function
    Public Shared Function Get_TaxID(seq As ISequence) As String
        Dim md As Bio.IO.GenBank.GenBankMetadata = seq.Metadata(Bio.Util.Helper.GenBankMetadataKey)
        If IsNothing(md) = True Then Return Nothing
        Dim source = From x In md.Features.All Where x.Key = "source"
        If source.Count > 0 Then
            If source.First.Qualifiers.ContainsKey("db_xref") Then
                Dim db = source.First.Qualifiers("db_xref")
                Dim res = From x In db Where x.Contains("taxon")
                If res.Count > 0 Then
                    Dim s1 = Split(res.First, "taxon:").Last
                    Return s1.Trim(Chr(34))
                End If

            End If
        End If
        Return 0
    End Function
End Class
