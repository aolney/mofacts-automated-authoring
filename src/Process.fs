module Process

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json //for Json; might be cleaner way
//TODO: possibly replace with this: https://github.com/thoth-org/Thoth.Fetch
open Fable.SimpleHttp

//Fable 2 transition 
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)
//for faking return types
let promisify ( input:string ) =
    promise{ return 1,input}

//Import node transliteration
type ITransliteration =
    abstract transliterate : string -> string
[<ImportAll("transliteration")>]
let transliteration : ITransliteration = jsNative

// Domain model: TODO move to separate file
/// Endpoint configuration object
type Endpoints = 
    {
        SRL : string
        Coreference : string
        DependencyParser : string
        SentenceSplitter : string
    }

///////////////////////////////////////////////////////////////////////
/// RESULTS

type Sentence =
  {
    sentence :  string
    itemId : int
    hasCloze : bool
  }

type Cloze =
  {
    cloze : string
    itemId : int
    clozeId : int
    correctResponse : string
  }

type ClozeAPIResult =
  {
    sentences : Sentence[]
    clozes : Cloze[]
  }


type SRLVerb =
    {
        verb : string
        description : string
        tags : string[]
    }
type SRLResult =
    {
        words : string[]
        verbs : SRLVerb[]
    }

// type HierplaneTree =
//     {
//         linkToPosition : Map<string,string>
//         nodeTypeToStyle : Map<string,string[]>
//         text : string
//     }

type DependencyParseResult =
    {
        arc_loss : float
        //TODO: we want to ignore this; it is redundant with other data
        //hierplane_tree : HierplaneTree
        loss : float
        pos : string[]
        predicted_dependencies : string[]
        predicted_heads : int[]
        tag_loss : float
        words : string[]
    }

type CoreferenceResult =
    {
        clusters : int[][][]
        document : string[]
        predicted_antecedents : int[]
        top_spans : int[][]
    }

type SentenceCoreference =
    {
        ///start/stop word in sentence normalized to sentence indices
        spans : int[][]
        ///id of chain; maps to CoreferenceResult clusters
        clusters : int[]
    }
type SentenceAnnotation = 
    {
        id : int
        sen : string
        srl : SRLResult
        dep : DependencyParseResult
        cor : SentenceCoreference
    }

type DocumentAnnotation = 
    {
        sentences : SentenceAnnotation[]
        coreference : CoreferenceResult
    }

type Clozable =
    {
        words : string[]
        start : int
        stop : int
        trace : ResizeArray<string>
        prob : float
    }
///////////////////////////////////////////////////////////////////////
/// REQUESTS

/// AllenNLP request where a sentence is sent
type SentenceRequest =
    {
        sentence : string
    }

/// AllenNLP request where a document is sent
type DocumentRequest =
    {
        document : string
    }
/// Spacy request where text is sent
type TextRequest =
    {
        text : string
        model : string
    }

let endpoints =
    {
        SRL = "http://141.225.12.235:8000/predict/semantic-role-labeling"
        Coreference = "http://141.225.12.235:8000/predict/coreference-resolution"
        DependencyParser = "http://141.225.12.235:8000/predict/dependency-parsing"
        SentenceSplitter = "http://141.225.12.235:8001/sents"
    }

///Function template for POSTs. Requires on-campus IP address. We assume Promise will give better meteor compatibility
let PostAPI (input:obj) endpoint =
    async {
        let requestData = input |> toJson 
        let! response = 
            Http.request endpoint
            |> Http.method POST
            |> Http.content (BodyContent.Text requestData)
            |> Http.header (Headers.contentType "application/json")
            |> Http.send
        return response.statusCode,response.responseText
    }
    |> Async.StartAsPromise

///Get coreferences from AllenNLP
let GetCoreference( input: string ) =
    PostAPI { document = input } endpoints.Coreference

///Get SRL from AllenNLP.
let GetSRL( input: string ) =
    PostAPI { sentence = input } endpoints.SRL

///Get a parse from AllenNLP.
let GetDependencyParse( input: string ) =
    PostAPI { sentence = input } endpoints.DependencyParser

///Get split sentences from Spacy
let GetSentences( input: string ) =
    PostAPI { text = input; model = "en" } endpoints.SentenceSplitter

///Call services with sentences to generate a seq of promises; remap to promise of seq
let GetForSentences (service: string -> JS.Promise<int*string>) (sentences:string[]) =
    sentences 
    //TODO: this throws away status; use this? https://fsharpforfunandprofit.com/posts/recipe-part2/
    //Possibly can do more intelligent things with promises instead
    |> Seq.map( fun sentence -> sentence |> service  |> Promise.map snd) 
    |> Promise.all

let RegexReplace (pattern : string) (replacement:string) (input:string) =
    System.Text.RegularExpressions.Regex.Replace( input, pattern, replacement )
    
///Per sentence text cleaning
let CleanText input =
    input 
    |> RegexReplace "Page[ 0-9]+" ""
    |> RegexReplace "\(fig[^\)]+\)" ""
    |> RegexReplace "\(see[^\)]+\)" ""
    |> RegexReplace "\(note[^\)]+\)" ""
    |> RegexReplace "\([^\)]+\)" "" //This one is a bit strong: we remove ALL parenthetical material (creates a mess with cloz
    |> RegexReplace "\s+" " "
    |> RegexReplace " \.$" "." //replacements leave spaces before final period
    |> transliteration.transliterate


///Call all NLP functions for a piece of text
let GetNLP( input : string) =
    promise {
        //start with a promise for sentences, throwing away status
        let! rawSentences = input |> GetSentences |> Promise.map( snd >> ofJson<string[]> )
        let sentences = rawSentences |> Array.map CleanText

        //call global services not requiring sentences; note we reformat cleaned sentences to solid text for this
        let! corJson = sentences |> String.concat " " |> GetCoreference |> Promise.map snd
        let cor = corJson |> ofJson<CoreferenceResult>
        //map to first token of span, entire span, and cluster index
        let tokenIdCorefMap =
            cor.clusters
            |> Seq.mapi( fun i c -> 
                c |> Seq.map( fun span -> span.[0],(span,i ))
            )
            |> Seq.collect id
            |> Map.ofSeq

        //call various services with sentences
        let! srlJsons = sentences |> GetForSentences GetSRL
        let! depJsons = sentences |> GetForSentences GetDependencyParse
        //construct the composite NLP object for each sentence
        let sentenceAnnotations = 
            seq {
                let mutable wordIndexOffset = 0
                for i = 0 to sentences.Length - 1 do
                    let srl = srlJsons.[i] |> ofJson<SRLResult>
                    let dep = depJsons.[i] |> ofJson<DependencyParseResult>
                    //map the corefs to this particular sentence by checking if each word is in a coref span
                    let spans = ResizeArray<int[]>()
                    let clusters = ResizeArray<int>()
                    for j = 0 to srl.words.Length - 1 do
                        match tokenIdCorefMap.TryFind( j + wordIndexOffset ) with
                        | Some(span,clusterIndex) ->
                            spans.Add( [| span.[0] - wordIndexOffset; span.[1] - wordIndexOffset |] ) //offset indices to zero for sentence
                            clusters.Add( clusterIndex )
                        | None -> ()
                    wordIndexOffset <- wordIndexOffset + srl.words.Length //all services should agree on tokens in sentence, so using srl here is arbitrary

                    yield { id=i; sen=sentences.[i] ; srl=srl; dep=dep; cor={spans=spans.ToArray();clusters=clusters.ToArray()} }
            }
            |> Seq.toArray

        let documentAnnotation = { sentences=sentenceAnnotations; coreference=cor}

        //return as json b/c our return type is string elsewhere
        return 1,documentAnnotation |> toJson 
    }

///Based on the Heart Study
let EstimateDesiredSentencesAndItems (sentences:string[] ) =
    let wordCount = sentences |> Seq.sumBy( fun sentence -> sentence.Split(' ').Length ) |> float
    let desiredSentences = (wordCount / 1000.0) * 25.0 |> int
    let desiredItems = desiredSentences * 2
    desiredSentences,desiredItems

/// Get weight of all chains in a sentence (add the lengths together)
let GetTotalWeight da sen =
    sen.cor.clusters 
    |> Array.collect( fun id -> 
        let cluster = da.coreference.clusters.[id]
        cluster |> Array.map( fun c -> c.Length) 
        )
    |> Array.sum

let GetClozable da sen =
    let clozable = new ResizeArray<Clozable>()
    //coref based cloze
    clozable.AddRange(
        sen.cor.spans
        |> Seq.map( fun si -> 
        { 
            words = sen.srl.words.[ si.[0] .. si.[1] ]
            start = si.[0] 
            stop =  si.[1]
            trace = ["coref"] |> ResizeArray
            prob = 0.
        }))
    //syntactic subj/obj
    clozable.AddRange(
        sen.dep.predicted_dependencies
        |> Seq.mapi( fun i x -> i,x)
        |> Seq.filter( fun (i,d) -> d.Contains("obj") || d.Contains("subj") ) 
        //must be noun or pronoun (catches edge cases of relative clauses)
        |> Seq.filter( fun (i,d) -> sen.dep.pos.[i].StartsWith("N") || sen.dep.pos.[i] = "PRP" )
        |> Seq.map( fun (i,d) -> 
        { 
            words = [| sen.dep.words.[i] |]
            start = i 
            stop =  i
            trace = ["dep";d] |> ResizeArray
            prob = 0.
        }))
    //srl
    clozable.AddRange(
        sen.srl.verbs
        |> Seq.collect( fun pred ->
            pred.tags 
            |> Seq.mapi( fun i t -> i,t)
            |> Seq.filter( fun (_,t) -> t.Contains("ARG") ) 
            |> Seq.groupBy( fun (_,t) -> t.Split('-').[1])
            |> Seq.map( fun (g,gtSeq) ->
                let start = (gtSeq |> Seq.minBy fst) |> fst
                let stop = (gtSeq |> Seq.maxBy fst) |> fst
                { 
                    words = sen.srl.words.[ start .. stop ]
                    start = start
                    stop =  stop
                    trace = ["srl";pred.description] |> ResizeArray
                    prob = 0.
                }))) 
    //remove overlapping cloze, preferring larger spans
    //a starts before b, but they overlap
    //b starts before a, but they overlap
    //a entirely inside b
    //b entirely inside a
    //all covered with a.start < b.end && b.start < a.end;
    let clozableStatic = clozable.ToArray()
    for ci = 0 to clozableStatic.Length - 1 do
        for cj = 0 to clozableStatic.Length - 1 do
            let overlap =  ci <> cj && clozableStatic.[ci].start < clozableStatic.[cj].stop && clozableStatic.[cj].start < clozableStatic.[ci].stop
            //keep the bigger one
            if overlap && (clozableStatic.[ci].stop - clozableStatic.[ci].start) > (clozableStatic.[cj].stop - clozableStatic.[cj].start) then 
                clozable.Remove( clozableStatic.[cj] ) |> ignore
            elif overlap then
                clozable.Remove( clozableStatic.[ci] ) |> ignore
    //TODO score clozable by probability; see papers in tdf generation about handling prob for words outside our list
    //
    clozable

///Returns cloze items given a block of text. This is the most recent way but by no means the best way
let GetCloze( input : string) =
    promise {
        let clozeAPIResult = {sentences=Array.empty;clozes=Array.empty}
        let! nlp = input |> GetNLP |> Promise.map snd 
        let da = nlp |> ofJson<DocumentAnnotation>
        //cloze:  We don't have the discourse parser yet, so we are only working with coref chains for the purpose of selecting sentences
        // for the moment we only care about #chains with length > 2 and total weight (chains*length) ()
        
        //TODO: open up Beaker and copy/paste here
        let sentenceCount,itemCount = da.sentences |> Array.map( fun x -> x.sen) |> EstimateDesiredSentencesAndItems
        
        //hard filter: we exclude sentences that don't meet these criteria:
        // 3 corefs with chain length > 2
        //partition sentences into those meeting strict criteria and the rest
        let hardFilterSentences,remainingSentences =
            da.sentences
            |> Array.toList
            |> List.partition( fun sen ->
                let chainsLength2OrMore = 
                    sen.cor.clusters 
                    |> Array.map( fun id -> da.coreference.clusters.[id])
                    |> Array.filter( fun c -> c.Length > 1)
                chainsLength2OrMore.Length > 2
            )
        let clozeSentences =
            //if hard filter produced more than we need, sort by total weight and take what we need
            if hardFilterSentences.Length > sentenceCount then
                hardFilterSentences |> List.sortByDescending(  GetTotalWeight da ) |> List.take sentenceCount |> List.sortBy( fun s -> s.id )
            else
                hardFilterSentences @ (remainingSentences  |> List.sortByDescending(  GetTotalWeight da ) |> List.take (sentenceCount-hardFilterSentences.Length ) )
                |> List.sortBy( fun s -> s.id )

        //TODO get clozable, apply minRest logic for assigning items
        return 1, clozeAPIResult |> toJson
    }

///Reverse a string. Test of fable library imports
let DoSimpleComputation( input : string ) =
    input.ToCharArray() |> Seq.rev |> Seq.cast |> String.concat ""