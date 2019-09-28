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

type SentenceAPI =
  {
    sentence :  string
    itemId : int
    hasCloze : bool
  }

type ClozableAPI =
  {
    cloze : string
    itemId : int
    clozeId : int
    correctResponse : string
  }

type ClozeAPI =
  {
    sentences : SentenceAPI[]
    clozes : ClozableAPI[]
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
        trace : string[]
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
    
let Split ( pattern : char ) (input: string) = input.Split( pattern 
)
///Per sentence text cleaning. NOTE regexes are A&P specific!
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

/// Finds acronyms in parentheses and tries to map to nearby words. Makes strong assumptions / not highly general
let GetAcronymMap input =
    //assumes all acronyms are caps only and bounded by parentheses. NOTE: used named group at first but gave up when it didn't work
    let acronymRegex = System.Text.RegularExpressions.Regex( "\(([A-Z]+)\)" )
    let matches = acronymRegex.Matches( input )
    let acronymMap = 
        if matches.Count <> 0 then
            seq {
                for m in matches |> Seq.cast<System.Text.RegularExpressions.Match> do
                    let acronym = m.Groups.[1].Value
                    let index = m.Index //group index (m.Groups.[1].Index) not implemented in fable; m.Index OK because match almost identical to group
                    //get the words before the acronym
                    let start = if index - 50 > 0 then index - 50 else 0
                    let words = input.Substring( start, 50 ) |> Split ' '
                    //convert words to a string of their first letters and search for acronym in it
                    let firstLetterString = words |> Array.map( fun w -> w.[0] ) |> String
                    let letterRegex = System.Text.RegularExpressions.Regex( acronym )
                    let lm = letterRegex.Match( firstLetterString.ToUpper() )
                    //if we find a match then return the corresponding phrase and acronym
                    if lm.Success then
                        let phrase = words.[ lm.Index .. acronym.Length ] |> String.concat " "
                        yield phrase,acronym
                        yield acronym,phrase
            }
            |> Map.ofSeq
        else
            Map.empty
    //
    acronymMap |> toJson

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
    let desiredSentences = (wordCount / 1000.0) * 25.0 |> int //
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

/// Returns a Clozable for a modified NP given a sentence annotation and span of interest
let GetModifiedNPClozable sen startInit stopInit head traceInit =
    let trace = ResizeArray<string>()
    trace.AddRange(traceInit)
    //this is a pseudohead of the span. we can't use real heads because stanford dependencies aren't universal dependencies
    //therefore we must allow for functional/exocentric heads but find the pseudohead approximating universal dependencies 
    let h =
        match head with
        | Some(x) -> x
        | None -> 
            let stanfordHead = 
                [|  startInit .. stopInit |] 
                //get the predicted heads for each index; predicted heads are 1 indexed (root is 0)
                |> Seq.map( fun i -> i,sen.dep.predicted_heads.[i])
                //find tuple with a predicted head outside the span (because English is projective)
                |> Seq.find( fun (_,h) -> h < startInit + 1 || h > stopInit + 1 )
                //return the index
                |> fst
            //require nominal pseudohead if stanfordHead is not nominal
            if sen.dep.pos.[ stanfordHead ].StartsWith("NN") |> not then
                trace.Add( "head is not nominal")
                //get subj/obj dependencies, take first
                let argOption = [|  startInit .. stopInit |] |> Seq.map( fun i -> i,sen.dep.predicted_dependencies.[i]) |> Seq.tryFind( fun (_,h) -> h.Contains("subj") || h.Contains("obj")) 
                //get nominal words, take last
                let nnOption = [|  startInit .. stopInit |] |> Seq.map( fun i -> i,sen.dep.pos.[i]) |> Seq.rev |> Seq.tryFind( fun (_,h) -> h.StartsWith("NN")) 
                match argOption,nnOption with
                | Some(arg),_ ->  trace.Add( "WARNING: using first syntactic arg as pseudohead"); arg |> fst
                | _, Some(nn) -> trace.Add( "WARNING: using last nominal as pseudohead"); nn |> fst
                | _,_ -> trace.Add( "CRITICAL: clozable without nominal or arg, defaulting to given span"); stopInit
            else
                stanfordHead
    //take preceeding modifiers of the nominal pseudohead that are nounish or JJ 
    let indices = [| startInit .. h |] |> Array.rev |> Array.takeWhile( fun i -> sen.dep.pos.[i].StartsWith("N") || sen.dep.pos.[i] =  "JJ" ) |> Array.rev
    let start = indices.[0]
    let stop = indices |> Array.last
    let words = sen.srl.words.[ start .. stop ]
    let clozable = 
        { 
            words = words
            start = start
            stop =  stop
            trace = trace.ToArray()
            //use the lowest freq word in the span
            prob = words |> Array.map WordFrequency.Get |> Array.min
        }
    //
    clozable

/// GetClozable items for a sentence using syntactic, srl, and coref information. TODO: filter non subj/obj/mod
let GetClozable sen =
    let clozable = new ResizeArray<Clozable>()
    //coref based cloze
    clozable.AddRange( 
        sen.cor.spans 
        |> Seq.map( fun si -> GetModifiedNPClozable sen si.[0] si.[1] None [| "coref" |] )
    )
    //syntactic subj/obj
    clozable.AddRange(
        sen.dep.predicted_dependencies
        |> Seq.mapi( fun i x -> i,x)
        |> Seq.filter( fun (i,d) -> d.Contains("obj") || d.Contains("subj") || d.Contains("root") ) //root for copula constructions
        //must be noun (catches edge cases of relative clauses) TODO: allow pronoun if resolved to referent
        |> Seq.filter( fun (i,d) -> sen.dep.pos.[i].StartsWith("N") ) // || sen.dep.pos.[i] = "PRP" )
        |> Seq.map( fun (i,d) -> GetModifiedNPClozable sen i i (i|>Some) [| "dep";d |] )
    )
    //srl
    clozable.AddRange(
        sen.srl.verbs
        |> Seq.collect( fun pred ->
            pred.tags 
            |> Seq.mapi( fun i t -> i,t)
            |> Seq.filter( fun (_,t) -> t.Contains("ARG") ) 
            |> Seq.groupBy( fun (_,t) -> t.Split('-').[1]) //e.g. I-ARG0, so group by ARG0
            |> Seq.map( fun (g,gtSeq) ->
                let start = (gtSeq |> Seq.minBy fst) |> fst
                let stop = (gtSeq |> Seq.maxBy fst) |> fst
                GetModifiedNPClozable sen start stop None [| "srl";pred.description |])
        )
    )
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
    //
    clozable

///To throw away sentences we don't know how to handle
let badSentenceRegex = System.Text.RegularExpressions.Regex( "(figure|table|section|clinical|application)\s+[0-9]",Text.RegularExpressions.RegexOptions.IgnoreCase)

///Returns cloze items given a block of text. This is the most recent way but by no means the best way
let GetClozeInternal( input : string) =
    promise {
        let! nlp = input |> GetNLP |> Promise.map snd 
        let da = nlp |> ofJson<DocumentAnnotation>

        //Estimate how many items we want
        let sentenceCount,itemCount = da.sentences |> Array.map( fun x -> x.sen) |> EstimateDesiredSentencesAndItems
        
        //hard filter: we exclude sentences that don't meet these criteria:
        // 3 corefs with chain length > 2
        // TODO: We don't have the discourse parser yet, so we can't apply the "sentence contains nucleus" constraint

        //partition sentences into those meeting strict criteria and the rest
        let hardFilterSentences,remainingSentences =
            da.sentences
            //Filter sentences we don't know how to handle (A&P specific)
            |> Array.filter( fun sa -> sa.sen |> badSentenceRegex.IsMatch |> not )
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

        //Internal output; has more information than we'd push through public API
        let clozeResultDictionary = new System.Collections.Generic.Dictionary<SentenceAnnotation,ResizeArray<Clozable>>()
        let AddClozable key item =
            if not <| clozeResultDictionary.ContainsKey(key) then
                clozeResultDictionary.Add(key, ResizeArray<Clozable>() )
            clozeResultDictionary.[key].Add(item)
        //Auto json serialization seems to fail for our generic types (or possibly nulls?); convert here
        let GetResultJson() =
            clozeResultDictionary
            |> Seq.map ( fun (KeyValue(sa, ra)) -> sa, if ra <> null then ra.ToArray() else Array.empty )
            |> Map.ofSeq
            |> toJson

        //We need to generate the desired # items BUT we also must take at least 1 from each sentence
        //Pass 1: Occurs inside. Take the lowest freq to make item for a sentence; add remaining items to list
        //Pass 2: take N-sent lowest freq from remainder list
        let restList =
            clozeSentences
            |> List.collect( fun sa -> 
                let clozables = sa |> GetClozable |> Seq.sortByDescending( fun c -> c.prob ) |> Seq.toList
                //make min items so each sentence has 1 item
                AddClozable sa clozables.Head
                //sentence, remaining clozables; flatten/inflate
                clozables.Tail |> List.map( fun c -> sa,c)
            )

        //make remaining items
        restList
        |> List.sortBy( fun (_,c) -> c.prob )
        |> List.take (itemCount - sentenceCount) //bc we already took sentenceCount worth: we took the min of each sentence
        |> List.iter( fun (sa,c) -> AddClozable sa c )

        //Add all sentences we are NOT using as well; they have null for clozables
        da.sentences
        |> Array.iter( fun sen ->
            if not <| clozeResultDictionary.ContainsKey(sen) then
                clozeResultDictionary.Add( sen, null )
        )

        //return 1, clozeResultDictionary |> toJson //gives Thoth serialization errors; suspect generics in nested types are problem
        return 1, GetResultJson()
    }

/// Return an item as a sentence with words blanked out, together with the corresponding words
let MakeItem (sa:SentenceAnnotation) (cl:Clozable)=
    let itemWords = Array.copy sa.srl.words
    for i = cl.start to cl.stop do
        itemWords.[i] <- "__________"
    itemWords |> String.concat " ", cl.words |> String.concat " "

/// Public facing API. Calls the internal function and then wraps result in API format
let GetClozeAPI (input : string) = 
    promise{
        let! clozeInternal = input |> GetClozeInternal |> Promise.map snd 
        let clozeResultDictionary = clozeInternal |> ofJson<Map<SentenceAnnotation,Clozable[]>>
        let acronymMap = input |> GetAcronymMap |> ofJson<Map<string,string>>

        let sentences = ResizeArray<SentenceAPI>()
        let clozes = ResizeArray<ClozableAPI>()

        //clozeResultDictionary.Keys
        clozeResultDictionary
        |> Map.toSeq
        |> Seq.sortBy( fun (sa,cl) -> sa.id ) //not sure if sorted order is needed/assumed
        |> Seq.iter( fun (sa,clozables) ->
            match clozables.Length with
            //no clozables for this sentence
            | 0 -> sentences.Add( { sentence = sa.sen; itemId = (hash sa); hasCloze = false} )
            | _ -> 
                sentences.Add( { sentence = sa.sen; itemId = (hash sa); hasCloze = true} )
                clozables |> Seq.iter( fun cl ->
                    let cloze,correctResponse = MakeItem sa cl
                    //insert any alternative correct responses here
                    let correctResponses = 
                        match acronymMap.TryFind(correctResponse) with
                        | Some( acronym ) -> correctResponse + "|" + acronym
                        | None -> correctResponse
                    clozes.Add( { cloze=cloze; itemId = hash sa; clozeId = hash clozables; correctResponse = correctResponses} )
                )
            )
        return 1, {sentences=sentences.ToArray();clozes=clozes.ToArray()} |> toJson
    }

///Reverse a string. Test of fable library imports
let DoSimpleComputation( input : string ) =
    input.ToCharArray() |> Seq.rev |> Seq.cast |> String.concat ""