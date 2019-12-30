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

/// Fake a promise return type 
let Promisify ( input:string ) =
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

///Public sentence component of an item
type SentenceAPI =
  {
    sentence :  string
    itemId : int
    hasCloze : bool
  }

///Public clozable component of an item
type ClozableAPI =
  {
    cloze : string
    itemId : int
    clozeId : int
    correctResponse : string
  }

///Public API for items
type ClozeAPI =
  {
    sentences : SentenceAPI[]
    clozes : ClozableAPI[]
  }

///AllenNLP SRL verb
type SRLVerb =
    {
        verb : string
        description : string
        tags : string[]
    }

///AllenNLP SRL result
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

///AllenNLP dependency parse result
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

///AllenNLP coreference result. At the document level only.
type CoreferenceResult =
    {
        clusters : int[][][]
        document : string[]
        predicted_antecedents : int[]
        top_spans : int[][]
    }

///Coreference information remaped to the sentence level
type SentenceCoreference =
    {
        ///start/stop word in sentence normalized to sentence indices
        spans : int[][]
        ///id of chain; maps to CoreferenceResult clusters
        clusters : int[]
    }

///Sentence level annotation combining  annotations from all NLP services
type SentenceAnnotation = 
    {
        id : int
        sen : string
        srl : SRLResult
        dep : DependencyParseResult
        cor : SentenceCoreference
    }

///Document level annotation combining annotations from all NLP services
type DocumentAnnotation = 
    {
        sentences : SentenceAnnotation[]
        coreference : CoreferenceResult
    }

///A clozable we have generated
type Clozable =
    {
        words : string[]
        start : int
        stop : int
        trace : string[]
        prob : float
    }
    /// A custom decoder allows precise json decoding errors to be reported
    static member Decoder : Thoth.Json.Decoder<Clozable>=
        Decode.object
            ( fun get ->
                {
                    words = get.Required.Field "words" (Decode.array Decode.string)
                    start = get.Required.Field "start" Decode.int
                    stop = get.Required.Field "stop" Decode.int
                    trace = get.Required.Field "trace" (Decode.array Decode.string)
                    prob = get.Required.Field "prob" Decode.float
                }
            )

///All data we have collected and will use for final creation of cloze items
type InternalAPI =
    {
        sentences : SentenceAnnotation[]
        coreference : CoreferenceResult
        clozables : Clozable[][]
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

///Endpoints for NLP services
let endpoints =
    {
        SRL = "http://141.225.12.235:8000/predict/semantic-role-labeling"
        Coreference = "http://141.225.12.235:8000/predict/coreference-resolution"
        DependencyParser = "http://141.225.12.235:8000/predict/dependency-parsing"
        SentenceSplitter = "http://141.225.12.235:8001/sents"
        // SRL = "https://allennlp.olney.ai/predict/semantic-role-labeling"
        // Coreference = "https://allennlp.olney.ai/predict/coreference-resolution"
        // DependencyParser = "https://allennlp.olney.ai/predict/dependency-parsing"
        // SentenceSplitter = "https://spacy.olney.ai/sents"
    }

///Function template for POSTs. Requires on-campus IP address. We assume Promise will give better meteor compatibility. Passing in the encoding to avoid CORS preflight on spacy.
let PostAPI (input:obj) endpoint encoding =
    async {
        let requestData = input |> toJson 
        let! response = 
            Http.request endpoint
            |> Http.method POST
            |> Http.content (BodyContent.Text requestData)
            |> Http.header (Headers.contentType encoding)
            |> Http.send
        return response.statusCode,response.responseText
    }
    |> Async.StartAsPromise

///Get coreferences from AllenNLP
let GetCoreference( input: string ) =
    PostAPI { document = input } endpoints.Coreference "application/json"

///Get SRL from AllenNLP.
let GetSRL( input: string ) =
    PostAPI { sentence = input } endpoints.SRL "application/json"

///Get a parse from AllenNLP.
let GetDependencyParse( input: string ) =
    PostAPI { sentence = input } endpoints.DependencyParser "application/json"

///Get split sentences from Spacy. HACK: passing in this encoding to avoid CORS preflight 
let GetSentences( input: string ) =
    PostAPI { text = input; model = "en" } endpoints.SentenceSplitter "text/plain"

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
        let! sentences = input |> CleanText|> GetSentences  |> Promise.map( snd >> ofJson<string[]> )
        //Clean "junk" from text; this can blank out sentences, so we filter those
        //let sentences = rawSentences |> Array.map CleanText |> Array.filter( fun sen -> sen.Length > 0 )

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
let EstimateDesiredSentences (sentences:string[] ) =
    let wordCount = sentences |> Seq.sumBy( fun sentence -> sentence.Split(' ').Length ) |> float
    let desiredSentences = (wordCount / 1000.0) * 25.0 |> int //
    desiredSentences
///Based on the Heart Study
let EstimateDesiredItems desiredSentences =
    let desiredItems = desiredSentences * 2
    desiredItems

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

    //check for insanity first. return empty if insane
    if startInit < 0 || stopInit >= sen.srl.words.Length then //|| head < 0 || head >= sen.srl.words.Length then
        trace.Add("CRITICAL: invalid clozable parameters for " + (sen |> toJson ) )
        { words=Array.empty; start=0; stop=0; trace=trace.ToArray() ; prob = 1.0 }
    else
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
                    //debug
                    // if sen.id > 140 then
                    //     printfn "debug"
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
        let start, stop, words = 
            if indices.Length <> 0 then
                let start = indices.[0]
                let stop = indices |> Array.last
                start, stop, sen.srl.words.[ start .. stop ]
            else
                trace.Add("CRITICAL: stanford head yields empty span, defaulting to given span")
                startInit, stopInit, sen.srl.words.[ startInit .. stopInit ]
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
            |> Seq.groupBy( fun (_,t) -> t.Substring(2)) //e.g. I-ARG0, so group by ARG0; we don't split b/c there are multiple hyphens
            |> Seq.map( fun (g,gtSeq) ->
                let start = (gtSeq |> Seq.minBy fst) |> fst
                let stop = (gtSeq |> Seq.maxBy fst) |> fst
                GetModifiedNPClozable sen start stop None [| "srl";pred.description |])
        )
    )

    clozable

///To throw away sentences we don't know how to handle
let badSentenceRegex = System.Text.RegularExpressions.Regex( "(figure|table|section|clinical|application)\s+[0-9]",Text.RegularExpressions.RegexOptions.IgnoreCase)

///Returns cloze items given a block of text and an optional JSON of DocumentAnnotation
let GetInternalAPI (nlpJsonOption: string option) ( input : string ) =
    promise {
        //Get a DocumentAnnotation if one wasn't passed in
        let! nlp = 
            match nlpJsonOption with
            | Some(nlpJson) -> nlpJson |> Promisify |> Promise.map snd 
            | None -> input |> GetNLP |> Promise.map snd 
        let da = nlp |> ofJson<DocumentAnnotation>

        //Make clozables for every sentence (not efficient, but useful for research)
        let clozables = da.sentences |> Array.map GetClozable |> Array.map( fun ra -> ra.ToArray() )

        return 1, {sentences = da.sentences; coreference = da.coreference; clozables = clozables} |> toJson
    }

/// Remove overlapping cloze, preferring larger spans
/// a starts before b, but they overlap
/// b starts before a, but they overlap
/// a entirely inside b
/// b entirely inside a
/// all covered with a.start < b.end && b.start < a.end;
let RemoveOverlappingClozables (clozables : Clozable[] ) =
    let clozablesOut = ResizeArray<Clozable>(clozables)
    for ci = 0 to clozables.Length - 1 do
        for cj = ci to clozables.Length - 1 do
            let overlap =  ci <> cj && clozables.[ci].start <= clozables.[cj].stop && clozables.[cj].start <= clozables.[ci].stop
            //keep the bigger one
            if overlap && (clozables.[ci].stop - clozables.[ci].start) >= (clozables.[cj].stop - clozables.[cj].start) then 
                clozablesOut.Remove( clozables.[cj] ) |> ignore
            elif overlap then
                clozablesOut.Remove( clozables.[ci] ) |> ignore
    //
    clozablesOut.ToArray()

/// Return an item as a sentence with words blanked out, together with the corresponding words
let MakeItem (sa:SentenceAnnotation) (cl:Clozable)=
    let itemWords = Array.copy sa.srl.words
    for i = cl.start to cl.stop do
        itemWords.[i] <- "__________"
    itemWords |> String.concat " ", cl.words |> String.concat " "

/// Public facing API. Calls the internal function and then wraps result in API format
let GetClozeAPI (nlpOption: string option) (sentenceCountOption: int option) (itemCountOption: int option) (input : string) = 
    promise{
        let! internalAPIJson = input |> GetInternalAPI nlpOption |> Promise.map snd 
        let internalAPI = internalAPIJson |> ofJson<InternalAPI>
        
          //Estimate how many items we want if this wasn't specified
        let sentenceCount = 
            match sentenceCountOption with
            | Some(sentenceCount) -> sentenceCount
            | None -> internalAPI.sentences |> Array.map( fun x -> x.sen) |> EstimateDesiredSentences 
        let itemCount =
            match itemCountOption with
            | Some(itemCount) -> itemCount
            | None -> sentenceCount |> EstimateDesiredItems
        
        //hard filter: we exclude sentences that don't meet these criteria:
        // 3 corefs with chain length > 2
        // TODO: We don't have the discourse parser yet, so we can't apply the "sentence contains nucleus" constraint

        //partition sentences into those meeting strict criteria and the rest
        let hardFilterTuples,remainingTuples =
            internalAPI.sentences
            |> Array.mapi( fun i s -> s,internalAPI.clozables.[i])
            //Filter sentences we don't know how to handle (A&P specific)
            |> Array.filter( fun (sa,_) -> sa.sen |> badSentenceRegex.IsMatch |> not )
            //Handle overlapping cloze
            |> Array.map( fun ( sa, cl ) -> sa, cl |> RemoveOverlappingClozables )
            //Remove impossibly hard cloze (>3 fill ins; TODO: get theoretical justification; assume pseudohead is problem)
            |> Array.map( fun ( sa, cls ) -> sa, cls |> Array.filter( fun cl -> cl.words.Length < 4 ) )
            //Filter sentences with no clozables
            |> Array.filter( fun (_,cl) -> cl.Length > 0 )
            |> Array.toList
            //Apply strict criteria to create partition
            |> List.partition( fun (sa,_) ->
                let chainsLengthTwoOrMore = 
                    sa.cor.clusters 
                    |> Array.map( fun id -> internalAPI.coreference.clusters.[id])
                    |> Array.filter( fun c -> c.Length > 1)
                chainsLengthTwoOrMore.Length > 2 //we have > 2 chains with length > 1
            )

        //Get the cloze tuples to make our items from. NOTE: we let desiredSentences take priority over desiredItems here. TODO decide which has priority, sentences or items.
        let clozeTuples =
            let hardFilterSentenceCount = hardFilterTuples.Length
            //let hardFilterItemCount = hardFilterTuples |> Seq.collect snd |> Seq.length
            //if hard filter produced at least as many items and sentences as we need, sort by total weight and take what we need. TODO use other criteria besides weight?
            if hardFilterSentenceCount > sentenceCount then //&& hardFilterItemCount > itemCount then
                hardFilterTuples |> List.sortByDescending( fun (sa,_) -> sa |> GetTotalWeight internalAPI ) |> List.take sentenceCount |> List.sortBy( fun (s,_) -> s.id )
            //otherwise use all hard filter tuples and add top remainingTuples to get desired counts
            else
                hardFilterTuples @ (remainingTuples  |> List.sortByDescending( fun (sa,_) -> sa |>  GetTotalWeight internalAPI ) |> List.take (sentenceCount-hardFilterTuples.Length ) )
                |> List.sortBy( fun (s,_)-> s.id )

        //We need to generate the desired # items BUT we also must take at least 1 from each sentence
        //Step 1. Partition clozables into min per sentence and rest per sentence
        let clozeProbTuples = 
            clozeTuples
            |> List.map( fun (sa,cls) -> 
                let sorted = cls |> Array.sortBy( fun cl -> cl.prob ) |> Array.toList
                sa,sorted.Head,sorted.Tail
            )

        //Step 2. Combine and rank rest clozables by prob, take itemCount, create lookup map
        let restClozableMap = 
            clozeProbTuples
            |> List.collect( fun (sa,_,rest) -> 
                rest |> List.map( fun c -> sa,c) //inflate
            )
            |> List.sortBy( fun (_,cl) -> cl.prob)
            |> List.take (itemCount - sentenceCount)
            |> List.groupBy fst
            |> Map.ofList
   
        //Step 3. Iterate over clozeProbTuples, adding matching high prob clozables to make allClozableMap
        let allClozableMap = 
            clozeProbTuples
            |> List.map( fun (sa, min, rest ) ->
                let cl = 
                    match restClozableMap.TryFind(sa) with 
                    | Some( t ) -> t |> List.map snd
                    | None -> []
                sa, min::cl
            )
            |> Map.ofList

        //Package for external API
        let acronymMap = input |> GetAcronymMap |> ofJson<Map<string,string>>
        let sentences = ResizeArray<SentenceAPI>()
        let clozes = ResizeArray<ClozableAPI>()

        internalAPI.sentences
        |> Seq.iter( fun sa ->
            match allClozableMap.TryFind(sa) with
            | None -> sentences.Add( { sentence = sa.sen; itemId = (hash sa); hasCloze = false} )
            | Some(clozables) -> 
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