module ClozeAPI

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json //for Json; might be cleaner way
//TODO: possibly replace with this: https://github.com/thoth-org/Thoth.Fetch
// open Fable.SimpleHttp

open AllenNLP

// Domain model
// stimulusId can be used to replace clozeId and clusterId to replace itemId. 
// itemId is used to represent the id as it is in the database so I think stimulusId would be more accurate.

//-- Public ------------------------------
///Public sentence component of an item
type SentenceAPI =
  {
    sentence :  string
    clusterId : int
    hasCloze : bool
  }

///Public clozable component of an item
type ClozableAPI =
  {
    cloze : string
    clusterId : int
    itemId : int
    correctResponse : string
    /// Tags we can use for cluster assignment
    tags : obj //the idea right now is to make this an anonymous record since the devs want an object rather than a list of attributes
  }

///Public API for items
type ClozeAPI =
  {
    sentences : SentenceAPI[]
    clozes : ClozableAPI[]
  }
  
//-- Internal ------------------------------
/// A tag such that we can create an object literal (a pojo) from a list of tags
type Tag =
    /// a measure of importance of an item group, with 1 being the most important
    | WeightGroup of int 
    /// the numeric order of an item group, with 1 being the first
    | OrderGroup of int 
    /// the dependency role of the cloze in the item
    | SyntacticRole of string 
    /// the semantic role of the cloze in the item
    | SemanticRole of string
    /// number of hops from the cloze to the root of the item
    | RootDistance of int 
    /// number of words from the start of the item to the cloze
    | StartDistance of int 
    /// The number of coref clusters/chains in the item
    | CorefClusters of int
    /// The total weight (length) of all coref chains in the item
    | CorefClusterTotalWeight of int
    /// The backward weight (length) of all coref chains in the item
    | CorefClusterBackwardWeight of int
    /// The forward weight (length) of all coref chains in the item
    | CorefClusterForwardWeight of int
    /// Variant of total weight
    | SentenceWeight of int
    /// Word probability of rarest word in a cloze answer
    | ClozeProbability of float
    /// Source sentence with coreferents resolved
    | ClozeCorefTransformation of string
    /// Correct response with coreferents resolved
    | CorrectResponseCorefTransformation of string
    /// Source sentence paraphrased
    | ClozeParaphraseTransformation of string
    /// Transformations source sentence has undergone to make cloze item
    | Transformations of string list
    /// Id of cluster (sentence); Phil requests this as tag in new API
    | ClusterId of int
    /// Id of item (cloze); Phil requests this as tag in new API
    | StimulusId of int
    /// Debug information
    | Trace of string
    /// Mark deprecated tags that may still be in parse
    | Deprecated of string


let StringToTag (keyValue : string) =
    let s = keyValue.Split(':')
    match s.[0] with
    | "weightGroup" -> s.[1] |> int |> WeightGroup
    | "orderGroup" -> s.[1] |> int |> OrderGroup
    | "OrderGroup" -> s.[1] |> int |> OrderGroup
    | "chunk" ->  s.[1] |> int |> OrderGroup //transitional
    | "default" ->  s.[1] |> Deprecated //transitional
    | _ -> "Error:" + keyValue |> Trace

///A clozable we have generated
type Clozable =
    {
        ///The words being clozed on, i.e. the cloze fill-in
        words : string[]
        ///Position of first cloze word in source sentence
        start : int
        ///Position of last cloze word in source sentence
        stop : int
        ///Collected messages reflecting our decision making; primarily for debug purposes (obj b/c some are Tag)
        trace : Tag list
        ///Probability assigned to this clozable (single probability)
        prob : float
        ///Tags we can use for cluster assignment
        tags : Tag list
    }
    //TODO switching to auto encoding/decoding b/c of the complexity of Tags and trace
    /// A custom decoder allows precise json decoding errors to be reported
    // static member Decoder : Thoth.Json.Decoder<Clozable>=
    //     Decode.object
    //         ( fun get ->
    //             {
    //                 words = get.Required.Field "words" (Decode.array Decode.string)
    //                 start = get.Required.Field "start" Decode.int
    //                 stop = get.Required.Field "stop" Decode.int
    //                 trace = get.Required.Field "trace" (Decode.array Decode.string)
    //                 prob = get.Required.Field "prob" Decode.float
    //                 tags = get.Required.Field "tags" (Decode.list Decode.string)
    //             }
    //         )

///All data we have collected and will use for final creation of cloze items
type InternalAPI =
    {
        sentences : SentenceAnnotation[]
        coreference : Coreference
        clozables : Clozable[][]
    }

/// 05 2022 Phil requested output of full stim file rather than original API
type MofactsResponse0522 =
    {
        correctResponse : string 
    }
type MofactsDisplay0522 =
    {
        clozeStimulus : string 
    }
type MofactsStim0522 =
    {
        response: MofactsResponse0522
        display: MofactsDisplay0522
        parameter: string
        tags : obj
    }
type MofactsCluster0522 =
    {
        stims : MofactsStim0522[]
    }
type MofactsSetspec0522 =
    {
        clusters : MofactsCluster0522[]
    }
type MofactsStimFile0522 =
    {
        setspec : MofactsSetspec0522
    }

[<Emit("new Map(Object.entries($0))")>]
let pojoToDictionary  (p:obj)  :System.Collections.Generic.Dictionary<string,obj> = jsNative

[<Emit("Object.fromEntries($0)")>]
let dictionaryToPojo  (p:System.Collections.Generic.Dictionary<string,obj>)  : obj= jsNative

/// convert the public api to stim fil 2022
/// This is bad design because we've already "rendered" the data as an API, but doing it this
/// way encapsulates variability
let PublicApiToStimFile (pa : ClozeAPI) =
    let stimFromClozableAPI (i:ClozableAPI) (display: string) response (addTags ) (deleteTags : string [])=
        // i.tags?stimulusId <- i.itemId
        // i.tags?clusterId <- iti.clusterId
        //tags were originally Tag list, but keyValueList has already collapsed to a pojo
        //so we cast to a dictionary, update the tags, and collapse to pojo again
        let newTags = pojoToDictionary i.tags
        newTags.Add("stimulusId", display + ":" + response |> hash  )
        newTags.Add("clusterId",i.clusterId)
        for (k,v) in addTags do
            if newTags.ContainsKey(k) then 
                newTags.[k] <- v
            else
                newTags.Add(k,v) |> ignore
        for k in deleteTags do
            newTags.Remove(k) |> ignore
        {
            response= { correctResponse=response }
            display= {clozeStimulus= display}
            parameter= "0,.70"
            tags = newTags |> dictionaryToPojo
        }
    /// group close by sentence,  each group is a cluster
    let clusters = pa.clozes |> Array.groupBy(fun i -> i.clusterId)
    /// for each cluster form stims
    let clusterRecords = 
        clusters
        |>  Array.map(fun (_,arr )->
            let stims = 
                arr |> Array.collect(fun i -> 
                    let temp  = ResizeArray()
                    // 6/1/22: Phil requests that if coref version exists, other versions be discarded
                    //coreference if it exists;  add indicator tag and remove other transformation tags
                    if i.tags?clozeCorefTransformation <> null then
                        let coref = stimFromClozableAPI i i.tags?clozeCorefTransformation i.tags?correctResponseCorefTransformation  [|("transformation","coreference")|] [|"clozeCorefTransformation";"clozeParaphraseTransformation";"correctResponseCorefTransformation"|]
                        temp.Add( coref )
                    else
                        //base cloze, remove transformation tags
                        let cloze =  stimFromClozableAPI i i.cloze i.correctResponse [|("transformation","none")|] [|"clozeCorefTransformation";"clozeParaphraseTransformation";"correctResponseCorefTransformation"|]
                        temp.Add(cloze )
                        //paraphrase if it exists; add indicator tag and remove other transformation tags
                        if i.tags?clozeParaphraseTransformation <> null then
                            let paraphrase =stimFromClozableAPI i i.tags?clozeParaphraseTransformation i.correctResponse [|("transformation","paraphrase")|] [|"clozeCorefTransformation";"clozeParaphraseTransformation";"correctResponseCorefTransformation"|]
                            temp.Add( paraphrase)
                    
                    temp.ToArray()
                )
            { stims = stims}
        )
    // return current stim format
    {
        setspec = {
            clusters= clusterRecords
        }
    }

///Based on the Heart Study
let EstimateDesiredSentences (sentences:string[] ) =
    let wordCount = sentences |> Seq.sumBy( fun sentence -> sentence.Split(' ').Length ) |> float
    //50% compression
    // let desiredSentences = (wordCount / 1000.0) * 25.0 |> int 
    //33% compression?
    let desiredSentences = (wordCount / 1000.0) * 12.0 |> int 
    desiredSentences

///Based on the Heart Study
let EstimateDesiredItems (desiredSentences : int) =
    //50% compression
    //let desiredItems = desiredSentences * 2
    //33% compression?
    let desiredItems = (desiredSentences |> float) * 1.3 |> int
    desiredItems

///Estimate desired sentences using arbitrary percentage
let EstimateDesiredSentencesFromPercentage (nlpJson : string) ( percentage : float ) =
    //Get a DocumentAnnotation if one wasn't passed in
    let da = nlpJson |> ofJson<DocumentAnnotation> 
    let desiredSentences = (da.sentences.Length |> float) * percentage
    //
    desiredSentences |> int
    
/// Get weight of all chains in a sentence (add the lengths together)
let GetTotalWeight ( coref : Coreference ) sen =
    // ** FALL 2019 VERSION **
    // Problems: 
    // 1. can have duplicates
    // 2. appears to be measuring length of spans not lengths of chains
    // sen.cor.clusters 
    // |> Array.collect( fun id -> 
    //     let cluster = coref.clusters.[id]
    //     cluster |> Array.map( fun c -> c.Length) 
    //     )
    // |> Array.sum
    // ****** TODO ***** below is techinically correct but we temporarily disable to generate features for analyzing fall 2019 data. Otherwise items may not match
    let totalWeight =
        sen.cor.clusters
        |> Array.distinct
        |> Array.sumBy( fun id -> coref.clusters.[id].Length )
    totalWeight

// -- Features for word level difficulty analysis, appended to trace ---
// COMPLETELY GENERIC trace features for analysis of difficulty of this word
/// distance from root feature
let getFeatureDistanceFromRoot start stop sen =
    let distanceFromRoot (index:int) (heads:int[]) =
        let mutable distance = 0
        let mutable i = index
        while i > -1 && heads.[i] <> 0 && distance < heads.Length do
            i <- heads.[i] - 1 //heads is 1 indexed
            distance <- distance + 1
        //
        distance
    let maxDistance = [|  start .. stop |] |> Seq.map( fun i -> distanceFromRoot i sen.dep.predicted_heads ) |> Seq.max
    // "@rootDistance:" + maxDistance.ToString()
    maxDistance |> RootDistance
/// distance from start of sentence feature
let getFeatureDistanceFromStart start =
    // "@startDistance:" + start.ToString()
    start |> StartDistance

// ANNOTATION SPECIFIC trace features
/// number of coref clusters/chains feature
let getFeatureCorefClusters sen =
    let clusters = sen.cor.clusters |> Array.distinct
    // "@corefClusters:" + clusters.Length.ToString()
    clusters.Length |> CorefClusters

/// total weight of sentence clusters feature: length of each cluster/chain in sentence
let getFeatureCorefClusterTotalWeight sen ( da : DocumentAnnotation ) =
    let totalWeight = GetTotalWeight da.coreference sen
    // ** FALL 2019 METHOD **
    // let totalWeight =
    //     sen.cor.clusters
    //     |> Array.distinct
    //     |> Array.sumBy( fun id -> da.coreference.clusters.[id].Length )
    // "@corefClusterTotalWeight:" + totalWeight.ToString()
    totalWeight |> CorefClusterTotalWeight

/// backward weight of sentence cluster feature: length of each cluster/chain in backward direction
let getFeatureCorefClusterBackwardWeight sen ( da : DocumentAnnotation )  =
    let weight = 
        sen.cor.clusters
        |> Array.distinct
        |> Array.collect( fun id -> da.coreference.clusters.[id] )
        |> Array.filter( fun c -> c.[1] < sen.cor.offset) //end of mention token index is before current sentence start token index
        |> Array.length
    // "@corefClusterBackwardWeight:" + weight.ToString()
    weight |> CorefClusterBackwardWeight

/// forward weight of sentence cluster feature: length of each cluster/chain in forward direction
let getFeatureCorefClusterForwardWeight sen ( da : DocumentAnnotation )  =
    let weight = 
        sen.cor.clusters
        |> Array.distinct
        |> Array.collect( fun id -> da.coreference.clusters.[id] )
        |> Array.filter( fun c -> c.[0] > sen.cor.offset + sen.srl.words.Length) //start of mention token index is after current sentence last token index
        |> Array.length
    // "@corefClusterForwardWeight:" + weight.ToString()
    weight |> CorefClusterForwardWeight

// -- END Features for word level difficulty analysis, appended to trace ---

/// Returns a Clozable for a modified NP given a sentence annotation and span of interest
let GetModifiedNPClozable sa startInit stopInit head traceInit =
    let trace = ResizeArray<Tag>()
    trace.AddRange(traceInit)
    trace.Add( getFeatureDistanceFromRoot startInit stopInit sa )
    trace.Add( getFeatureDistanceFromStart startInit)

    //check for insanity first. return empty if insane
    if startInit < 0 || stopInit >= sa.srl.words.Length then //|| head < 0 || head >= sen.srl.words.Length then
        trace.Add("CRITICAL: invalid clozable parameters for " + (sa |> toJson  ) |> Trace )
        { words=Array.empty; start=0; stop=0; trace= trace|> Seq.toList ; prob = 1.0 ; tags = sa.tags |> Array.map StringToTag |> Array.toList } 
    else
        //TODO: take another look at this logic now that we've created various utility functions for NLG that process AllenNLP dependencies
        //this is a pseudohead of the span. we can't use real heads because stanford dependencies aren't universal dependencies
        //therefore we must allow for functional/exocentric heads but find the pseudohead approximating universal dependencies 
        let h =
            match head with
            | Some(x) -> x
            | None -> 
                let stanfordHead = 
                    [|  startInit .. stopInit |] 
                    //get the predicted heads for each index; predicted heads are 1 indexed (root is 0)
                    |> Seq.map( fun i -> i,sa.dep.predicted_heads.[i])
                    //find tuple with a predicted head outside the span (because English is projective)
                    |> Seq.find( fun (_,h) -> h < startInit + 1 || h > stopInit + 1 )
                    //return the index
                    |> fst
                //require nominal pseudohead if stanfordHead is not nominal
                if sa.dep.pos.[ stanfordHead ].StartsWith("NN") |> not then
                    trace.Add( "head is not nominal" |> Trace)
                    //debug
                    // if sen.id > 140 then
                    //     printfn "debug"
                    //get subj/obj dependencies, take first
                    let argOption = [|  startInit .. stopInit |] |> Seq.map( fun i -> i,sa.dep.predicted_dependencies.[i]) |> Seq.tryFind( fun (_,h) -> h.Contains("subj") || h.Contains("obj")) 
                    //get nominal words, take last
                    let nnOption = [|  startInit .. stopInit |] |> Seq.map( fun i -> i,sa.dep.pos.[i]) |> Seq.rev |> Seq.tryFind( fun (_,h) -> h.StartsWith("NN")) 
                    match argOption,nnOption with
                    | Some(arg),_ ->  trace.Add( "WARNING: using first syntactic arg as pseudohead" |> Trace); arg |> fst
                    | _, Some(nn) -> trace.Add( "WARNING: using last nominal as pseudohead" |> Trace); nn |> fst
                    | _,_ -> trace.Add( "CRITICAL: clozable without nominal or arg, defaulting to given span" |> Trace); stopInit
                else
                    stanfordHead

        //NOTE: the logic here focuses on premodifiers + nominal not post modifying phrases
        //take preceeding modifiers of the nominal pseudohead that are nounish or JJ 
        let indices = [| startInit .. h |] |> Array.rev |> Array.takeWhile( fun i -> sa.dep.pos.[i].StartsWith("N") || sa.dep.pos.[i] =  "JJ" ) |> Array.rev
        let start, stop, words = 
            if indices.Length <> 0 then
                let start = indices.[0]
                let stop = indices |> Array.last
                start, stop, sa.srl.words.[ start .. stop ]
            else
                trace.Add("CRITICAL: stanford head yields empty span, defaulting to given span" |> Trace)
                startInit, stopInit, sa.srl.words.[ startInit .. stopInit ]

        let clozable = 
            { 
                words = words
                start = start
                stop =  stop
                trace = trace |> Seq.toList //.ToArray()
                //use the lowest freq word in the span
                prob = words |> Array.map WordFrequency.Get |> Array.min
                tags = sa.tags |> Array.map StringToTag |> Array.toList 
            }
        //
        clozable

/// GetClozable items for a sentence using syntactic, srl, and coref information. 
/// DocumentAnnotation level enhancements are inserted here for now (resolved coref and paraphrase)
/// TODO: filter non subj/obj/mod
let GetClozables ( da : DocumentAnnotation ) =
    // get analogues for sentences; some, like coref, require document-level context so belong here
    let corefresolvedSentences = da |> AllenNLP.resolveReferents

    da.sentences
    |> Array.map( fun sa -> 
        let clozable = new ResizeArray<Clozable>()
        //coref based cloze
        clozable.AddRange( 
            sa.cor.spans 
            |> Seq.mapi( fun i si -> 
                GetModifiedNPClozable sa si.[0] si.[1] None [ "coref" |> Trace; getFeatureCorefClusters sa; getFeatureCorefClusterTotalWeight sa da; getFeatureCorefClusterBackwardWeight sa da; getFeatureCorefClusterForwardWeight sa da ] )
        )
        //syntactic subj/obj
        clozable.AddRange(
            sa.dep.predicted_dependencies
            |> Seq.mapi( fun i x -> i,x)
            |> Seq.filter( fun (i,d) -> d.Contains("obj") || d.Contains("subj") || d.Contains("root") ) //root for copula constructions
            //must be noun (catches edge cases of relative clauses) TODO: allow pronoun if resolved to referent
            |> Seq.filter( fun (i,d) -> sa.dep.pos.[i].StartsWith("N") ) // || sen.dep.pos.[i] = "PRP" )
            |> Seq.map( fun (i,d) -> GetModifiedNPClozable sa i i (i|>Some) [ "dep" |> Trace; d |> Trace;  d |> SyntacticRole] )
        )
        //srl
        clozable.AddRange(
            sa.srl.verbs
            |> Seq.collect( fun pred ->
                pred.tags 
                |> Seq.mapi( fun i t -> i,t)
                |> Seq.filter( fun (_,t) -> t.Contains("ARG") ) 
                |> Seq.groupBy( fun (_,t) -> t.Substring(2)) //e.g. I-ARG0, so group by ARG0; we don't split b/c there are multiple hyphens
                |> Seq.map( fun (g,gtSeq) ->
                    let start = (gtSeq |> Seq.minBy fst) |> fst
                    let stop = (gtSeq |> Seq.maxBy fst) |> fst
                    GetModifiedNPClozable sa start stop None [ "srl" |> Trace ; pred.description |> Trace;  g |> SemanticRole ])
            )
        )

        // add tags; note some rely on da-level information and so really belong here and not elsewhere
        for i = 0 to clozable.Count - 1 do
            let tags = ResizeArray( clozable.[i].tags )
            tags.Add( sa |>  GetTotalWeight da.coreference |> Tag.SentenceWeight )
            tags.Add( clozable.[i].prob |> Tag.ClozeProbability )
            tags.Add( corefresolvedSentences.[ sa.id ] |> Tag.ClozeCorefTransformation)
            tags.Add( sa.sen |> Paraphrase.getCachedParaphrase |> Tag.ClozeParaphraseTransformation )
            //update clozable record
            clozable.[i] <- { clozable.[i] with tags = tags |> Seq.toList }

        clozable
    )

///To throw away sentences we don't know how to handle
let badSentenceRegex = System.Text.RegularExpressions.Regex( "(figure|table|section|clinical|application)\s+[0-9]",Text.RegularExpressions.RegexOptions.IgnoreCase)

/// Generates clozables for every sentence when given text and an optional JSON of DocumentAnnotation (a serialized parse)
/// Text input may be a JSON encoded string array or a string
/// NOTE: input may be empty if serialized parse is passed in.
let GetAllCloze (nlpJsonOption: string option) ( stringArrayJsonOption : string option) ( inputText : string )=
    promise {
        //Get a DocumentAnnotation if one wasn't passed in
        let! nlpResult = 
            match nlpJsonOption with
            | Some(nlpJson) -> nlpJson |> ofJson<DocumentAnnotation> |> Promisify 
            | None -> GetNLP stringArrayJsonOption inputText 

        match nlpResult with
        | Ok(da) ->
            let clozables = da |> GetClozables |> Array.map( fun ra -> ra.ToArray() )
            return Ok( {sentences = da.sentences; coreference = da.coreference; clozables = clozables} )
        | Error(e) -> 
            return Error(e)
    }

//Import node diff
type IChangeObject =
    abstract value : string with get
    abstract added : bool option with get
    abstract removed : bool option with get
type IJsDiff =
    abstract diffWords : string * string * obj -> IChangeObject array
[<ImportAll("diff")>]
let diff : IJsDiff = jsNative

/// 20200714 Generate data requested by Luke Eglington. Gets all clozables, resolves coref, and calculates some properties related to cloze generation/selection
let GetAllClozeLukeFormat20200714 (nlpJsonOption: string option) ( stringArrayJsonOption : string option) ( inputText : string )=
    promise {
        //Get a DocumentAnnotation if one wasn't passed in
        let! nlpResult = 
            match nlpJsonOption with
            | Some(nlpJson) -> nlpJson |> ofJson<DocumentAnnotation> |> Promisify 
            | None -> GetNLP stringArrayJsonOption inputText 

        match nlpResult with
        //sentence,cloze,sentenceWeight,clozeProbability
        | Ok(da) ->
            let clozables = da |> GetClozables |> Array.map( fun ra -> ra.ToArray() )
            // return Ok( {sentences = da.sentences; coreference = da.coreference; clozables = clozables} )
            let output = 
                da.sentences
                |> Array.mapi( fun i sa ->
                    let totalWeight = sa |>  GetTotalWeight da.coreference
                    clozables.[i]
                    |> Array.map( fun cl -> 
                        let cloze = cl.words |> String.concat " "
                        let sentence = sa.sen |> AllenNLP.removePrePunctuationSpaces
                        //map coreferents
                        let crOption = cl.tags |> List.choose( function | Tag.ClozeCorefTransformation sen -> Some(sen) | _ ->  None ) |> List.tryHead
                        match crOption with
                        | Some(cr) ->
                            //attempt to make coref resolve variant of item; will be identical to cr if clozeAnswer isn't found
                            //NOTE: we must handle resolution in clozeAnswer as well
                            //map of diffs: original cloze -> coref replacement cloze
                            let diffList = diff.diffWords( sa.sen, cr, {| ignoreCase = true |} )
                            let diffMap = 
                                [| 
                                    let removeList = ResizeArray<string>()
                                    let addList = ResizeArray<string>()
                                    for d in diffList do
                                        if d.removed.IsSome then 
                                            removeList.Add( d.value )
                                        elif d.added.IsSome then
                                            addList.Add( d.value )
                                        elif d.value.Trim() = "" then
                                            () //continue to accumulate over whitespace
                                        else
                                            if removeList.Count > 0 then
                                                yield removeList |> String.concat " ", (addList |> String.concat " ").Trim()
                                                removeList.Clear(); addList.Clear();
                                |]
                                |> Map.ofArray
                            //correct response
                            let crCloze =
                                match diffMap.TryFind cloze with
                                | Some(diffCloze) -> diffCloze
                                | None -> cloze

                            if sentence <> cr then
                                {|Sentence=cr; Cloze=crCloze ; SentenceWeight=totalWeight; ClozeProbability=cl.prob |} //; OriginalSen=sa.sen |}
                            else
                                {|Sentence=sentence; Cloze=cl.words |> String.concat " "; SentenceWeight=totalWeight; ClozeProbability=cl.prob |} // ; OriginalSen="" |}
                                
                        //no coref resolution
                        | _ -> 
                            {|Sentence=sentence; Cloze=cl.words |> String.concat " "; SentenceWeight=totalWeight; ClozeProbability=cl.prob |} //; OriginalSen="" |}
                    )
                )
            
            return Ok( output )
        | Error(e) -> 
            return Error(e)
    }
    
/// 20201218 Generate data requested by Luke Eglington. Gets all clozables, resolves coref, and calculates some properties related to cloze generation/selection
let GetAllClozeLukeFormat20201218 (nlpJsonOption: string option) ( stringArrayJsonOption : string option) ( inputText : string )=
    promise {
        //Get a DocumentAnnotation if one wasn't passed in
        let! nlpResult = 
            match nlpJsonOption with
            | Some(nlpJson) -> nlpJson |> ofJson<DocumentAnnotation> |> Promisify 
            | None -> GetNLP stringArrayJsonOption inputText 

        match nlpResult with
        //sentence,cloze,sentenceWeight,clozeProbability
        | Ok(da) ->
            let clozables = da |> GetClozables |> Array.map( fun ra -> ra.ToArray() )
            // return Ok( {sentences = da.sentences; coreference = da.coreference; clozables = clozables} )
            let output = 
                da.sentences
                |> Array.mapi( fun i sa ->
                    let totalWeight = sa |>  GetTotalWeight da.coreference
                    clozables.[i]
                    |> Array.map( fun cl -> 
                        let cloze = cl.words |> String.concat " "
                        let sentence = sa.sen |> AllenNLP.removePrePunctuationSpaces
                        //map coreferents
                        let crOption = cl.tags |> List.choose( function | Tag.ClozeCorefTransformation sen -> Some(sen) | _ ->  None ) |> List.tryHead
                        match crOption with
                        | Some(cr) ->
                            //attempt to make coref resolve variant of item; will be identical to cr if clozeAnswer isn't found
                            //NOTE: we must handle resolution in clozeAnswer as well
                            //map of diffs: original cloze -> coref replacement cloze
                            let diffList = diff.diffWords( sa.sen, cr, {| ignoreCase = true |} )
                            let diffMap = 
                                [| 
                                    let removeList = ResizeArray<string>()
                                    let addList = ResizeArray<string>()
                                    for d in diffList do
                                        if d.removed.IsSome then 
                                            removeList.Add( d.value )
                                        elif d.added.IsSome then
                                            addList.Add( d.value )
                                        elif d.value.Trim() = "" then
                                            () //continue to accumulate over whitespace
                                        else
                                            if removeList.Count > 0 then
                                                yield removeList |> String.concat " ", (addList |> String.concat " ").Trim()
                                                removeList.Clear(); addList.Clear();
                                |]
                                |> Map.ofArray
                            //correct response
                            let crCloze =
                                match diffMap.TryFind cloze with
                                | Some(diffCloze) -> diffCloze
                                | None -> cloze

                            if sentence <> cr then
                                {|Sentence=cr; Cloze=crCloze ; clusterId = hash sa; itemId = hash cloze; OriginalSentence=sentence; Tags=cl.tags @ cl.trace  |}
                            else
                                {|Sentence=sentence; Cloze=cl.words |> String.concat " "; clusterId = hash sa; itemId = hash cloze;  OriginalSentence=sentence; Tags=cl.tags @ cl.trace  |}
                                
                        //no coref resolution
                        | _ -> 
                            {|Sentence=sentence; Cloze=cl.words |> String.concat " "; clusterId = hash sa; itemId = hash cloze; OriginalSentence=sentence; Tags=cl.tags @ cl.trace |} 
                    )
                )
            
            return Ok( output )
        | Error(e) -> 
            return Error(e)
    }

/// Generate data for human evaluation. Gets all clozables  and calculates some properties related to cloze generation/selection
let GetAllClozeForHumanEvaluation2021061121 (nlpJsonOption: string option) ( stringArrayJsonOption : string option) ( inputText : string )=
    promise {
        //Get a DocumentAnnotation if one wasn't passed in
        let! nlpResult = 
            match nlpJsonOption with
            | Some(nlpJson) -> nlpJson |> ofJson<DocumentAnnotation> |> Promisify 
            | None -> GetNLP stringArrayJsonOption inputText 

        match nlpResult with
        //sentence,cloze,sentenceWeight,clozeProbability
        | Ok(da) ->
            let clozables = da |> GetClozables |> Array.map( fun ra -> ra.ToArray() )
            // return Ok( {sentences = da.sentences; coreference = da.coreference; clozables = clozables} )
            let output = 
                da.sentences
                |> Array.mapi( fun i sa ->
                    // sum length of all coref chains in sentence
                    let totalWeight = sa |>  GetTotalWeight da.coreference
                    // number of coref chains with length > 1
                    let chainsLengthTwoOrMore = 
                        sa.cor.clusters 
                        |> Array.map( fun id -> da.coreference.clusters.[id])
                        |> Array.filter( fun c -> c.Length > 1)
                        |> Array.length
                    clozables.[i]
                    |> Array.map( fun cl -> 
                        let cloze = cl.words |> String.concat " "
                        let sentence = sa.sen |> AllenNLP.removePrePunctuationSpaces
                        {|SentenceWeight=totalWeight; Chains=chainsLengthTwoOrMore; ClozeProbability=cl.prob; Sentence=sentence; SentenceIndex=i; ClozeStart=cl.start; ClozeStop=cl.stop; Cloze=cloze; clusterId = hash sa; itemId = hash cloze;  OriginalSentence=sentence; Tags=cl.tags @ cl.trace  |}
                    )
                )
            
            return Ok( output )
        | Error(e) -> 
            return Error(e)
    }


//TODO: since we are not allowing truly long fill ins (~4 words long) prefering longer once here 
//may be causing us to throw items away. To prevent that, the length restriction need to be here as well
/// Remove overlapping cloze, preferring larger spans
/// a starts before b, but they overlap
/// b starts before a, but they overlap
/// a entirely inside b
/// b entirely inside a
/// all covered with a.start < b.end && b.start < a.end;
let RemoveOverlappingClozables (clozables : Clozable[] ) =
    // let clozablesOut = ResizeArray<Clozable>(clozables)
    // TODO: magic length parameter should be passed in not hard coded
    let clozablesOut = ResizeArray<Clozable>(clozables |> Array.filter( fun cl -> cl.words.Length < 4 ))
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

/// Spaceless equality:  Removes all spaces from text strings, e.g. "the car" -> "thecar", to resolve string comparision issues where only the space is different
let se( text1 : string ) ( text2 : string ) =
    text1.Replace(" ","") = text2.Replace(" ","")

/// Spaceless INequality:  Removes all spaces from text strings, e.g. "the car" -> "thecar", to resolve string comparision issues where only the space is different
let nse( text1 : string ) ( text2 : string ) =
    text1.Replace(" ","") <> text2.Replace(" ","")


/// Perform item transformations (coref resolution/paraphrase) as applicable
/// Store these alternatives as tags
let MakeItemWithTranformations (sa:SentenceAnnotation) (cl:Clozable) =
    //default item
    let blank = [| for _ = cl.start to cl.stop do yield "__________" |] |> String.concat " "
    let sentence = Array.copy sa.srl.words |> String.concat " "
    let cloze = cl.words |> String.concat " "
    let item = System.Text.RegularExpressions.Regex.Replace( sentence, @"\b" + cloze + @"\b", blank) |> AllenNLP.removePrePunctuationSpaces

    let crOption = cl.tags |> List.choose( function | Tag.ClozeCorefTransformation sen -> Some(sen) | _ ->  None ) |> List.tryHead
    let paOption = cl.tags |> List.choose( function | Tag.ClozeParaphraseTransformation sen -> Some(sen) | _ ->  None ) |> List.tryHead
    let tags = cl.tags |> List.filter( function | Tag.ClozeCorefTransformation  _ | Tag.ClozeParaphraseTransformation _ -> false | _ -> true ) 
    match crOption,paOption with
    | Some(cr),Some(pa) ->
        //attempt to make paraphrase item; will be identical to pa if clozeAnswer isn't found
        let paItem = System.Text.RegularExpressions.Regex.Replace( pa, @"\b" + cloze + @"\b", blank)
        //attempt to make coref resolve variant of item; will be identical to cr if clozeAnswer isn't found
        //NOTE: we must handle resolution in clozeAnswer as well
        //map of diffs: original -> replacement
        let diffList = diff.diffWords( sa.sen, cr, {| ignoreCase = true |} )
        let diffMap = 
            [| 
                let removeList = ResizeArray<string>()
                let addList = ResizeArray<string>()
                for d in diffList do
                    if d.removed.IsSome then 
                        removeList.Add( d.value )
                    elif d.added.IsSome then
                        addList.Add( d.value )
                    elif d.value.Trim() = "" then
                        () //continue to accumulate over whitespace
                    else
                        if removeList.Count > 0 then
                            yield removeList |> String.concat " ", (addList |> String.concat " ").Trim()
                            removeList.Clear(); addList.Clear();
            |]
            |> Map.ofArray
        let crCloze =
            match diffMap.TryFind cloze with
            | Some(diffCloze) -> diffCloze
            | None -> cloze
        let crItem = System.Text.RegularExpressions.Regex.Replace( cr, @"\b" + crCloze + @"\b", blank) 
            // |> String.mapi( fun i c -> match i with | 0 -> (Char.ToUpper(c)) | _ -> c) //uppercase first letter as needed

        //handle cases for transformations
        //simplified version
        let tempTags = tags |> ResizeArray
        // coref transformation is different from original and we can make an item
        if (nse cr sa.sen) && (nse cr crItem) then
            tempTags.Add( Tag.ClozeCorefTransformation(crItem) ); tempTags.Add( Tag.CorrectResponseCorefTransformation(crCloze) )
        // paraphrase transformation is different from original and we can make an item
        if (nse pa sa.sen) && (nse pa paItem) then
            tempTags.Add( Tag.ClozeParaphraseTransformation(paItem) )

        //return clean transformation tags
        item, cloze, tempTags |> Seq.toList

        // //all equal, no transformations, purge existing transformation tags
        // if cr = sa.sen && pa = sa.sen then
        //     item, cloze, tags
        // //pa only, pa item succeeded, make item with pa tag
        // elif cr = sa.sen && pa <> sa.sen && pa <> paItem then
        //     item, cloze, Tag.ClozeParaphraseTransformation(paItem)::tags
        // //cr only, cr item succeeded, make item with cr tag
        // elif cr <> sa.sen && pa = sa.sen && cr <> crItem then
        //     item, cloze, Tag.ClozeCorefTransformation(crItem)::Tag.CorrectResponseCorefTransformation(crCloze)::tags
        // //pa and cr, make items for both
        // elif cr <> sa.sen && pa <> sa.sen then
        //     let tempTags = tags |> ResizeArray
        //     if pa <> paItem then tempTags.Add( Tag.ClozeParaphraseTransformation(paItem) )
        //     if cr <> crItem then tempTags.Add( Tag.ClozeCorefTransformation(crItem) ); tempTags.Add( Tag.CorrectResponseCorefTransformation(crCloze) )
        //     item, cloze, tempTags |> Seq.toList
        // //something went wrong, keep basic item and purge transformation tags
        // else
        //     item, cloze, tags
    //partial or total transformation failure, keep basic item and purge transformation tags
    | _ , _ -> item, cloze, tags

/// Return an item as a sentence with words blanked out, together with the corresponding words
/// Modified to blank out ALL occurances of clozeAnswer, even if they appear multiple times
/// Modified to perform optional item transformations (coref resolution/paraphrase)
let MakeItem (sa:SentenceAnnotation) (cl:Clozable)=
    let blank = [| for _ = cl.start to cl.stop do yield "__________" |] |> String.concat " "
    let sentence = Array.copy sa.srl.words |> String.concat " "
    let cloze = cl.words |> String.concat " "
    let item = System.Text.RegularExpressions.Regex.Replace( sentence, @"\b" + cloze + @"\b", blank) |> AllenNLP.removePrePunctuationSpaces
    item,cloze

// Original version
// let MakeItem (sa:SentenceAnnotation) (cl:Clozable)=
//     let itemWords = Array.copy sa.srl.words
//     for i = cl.start to cl.stop do
//         itemWords.[i] <- "__________"
//     itemWords |> String.concat " ", cl.words |> String.concat " "

// TODO: CLEANING OUT PARENTHESES IN CLEANTEXT PROBABLY LIMITS OR UNDOES THIS
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


/// Returns select clozables given a target number by ranking clozables and returning top ranked.
/// Since target numbers may be impossible to satisfy, does not guarantee returning the target quantities.
/// Accepts optional serialized NLP, #sentences/items, and chunks of text (e.g. subsections). Any of these override the default if present.
/// inputText (as a single chunk) is the default of stringArrayJsonOption and so may be empty if stringArrayJsonOption exists.
let GetSelectCloze (nlpJsonOption: string option) (sentenceCountOption: int option) (itemCountOption: int option) (doTrace : bool) ( stringArrayJsonOption : string option) ( inputText : string ) = 
    promise{
        let! allClozeResult = GetAllCloze nlpJsonOption stringArrayJsonOption inputText

        match allClozeResult with
        | Ok( allCloze ) ->
            
              //Estimate how many items we want if this wasn't specified
            let sentenceCount = 
                match sentenceCountOption with
                | Some(sentenceCount) -> sentenceCount
                | None -> allCloze.sentences |> Array.map( fun x -> x.sen) |> EstimateDesiredSentences 
            let itemCount =
                match itemCountOption with
                | Some(itemCount) -> itemCount
                | None -> sentenceCount |> EstimateDesiredItems
            
            //hard filter: we exclude sentences that don't meet these criteria:
            // 3 corefs with chain length > 2
            // TODO: We don't have the discourse parser yet, so we can't apply the "sentence contains nucleus" constraint

            //partition sentences into those meeting strict criteria and the rest
            let hardFilterTuples,remainingTuples =
                allCloze.sentences
                |> Array.mapi( fun i s -> s,allCloze.clozables.[i])
                //Filter sentences we don't know how to handle (A&P specific)
                |> Array.filter( fun (sa,_) -> sa.sen |> badSentenceRegex.IsMatch |> not )
                //Handle overlapping cloze
                |> Array.map( fun ( sa, cl ) -> sa, cl |> RemoveOverlappingClozables )
                //Remove impossibly hard cloze (>3 fill ins; TODO: get theoretical justification; assume pseudohead is problem)
                |> Array.map( fun ( sa, cls ) -> sa, cls |> Array.filter( fun cl -> cl.words.Length < 4 ) )
                //Filter sentences with no clozables
                |> Array.filter( fun (_,cl) -> cl.Length > 0 )
                //Remove duplicates defined by senId and clozeWords; guards against case where clozeWords are repeated in a sentence
                |> Array.map( fun (sa,cls) -> sa, cls |> Array.distinctBy( fun cl -> cl.words ) )
                //Apply strict criteria to create partition
                |> Array.toList
                |> List.partition( fun (sa,_) ->
                    let chainsLengthTwoOrMore = 
                        sa.cor.clusters 
                        |> Array.map( fun id -> allCloze.coreference.clusters.[id])
                        |> Array.filter( fun c -> c.Length > 1)
                    chainsLengthTwoOrMore.Length > 2 //we have > 2 chains with length > 1
                )

            //Get the cloze tuples to make our items from. NOTE: we let desiredSentences take priority over desiredItems here. TODO decide which has priority, sentences or items.
            let clozeTuples =
                let hardFilterSentenceCount = hardFilterTuples.Length
                //let hardFilterItemCount = hardFilterTuples |> Seq.collect snd |> Seq.length
                //if hard filter produced at least as many items and sentences as we need, sort by total weight and take what we need. TODO use other criteria besides weight?
                if hardFilterSentenceCount > sentenceCount then //&& hardFilterItemCount > itemCount then
                    hardFilterTuples |> List.sortByDescending( fun (sa,_) -> sa |> GetTotalWeight allCloze.coreference ) |> List.take sentenceCount |> List.sortBy( fun (s,_) -> s.id )
                //otherwise use all hard filter tuples and add top remainingTuples to get desired counts
                else
                    hardFilterTuples @ (remainingTuples  |> List.sortByDescending( fun (sa,_) -> sa |>  GetTotalWeight allCloze.coreference ) |> List.take (sentenceCount-hardFilterTuples.Length ) )
                    |> List.sortBy( fun (s,_)-> s.id )

            //We need to generate the desired # items BUT we also must take at least 1 from each sentence
            //Step 1. Partition clozables into min probability per sentence and rest per sentence
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

            //Item tagging: rank sentences by totalweight, group into sets of 30, then create map of sentence to importance tags
            let importantClozeMap =
                allClozableMap
                |> Map.toArray
                |> Array.sortByDescending( fun (sa,_) -> sa |>  GetTotalWeight allCloze.coreference )
                |> Array.collect( fun (sa,cl) -> cl |> List.toArray )
                |> Array.chunkBySize 30 //TODO arbitrary size here; need theoretical justification
                |> Array.mapi( fun i cl -> 
                    cl |> Array.map( fun cl -> cl,i )
                )
                |> Array.collect id
                |> Map.ofArray
                
            // let test =
            //     allClozableMap
            //     |> Map.toArray
            //     |> Array.sortByDescending( fun (sa,_) -> sa |>  GetTotalWeight allCloze.coreference )
            //     |> Array.collect( fun (sa,cl) -> cl |> Array.ofList )
            //     |> Array.windowed 30 //TODO arbitrary size here; need theoretical justification

            //Create acronym map to expand "correct" answers for items
            let input = 
                match stringArrayJsonOption with
                | Some(chunksJson) -> chunksJson |> ofJson<string[]>
                | None -> [| inputText |]
            let acronymMap = input |> String.concat " " |> GetAcronymMap |> ofJson<Map<string,string>> 
            
            //Package for external API
            let sentences = ResizeArray<SentenceAPI>()
            let clozes = ResizeArray<ClozableAPI>()

            allCloze.sentences
            |> Seq.iter( fun sa ->
                match allClozableMap.TryFind(sa) with
                | None -> sentences.Add( { sentence = sa.sen; clusterId = (hash sa); hasCloze = false} )
                | Some(clozables) -> 
                    sentences.Add( { sentence = sa.sen; clusterId = (hash sa); hasCloze = true} )
                    clozables |> Seq.iter( fun cl -> 
                        // //append the weight group to our tags and convert the list of tags into an object literal
                        // let tags =  
                        //     (importantClozeMap.[cl] |> WeightGroup) :: cl.tags @ cl.trace  
                        //     |> List.choose( fun t ->   //filter junk tags
                        //         match t with 
                        //         | Deprecated(x) -> None 
                        //         | Trace(x) -> None
                        //         | _ -> Some(t) )
                        //     |> keyValueList CaseRules.LowerFirst
                        // let cloze,correctResponse = MakeItem sa cl
                        let cloze,correctResponse, clTags = MakeItemWithTranformations sa cl
                        //append the weight group to our tags and convert the list of tags into an object literal
                        let tags =  
                            (importantClozeMap.[cl] |> WeightGroup) :: clTags @ cl.trace  
                            |> List.choose( fun t ->   //filter junk tags
                                match t with 
                                | Deprecated(x) -> None 
                                | Trace(x) -> None
                                | _ -> Some(t) )
                            |> keyValueList CaseRules.LowerFirst
                        //insert any alternative correct responses here
                        let correctResponses = 
                            match acronymMap.TryFind(correctResponse) with
                            | Some( acronym ) -> correctResponse + "|" + acronym // + if doTrace then "~" + (cl.trace |> String.concat "~") else ""
                            | None -> correctResponse //+ if doTrace then "~" + (cl.trace |> String.concat "~") else ""

                        clozes.Add( { cloze=cloze; clusterId = hash sa; itemId = hash cloze; correctResponse = correctResponses; tags=tags} ) 
                    )
                )
            return Ok( {sentences=sentences.ToArray();clozes=clozes.ToArray()} )
        | Error(e) -> 
            return Error(e)
    }

/// Designed for Phil to test; based around the node code for generating stims
// var parse = fs.readFileSync(filename, 'utf8');
// var sections = fs.readFileSync( filename.replace(/parse/g,"sectionlist"), 'utf8');
// var timeout = 4000 //delay buffer
// var sentenceCount = ClozeAPI.EstimateDesiredSentencesFromPercentage(parse,parseFloat(percentage))
// var itemCount = sentenceCount * 2
// ClozeAPI.GetSelectCloze(parse,sentenceCount,itemCount,false,sections,"").then( (result) => {
let GetSelectClozePercentage (percentage : float) ( stringArrayJsonOption : string option) (nlpJsonOption: string option) ( inputText : string )  =
    promise {
        //Get a DocumentAnnotation
        // let! nlpResult = GetNLP stringArrayJsonOption inputText
        //Get a DocumentAnnotation if one wasn't passed in
        let! nlpResult = 
            match nlpJsonOption with
            | Some(nlpJson) -> nlpJson |> ofJson<DocumentAnnotation> |> Promisify 
            | None -> GetNLP stringArrayJsonOption inputText 

        match nlpResult with
        | Ok(da) ->
            let sentenceCount = int((da.sentences.Length |> float) * percentage) 
            let itemCount = sentenceCount * 2
            let! clozeResult= GetSelectCloze (da |> toJson |> Some) (sentenceCount |> Some) (itemCount |> Some) true stringArrayJsonOption inputText
            // let clozables = da |> GetClozables |> Array.map( fun ra -> ra.ToArray() )
            match clozeResult with
            | Ok(clozeAPI) ->
                let stimFile = clozeAPI |> PublicApiToStimFile //|> toJson
                return Ok( stimFile)//clozeAPI)//stimFile )
            | Error(e) -> 
                return Error(e)
        | Error(e) -> 
            return Error(e)
    }

///Reverse a string. Test of fable library imports
let DoSimpleComputation( input : string ) =
    input.ToCharArray() |> Seq.rev |> Seq.cast |> String.concat ""