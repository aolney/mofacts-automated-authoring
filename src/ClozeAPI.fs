module ClozeAPI

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json //for Json; might be cleaner way
//TODO: possibly replace with this: https://github.com/thoth-org/Thoth.Fetch
// open Fable.SimpleHttp

open AllenNLP

// Domain model

//-- Public ------------------------------
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
        words : string[]
        start : int
        stop : int
        /// Collected messages reflecting our decision making; primarily for debug purposes (obj b/c some are Tag)
        trace : Tag list
        prob : float
        /// Tags we can use for cluster assignment
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
let GetModifiedNPClozable sen startInit stopInit head traceInit =
    let trace = ResizeArray<Tag>()
    trace.AddRange(traceInit)
    trace.Add( getFeatureDistanceFromRoot startInit stopInit sen )
    trace.Add( getFeatureDistanceFromStart startInit)

    //check for insanity first. return empty if insane
    if startInit < 0 || stopInit >= sen.srl.words.Length then //|| head < 0 || head >= sen.srl.words.Length then
        trace.Add("CRITICAL: invalid clozable parameters for " + (sen |> toJson  ) |> Trace )
        { words=Array.empty; start=0; stop=0; trace= trace|> Seq.toList ; prob = 1.0 ; tags = sen.tags |> Array.map StringToTag |> Array.toList } 
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
                    |> Seq.map( fun i -> i,sen.dep.predicted_heads.[i])
                    //find tuple with a predicted head outside the span (because English is projective)
                    |> Seq.find( fun (_,h) -> h < startInit + 1 || h > stopInit + 1 )
                    //return the index
                    |> fst
                //require nominal pseudohead if stanfordHead is not nominal
                if sen.dep.pos.[ stanfordHead ].StartsWith("NN") |> not then
                    trace.Add( "head is not nominal" |> Trace)
                    //debug
                    // if sen.id > 140 then
                    //     printfn "debug"
                    //get subj/obj dependencies, take first
                    let argOption = [|  startInit .. stopInit |] |> Seq.map( fun i -> i,sen.dep.predicted_dependencies.[i]) |> Seq.tryFind( fun (_,h) -> h.Contains("subj") || h.Contains("obj")) 
                    //get nominal words, take last
                    let nnOption = [|  startInit .. stopInit |] |> Seq.map( fun i -> i,sen.dep.pos.[i]) |> Seq.rev |> Seq.tryFind( fun (_,h) -> h.StartsWith("NN")) 
                    match argOption,nnOption with
                    | Some(arg),_ ->  trace.Add( "WARNING: using first syntactic arg as pseudohead" |> Trace); arg |> fst
                    | _, Some(nn) -> trace.Add( "WARNING: using last nominal as pseudohead" |> Trace); nn |> fst
                    | _,_ -> trace.Add( "CRITICAL: clozable without nominal or arg, defaulting to given span" |> Trace); stopInit
                else
                    stanfordHead

        //NOTE: the logic here focuses on premodifiers + nominal not post modifying phrases
        //take preceeding modifiers of the nominal pseudohead that are nounish or JJ 
        let indices = [| startInit .. h |] |> Array.rev |> Array.takeWhile( fun i -> sen.dep.pos.[i].StartsWith("N") || sen.dep.pos.[i] =  "JJ" ) |> Array.rev
        let start, stop, words = 
            if indices.Length <> 0 then
                let start = indices.[0]
                let stop = indices |> Array.last
                start, stop, sen.srl.words.[ start .. stop ]
            else
                trace.Add("CRITICAL: stanford head yields empty span, defaulting to given span" |> Trace)
                startInit, stopInit, sen.srl.words.[ startInit .. stopInit ]

        let clozable = 
            { 
                words = words
                start = start
                stop =  stop
                trace = trace |> Seq.toList //.ToArray()
                //use the lowest freq word in the span
                prob = words |> Array.map WordFrequency.Get |> Array.min
                tags = sen.tags |> Array.map StringToTag |> Array.toList 
            }
        //
        clozable

/// GetClozable items for a sentence using syntactic, srl, and coref information. TODO: filter non subj/obj/mod
let GetClozable ( da : DocumentAnnotation ) =
    da.sentences
    |> Array.map( fun sen -> 
        let clozable = new ResizeArray<Clozable>()
        //coref based cloze
        clozable.AddRange( 
            sen.cor.spans 
            |> Seq.mapi( fun i si -> GetModifiedNPClozable sen si.[0] si.[1] None [ "coref" |> Trace; getFeatureCorefClusters sen; getFeatureCorefClusterTotalWeight sen da; getFeatureCorefClusterBackwardWeight sen da; getFeatureCorefClusterForwardWeight sen da  ] )
        )
        //syntactic subj/obj
        clozable.AddRange(
            sen.dep.predicted_dependencies
            |> Seq.mapi( fun i x -> i,x)
            |> Seq.filter( fun (i,d) -> d.Contains("obj") || d.Contains("subj") || d.Contains("root") ) //root for copula constructions
            //must be noun (catches edge cases of relative clauses) TODO: allow pronoun if resolved to referent
            |> Seq.filter( fun (i,d) -> sen.dep.pos.[i].StartsWith("N") ) // || sen.dep.pos.[i] = "PRP" )
            |> Seq.map( fun (i,d) -> GetModifiedNPClozable sen i i (i|>Some) [ "dep" |> Trace; d |> Trace;  d |> SyntacticRole] )
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
                    GetModifiedNPClozable sen start stop None [ "srl" |> Trace ; pred.description |> Trace;  g |> SemanticRole ])
            )
        )

        clozable
    )

///To throw away sentences we don't know how to handle
let badSentenceRegex = System.Text.RegularExpressions.Regex( "(figure|table|section|clinical|application)\s+[0-9]",Text.RegularExpressions.RegexOptions.IgnoreCase)

/// Generates clozables for every sentence when given a block of text and an optional JSON of DocumentAnnotation (a serialized parse)
/// NOTE: input may be empty if serialized parse is passed in.
let GetAllCloze (nlpJsonOption: string option) ( chunksJsonOption : string option) ( inputText : string )=
    promise {
        //Get a DocumentAnnotation if one wasn't passed in
        let! nlpResult = 
            match nlpJsonOption with
            | Some(nlpJson) -> nlpJson |> ofJson<DocumentAnnotation> |> Promisify 
            | None -> GetNLP chunksJsonOption inputText 

        match nlpResult with
        | Ok(da) ->
            let clozables = da |> GetClozable |> Array.map( fun ra -> ra.ToArray() )
            return Ok( {sentences = da.sentences; coreference = da.coreference; clozables = clozables} )
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

// TODO: CLEANING OUT PARENTHESES IN CLEANTEXT PROBABLY LIMITS OR UNDOES THIS
// TODO: USE SIMILAR APPROACH FOR COREFERENCE CHAINS?
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
/// inputText (as a single chunk) is the default of chunksJsonOption and so may be empty if chunksJsonOption exists.
let GetSelectCloze (nlpJsonOption: string option) (sentenceCountOption: int option) (itemCountOption: int option) (doTrace : bool) ( chunksJsonOption : string option) ( inputText : string ) = 
    promise{
        let! allClozeResult = GetAllCloze nlpJsonOption chunksJsonOption inputText

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
                |> Array.toList
                //Apply strict criteria to create partition
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
                match chunksJsonOption with
                | Some(chunksJson) -> chunksJson |> ofJson<string[]>
                | None -> [| inputText |]
            let acronymMap = input |> String.concat " " |> GetAcronymMap |> ofJson<Map<string,string>> 
            
            //Package for external API
            let sentences = ResizeArray<SentenceAPI>()
            let clozes = ResizeArray<ClozableAPI>()

            allCloze.sentences
            |> Seq.iter( fun sa ->
                match allClozableMap.TryFind(sa) with
                | None -> sentences.Add( { sentence = sa.sen; itemId = (hash sa); hasCloze = false} )
                | Some(clozables) -> 
                    sentences.Add( { sentence = sa.sen; itemId = (hash sa); hasCloze = true} )
                    clozables |> Seq.iter( fun cl -> 
                        //append the weight group to our tags and convert the list of tags into an object literal
                        let tags =  
                            (importantClozeMap.[cl] |> WeightGroup) :: cl.tags @ cl.trace  
                            |> List.choose( fun t ->   //filter junk tags
                                match t with 
                                | Deprecated(x) -> None 
                                | Trace(x) -> None
                                | _ -> Some(t) )
                            |> keyValueList CaseRules.LowerFirst
                        let cloze,correctResponse = MakeItem sa cl
                        //insert any alternative correct responses here
                        let correctResponses = 
                            match acronymMap.TryFind(correctResponse) with
                            | Some( acronym ) -> correctResponse + "|" + acronym // + if doTrace then "~" + (cl.trace |> String.concat "~") else ""
                            | None -> correctResponse //+ if doTrace then "~" + (cl.trace |> String.concat "~") else ""
                        //construct tags here

                        
                        clozes.Add( { cloze=cloze; itemId = hash sa; clozeId = hash cloze; correctResponse = correctResponses; tags=tags} ) 
                    )
                )
            return Ok( {sentences=sentences.ToArray();clozes=clozes.ToArray()} )
        | Error(e) -> 
            return Error(e)
    }

///Reverse a string. Test of fable library imports
let DoSimpleComputation( input : string ) =
    input.ToCharArray() |> Seq.rev |> Seq.cast |> String.concat ""