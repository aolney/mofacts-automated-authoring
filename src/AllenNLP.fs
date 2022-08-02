module AllenNLP

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch

//for node compatibility
importSideEffects "isomorphic-fetch"

//Fable 2 transition 
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)

/// Fake a promise return type with success indicator
let Promisify ( input:'t ) =
    promise{ return Ok(input) }

/// Fake a promise return type with success indicator
// let Promisify ( input:string ) =
//     promise{ return "ok",input}

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
        TextualEntailment : string
    }

///////////////////////////////////////////////////////////////////////
/// RESULTS

///AllenNLP SRL verb
type SRLVerb =
    {
        verb : string
        description : string
        tags : string[]
    }

///AllenNLP SRL result
type SRL =
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
type DependencyParse =
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
type Coreference =
    {
        /// A list of clusters. A cluster is a list of spans. A span is a tuple of onset/offset word indices for the mention (allen uses list instead of tuple)
        clusters : int[][][]
        /// A flat array of words in the document, used to associate indices in the spans with word tokens
        document : string[]
        /// UNUSED. For each top span, the index of the most likely antecedent, with -1 representing none
        predicted_antecedents : int[]
        /// UNUSED. spans that survived pruning. A span is a tuple of onset/offset word indices for the mention (allen uses list instead of tuple)
        top_spans : int[][]
    }

///Coreference information remaped to the sentence level
type SentenceCoreference =
    {
        ///offset into the document, useful for indexing back into clusters
        offset: int
        ///start/stop word in sentence normalized to sentence indices
        spans : int[][]
        ///for each span, the id of chain; maps to CoreferenceResult clusters
        clusters : int[]
    }

///Sentence level annotation combining  annotations from all NLP services
type SentenceAnnotation = 
    {
        /// Position in text
        id : int
        /// Arbitrary tags assigned to annotation, e.g. source information
        tags : string[]
        sen : string
        srl : SRL
        dep : DependencyParse
        cor : SentenceCoreference
    }

///Document level annotation combining annotations from all NLP services
type DocumentAnnotation = 
    {
        sentences : SentenceAnnotation[]
        coreference : Coreference
    } with
    static member CreateEmpty() = { sentences = [||] ; coreference = !!null }

type Entailment =
    {
        h2p_attention : float[][]
        hypothesis_tokens : string[]
        label_logits : float[]
        label_probs : float[]
        p2h_attention : float[][]
        premise_tokens : string[]
    } with
    static member CreateEmpty() = { h2p_attention  = [||]; hypothesis_tokens = [||]; label_logits  = [||]; label_probs  = [||]; p2h_attention = [||]; premise_tokens = [||]; }


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

//AllenNLP request where hypothesis and premise strings are sent
type EntailmentRequest =
    {
        hypothesis : string
        premise : string
    }

///Endpoints for NLP services
let endpoints =
    {
        //http requires on-campus IP address
        // SRL = "http://141.225.12.235:8002/predict/semantic-role-labeling"
        // Coreference = "http://141.225.12.235:8002/predict/coreference-resolution"
        // DependencyParser = "http://141.225.12.235:8002/predict/dependency-parsing"
        // SentenceSplitter = "http://141.225.12.235:8001/sents"
        SRL = "https://allennlp.olney.ai/predict/semantic-role-labeling"
        Coreference = "https://allennlp.olney.ai/predict/coreference-resolution"
        DependencyParser = "https://allennlp.olney.ai/predict/dependency-parsing"
        SentenceSplitter = "https://spacy.olney.ai/sents"
        TextualEntailment = "https://allennlp.olney.ai/predict/textual-entailment"
        
    }

/// Function template for POSTs. 
/// We assume Promise will give better meteor compatibility. 
/// ~~Passing in the encoding to avoid CORS preflight on spacy.~~
// let PostAPI (input:obj) endpoint encoding =
    // promise { do! Promise.sleep 500 } |> Promise.start //attempted throttle
    // async {
    //     do! Async.Sleep 200 //attempted throttle
    //     let requestData = input |> toJson 
    //     let! response = 
    //         Http.request endpoint
    //         |> Http.method POST
    //         |> Http.content (BodyContent.Text requestData)
    //         |> Http.header (Headers.contentType encoding)
    //         |> Http.send
    //     return response.statusCode,response.responseText
    // }
    // |> Async.StartAsPromise
    // promise {
    //     return! Fetch.post( endpoint, input, caseStrategy = SnakeCase)
    // }
    
///Get coreferences from AllenNLP
let GetCoreference( input: string ) : JS.Promise<Result<Coreference,FetchError>> =
    // PostAPI { document = input } endpoints.Coreference "application/json"
    promise {
        return! Fetch.tryPost( endpoints.Coreference , { document = input }, caseStrategy = SnakeCase)
    }

///Get SRL from AllenNLP.
let GetSRL( input: string ) : JS.Promise<Result<SRL,FetchError>> =
    // PostAPI { sentence = input } endpoints.SRL "application/json"
    promise {
        return! Fetch.tryPost( endpoints.SRL , { sentence = input }, caseStrategy = SnakeCase)
    }

///Get a parse from AllenNLP.
let GetDependencyParse( input: string ) : JS.Promise<Result<DependencyParse,FetchError>> =
    // PostAPI { sentence = input } endpoints.DependencyParser "application/json"
    promise {
        return! Fetch.tryPost( endpoints.DependencyParser , { sentence = input }, caseStrategy = SnakeCase)
        // return! Fetch.tryPost( endpoints.DependencyParser , input, properties = [Fetch.Types.RequestProperties.Mode Fetch.Types.RequestMode.Cors], caseStrategy = SnakeCase)
        
    }

///Get split sentences from Spacy. HACK: passing in this encoding to avoid CORS preflight 
let GetSentences( input: string ) : JS.Promise<Result<string[],FetchError>> =
    // PostAPI { text = input; model = "en" } endpoints.SentenceSplitter "text/plain"
    promise {
        // return! Fetch.tryPost( endpoints.SentenceSplitter , input, properties = [Fetch.Types.RequestProperties.Mode Fetch.Types.RequestMode.Cors], headers = [Fetch.Types.HttpRequestHeaders.ContentType "text/plain"], caseStrategy = SnakeCase)
        // return! Fetch.tryPost( endpoints.SentenceSplitter , input, properties = [Fetch.Types.RequestProperties.Mode Fetch.Types.RequestMode.Cors], caseStrategy = SnakeCase)
        return! Fetch.tryPost( endpoints.SentenceSplitter , { text = input; model = "en" }, caseStrategy = SnakeCase)
    }

///Call services with sentences to generate a seq of promises; remap to promise of seq
let GetForSentences (service: string -> JS.Promise<Result<'t,FetchError>>) (sentences:string[]) =
    sentences 
    |> Seq.map( fun sentence -> sentence |> service )
    |> Promise.all

///Get a textual entailment from AllenNLP.
let GetTextualEntailment( premise: string ) (hypothesis :string): JS.Promise<Result<Entailment,FetchError>> =
    promise {
        return! Fetch.tryPost( endpoints.TextualEntailment, { hypothesis = hypothesis; premise=premise }, caseStrategy = SnakeCase)       
    }


let RegexReplace (pattern : string) (replacement:string) (input:string) =
    System.Text.RegularExpressions.Regex.Replace( input, pattern, replacement )
    
let Split ( pattern : char ) (input: string) = input.Split( pattern 
)
///Per sentence text cleaning. NOTE regexes are A&P specific!
let CleanText input =
    input 
    //contractions are split by parser (NOTE not exhaustive); normalize them here to avoid headaches later (TODO use rules for this/a better way)
    |> RegexReplace @"\bisn't\b" "is not"
    |> RegexReplace @"\baren't\b" "are not"
    |> RegexReplace @"\bwasn't\b" "was not"
    |> RegexReplace @"\bweren't\b" "were not"
    |> RegexReplace @"\bwon't\b" "will not"
    |> RegexReplace @"\bcan't\b" "can not"
    |> RegexReplace @"\bcouldn't\b" "could not"
    |> RegexReplace @"\don't\b" "do not"
    |> RegexReplace @"\bdoesn't\b" "does not"
    |> RegexReplace @"\didn't\b" "did not"
    |> RegexReplace @"\haven't\b" "have not"
    |> RegexReplace @"\hasn't\b" "has not"
    |> RegexReplace @"\hadn't\b" "had not"
    |> RegexReplace @"\that's\b" "that is"
    |> RegexReplace "Page[ 0-9]+" ""
    |> RegexReplace "\(fig[^\)]+\)" ""
    |> RegexReplace "\(see[^\)]+\)" ""
    |> RegexReplace "\(note[^\)]+\)" ""
    |> RegexReplace "\([^\)]+\)" "" //This one is a bit strong: we remove ALL parenthetical material (creates a mess with cloze)
    //These two added by request; similarly are very strong like the above
    |> RegexReplace "\[[^\]]+\]" "" //This one is a bit strong: we remove ALL parenthetical material (creates a mess with cloze)
    |> RegexReplace "\{[^\}]+\}" "" //This one is a bit strong: we remove ALL parenthetical material (creates a mess with cloze)
    |> RegexReplace "\s+" " "
    |> RegexReplace " \.$" "." //replacements leave spaces before final period
    //TODO: possible replacement for above
    // |> RegexReplace "\s+([\.\?!;,])" "$1" //replacements leave spaces before final punctuation
    |> transliteration.transliterate

/// Call all NLP functions either for a JSON array of text, where each element represents a semantic grouping 
/// (i.e. chapter section) of the same type, OR for a single piece of text. If both are passed, chunks have priority.
/// Service failures create meaningful error messages and stop execution.
let GetNLP( stringArrayJsonOption : string option ) ( inputText : string )=
    promise {
        let chunks = 
            match stringArrayJsonOption with
            | Some( chunksJson ) -> chunksJson |> ofJson<string[]>
            //treat inputText as a single chunk
            | None -> [| inputText |]

        //Each chunk is a collection of sentences
        let! chunkSentenceResults = 
            chunks
            |> Array.map( fun chunk -> chunk |> CleanText |> GetSentences  ) //|> Promise.map( snd >> ofJson<string[]> ) )
            |> Promise.all
        
        //check result (better way to do this? railway oriented or computation expressions?)
        let allOK (resultsArr : Result<'t,'e>[] ) = resultsArr |> Array.forall( fun r -> match r with | Ok(r) -> true | Error(e) -> false )
        let resultsToType (resultsArr : Result<'t,'e>[] )  = resultsArr |> Array.choose( fun r -> match r with | Ok(r) -> Some(r) | Error(_) -> None ) 
        let resultsToError (resultsArr : Result<'t,'e>[] )  = resultsArr |> Array.choose( fun r -> match r with | Ok(r) -> None | Error(e) -> Some(e) ) 

        if chunkSentenceResults |> allOK then
            //create lists of tags and sentences
            let tags,sentences = 
                //generate tag tuples from chunks, using chunk position as id
                chunkSentenceResults
                |> resultsToType
                |> Array.mapi ( fun i chunk -> 
                    chunk |> Array.map( fun sen -> 
                        [| "orderGroup:" + i.ToString() |] ,sen  ) //053120 camelcased OrderGroup
                ) 
                |> Array.collect id //flatten to single list of sentences
                |> Array.unzip // unzip to parallel lists of tags and sentences

            //call other services and check for errors before proceeding
            let! corResult = sentences |> String.concat " " |> GetCoreference
            let! srlResults = sentences |> GetForSentences GetSRL
            let! depResults = sentences |> GetForSentences GetDependencyParse

            //if no errors
            if [| corResult |] |> allOK && srlResults |> allOK && depResults |> allOK then
                
                let cor = [| corResult |] |> resultsToType |> Array.head
                let srls = srlResults |> resultsToType
                let deps = depResults |> resultsToType

                //map to first token id of span, entire span, and cluster index
                let tokenIdCorefMap =
                    cor.clusters
                    |> Seq.mapi( fun i c -> 
                        c |> Seq.map( fun span -> span.[0],(span,i ))
                    )
                    |> Seq.collect id
                    |> Map.ofSeq

                //construct the composite NLP object for each sentence
                let sentenceAnnotations = 
                    seq {
                        let mutable wordIndexOffset = 0
                        for i = 0 to sentences.Length - 1 do
                            let srl = srls.[i] 
                            let dep = deps.[i]
                            //map the corefs to this particular sentence by checking if each word is in a coref span
                            let spans = ResizeArray<int[]>()
                            let clusters = ResizeArray<int>()
                            for j = 0 to srl.words.Length - 1 do
                                match tokenIdCorefMap.TryFind( j + wordIndexOffset ) with
                                | Some(span,clusterIndex) ->
                                    spans.Add( [| span.[0] - wordIndexOffset; span.[1] - wordIndexOffset |] ) //offset indices to zero for sentence
                                    clusters.Add( clusterIndex )
                                | None -> ()

                            yield { id=i; tags = tags.[i]; sen=sentences.[i] ; srl=srl; dep=dep; cor={offset=wordIndexOffset; spans=spans.ToArray();clusters=clusters.ToArray()} }

                            wordIndexOffset <- wordIndexOffset + srl.words.Length //all services should agree on tokens in sentence, so using srl here is arbitrary

                    }
                    |> Seq.toArray

                let documentAnnotation = { sentences=sentenceAnnotations; coreference=cor}
                //return "ok",documentAnnotation |> toJson 
                return Ok(documentAnnotation)
                 
            //we have errors in cor/dep/srl
            else
                //collect all errors; avoid duplicates
                let errorPayload = ResizeArray<string>()
                errorPayload.AddRange( [| corResult |] |> resultsToError |> Array.map (sprintf "coreference error: %A") |> Array.distinct )
                errorPayload.AddRange( srlResults |> resultsToError |> Array.map (sprintf "srl error: %A") |> Array.distinct )
                errorPayload.AddRange( depResults |> resultsToError |> Array.map (sprintf "dependency parse error: %A") |> Array.distinct )
                //return "error", errorPayloads |> String.concat "\n"
                return Error(errorPayload |> String.concat "\n")
        //we have errors in sentence
        else
            let errorPayload = (chunkSentenceResults |> resultsToError |> Array.map (sprintf "sentence split error: %A") |> Array.distinct )
            //return "error", formattedErrors |> String.concat "\n"
            return Error(errorPayload |> String.concat "\n")
    }

///////////////////////////////////////////////////////////////////////
/// Post processing
/// ///////////////////////////////////////////////////////////////////
/// Match a non-word character preceeded by a space and followed by whitespace
let prePunctuationSpaceRegex = System.Text.RegularExpressions.Regex(@" ([^\w\s]+)")
/// Remove pre punctuation spaces using regular expression
let removePrePunctuationSpaces ( input : string ) =
    prePunctuationSpaceRegex.Replace(input, "$1")

/// Use the dependency collapser to simplify dependencies
let collapseDependencies (sa : SentenceAnnotation) = 
    let ruleTokens = 
        sa.dep.words 
        |> Array.mapi( fun i w -> 
            DependencyCollapser.Rules.Token.Create( i, w, sa.dep.pos.[i], sa.dep.predicted_dependencies.[i], sa.dep.predicted_heads.[i])
        ) |> Array.toList

    let dependencies, dependenciesCC = DependencyCollapser.Collapser.CollapseTokens( ruleTokens )
    //
    dependenciesCC

/// Get a list of dependent indices from start index (0-base index)
let getDependentIndices ( start : int ) ( sa : SentenceAnnotation ) =
    let dependents = ResizeArray<int>()
    for i = 0 to sa.dep.predicted_heads.Length - 1 do
        //predicted_heads are 1-base indexed
        let mutable hbar = sa.dep.predicted_heads.[i] - 1
        //walk predicted heads until we either match the start index or bottom out in ROOT (-1)
        while hbar <> start && hbar <> -1 do
            hbar <- sa.dep.predicted_heads.[hbar] - 1
        //add where we stopped as a dependency only if it is a dependent of start or start itself
        if hbar = start || i = start then dependents.Add(i)
    //if copula, then predicate is root but we don't want the whole sentence; HACK: chop off everything including copula
    if sa.dep.predicted_heads.[start] = 0 && not <| sa.dep.pos.[start].StartsWith("VB") then 
        //find copula index (if it exists)
        let copulaIndex = dependents |> Seq.mapi( fun i d -> i,d ) |> Seq.tryFindIndex( fun (i,d) -> sa.dep.predicted_dependencies.[i] = "cop" )
        match copulaIndex with
        //do not allow copula to be the last word; get everything after copula
        | Some(i) when i < dependents.Count - 1 -> dependents |> Seq.skip 1 |> Seq.toArray
        | _ -> Array.empty

        // dependents |> Seq.mapi( fun i d -> i,d ) |> Seq.skipWhile( fun (i,d) -> sa.dep.predicted_dependencies.[i] <> "cop" ) |> Seq.skip 1 |> Seq.map snd |> Seq.toArray
        
    else
        dependents.ToArray()

/// Convert SRL BIO tags to a map with key as tag without BIO prefix and value a list of tag/index tuples for that tag
let srlArgToIndexMap (srlTags : string[]) =
    srlTags 
    |> Array.mapi( fun i t -> t.Substring( t.IndexOf("-") + 1 ),i)
    |> Array.groupBy fst
    |> Map.ofArray

let srlArgToIndexMapWithCollapsedReferents (srlTags : string[]) =
    srlTags 
    |> Array.mapi( fun i t -> 
        //absorb referents, e.g. R-ARG0, into their arguments
        t.Substring( t.LastIndexOf("-") + 1 ),i )
    |> Array.groupBy fst
    |> Map.ofArray

// NOTE: A more direct, though simplified, version of CGA3 follows
/// Get the subject predicate index of the parse
/// Had to modify from LTH b/c of different formalism
/// NSUBJ seems pretty reliable but can sometimes be WH: It was John who (nsubj) came ; in this case "It" is chosen
/// OLD method backed off to first noun in sentence on failure; for now we prefer to fail without a kludge
let getSubjectIndex ( sa : SentenceAnnotation ) = 
    let rootIndex = sa.dep.predicted_heads |> Array.findIndex( fun h -> h = 0) //assuming there is always a root...
    //the first nsubj preceding the root
    sa.dep.predicted_dependencies.[0 .. rootIndex] |> Array.tryFindIndexBack( fun h -> h.StartsWith("nsubj") ) //allow for nsubjpass

/// Returns the index of a "be" or copular verb when it can be viewed as the root (anti Stanford, which views copular complement as root)
/// This check subsumes verb chaining because verb chains are broken and otherwise marked as "aux" rather than "cop"
let getBeRootIndex ( sa : SentenceAnnotation ) =
    let rootIndex = sa.dep.predicted_heads |> Array.findIndex( fun h -> h = 0) //assuming there is always a root...
    //the first copular child of the root
    sa.dep.predicted_heads 
    |> Array.tryFindIndex( fun h -> h = rootIndex && sa.dep.predicted_dependencies.[h] = "cop" )

/// Returns the first aux for inversion; e.g. John has been eating pie -> What has John been eating?
let getInvertAuxIndex ( sa : SentenceAnnotation ) =
    let rootIndex = sa.dep.predicted_heads |> Array.findIndex( fun h -> h = 0) //assuming there is always a root...
    //the first copular child of the root
    sa.dep.predicted_heads 
    |> Array.tryFindIndex( fun h -> h = rootIndex && sa.dep.predicted_dependencies.[h] = "aux" )

/// Get the root predicate index of the parse
/// Had to modify from LTH b/c of different formalism
/// If ROOT is VB, then take first ~~DOBJ~~ leftmost dependent of ROOT : John kissed (root) Mary (dobj) on the head
/// If ROOT is anything else, then take ROOT: It was John (root) who came ; Sally was happy (root) to see her
let getPredicateIndex ( sa : SentenceAnnotation ) = 
    let rootIndex = sa.dep.predicted_heads |> Array.findIndex( fun h -> h = 0) //assuming there is always a root...
    if sa.dep.pos.[rootIndex].StartsWith("VB") then
        //the first child of the verb that is a dobj (starting from the beginning shouldn't matter)
        sa.dep.predicted_heads 
        //make zero indexed
        |> Array.mapi( fun i h -> i,h - 1)
        |> Array.tryFindIndex( fun (i,h) -> h = rootIndex && i > rootIndex  )
        // restricting to dobj seems too limiting
        // |> Array.tryFindIndex( fun (i,h) -> h = rootIndex && sa.dep.predicted_dependencies.[i] = "dobj" )
    else
        rootIndex |> Some //NOTE: SPAN WILL DOMINATE S

/// Get all object indices. Follows logic of getPredicateIndex
let getObjectIndices ( sa : SentenceAnnotation ) = 
    let rootIndex = sa.dep.predicted_heads |> Array.findIndex( fun h -> h = 0) //assuming there is always a root...
    if sa.dep.pos.[rootIndex].StartsWith("VB") then
        //the first child of the verb that is a dobj (starting from the beginning shouldn't matter)
        sa.dep.predicted_heads 
        //make zero indexed
        |> Array.mapi( fun i h -> i,h - 1)
        |> Array.filter( fun (i,h) -> 
            // h = rootIndex && //this would require them to be children of ROOT
            (sa.dep.predicted_dependencies.[i] = "dobj" || sa.dep.predicted_dependencies.[i]  = "iobj" ) )
        |> Array.map( fun (i,h) -> Some <| i)
        // |> Array.map snd
    else
        Array.empty

/// Get the root of the span as the leftmost token whose root is outside the span.
let getRootOfSpan ( start : int) (stop : int) ( sa : SentenceAnnotation ) = 
    //make zero indexed, because start/stop are
    let spanHeads = sa.dep.predicted_heads.[start..stop] |> Array.map( fun h -> h - 1)
    let spanHeadIndex = spanHeads |> Array.findIndex( fun h -> h  < start || h  > stop ) 
    start + spanHeadIndex //offset

let firstLetterLower (input : string) =
    input.Substring(0,1).ToLower() + input.Substring(1)

/// Get referent label that best represents coreferents in chain
let resolveReferents ( da : DocumentAnnotation ) =
    //Map from cluster id to all sentences with that cluster
    let clusterSentenceMap =
        da.sentences
        |> Array.mapi( fun i s -> 
            s.cor.clusters
            |> Array.mapi( fun j c -> c,i,j )
            )
        |> Array.collect id
        // |> Array.distinct
        |> Array.groupBy( fun (c,_,_) -> c )
        |> Map.ofArray

    let demonstrativeRegex = System.Text.RegularExpressions.Regex( "(this|that|these|those)", System.Text.RegularExpressions.RegexOptions.IgnoreCase )
    //A span is pronominal if it starts with a PTB pronoun or a manual demonstrative pronoun
    let spanIsPronominal (sa : SentenceAnnotation ) ( span : int[] ) =
        sa.dep.pos.[  span.[0] ].StartsWith("PRP") ||
        demonstrativeRegex.IsMatch( sa.dep.words.[  span.[0] ] )

    let spanIsPronominalNotPossessive (sa : SentenceAnnotation ) ( span : int[] ) =
        sa.dep.pos.[  span.[0] ] = "PRP" ||
        demonstrativeRegex.IsMatch( sa.dep.words.[  span.[0] ] )

    //We could try to "splice" a new sentence annotation using the original and resolved referent, but 
    //it seems safer to stick to the word level and then reparse to get annotations as needed unless performance becomes a concern
    let resolvedSentences =
        da.sentences
        |> Array.map( fun sa -> 
            //for each cluster, 
            //get the associated sentences and spans; 
            //determine the span that best represents the coreferents in the chain using POS; replace tokens with that
            let clusterReferents =
                sa.cor.clusters
                |> Array.map( fun clusterId -> 
                    //Nominal referent phrases for this cluster; may be empty
                    let nominalReferents = 
                        clusterSentenceMap.[clusterId] 
                        |> Array.map( fun (_,sentenceId,spanId)-> (sentenceId,spanId) )
                        |> Array.sortBy fst
                        |> Array.choose( fun (sentenceId,spanId) ->
                            //span is [start; stop]
                            let span =  da.sentences.[sentenceId].cor.spans.[spanId]
                            //check if any one element in the span is a noun
                            // let hasNoun = da.sentences.[sentenceId].dep.pos.[ span.[0] .. span.[1] ] |> Array.exists( fun pos -> pos.StartsWith("NN") )
                            //if the span contains a noun, return the associated words
                            // if hasNoun then
                            //     Some(da.sentences.[sentenceId].dep.words.[ span.[0] .. span.[1] ] |> String.concat " " )
                            // else
                            //     None
                            // as above, but preserve the sentence id to do locality-sensitive resolution
                            if spanIsPronominal da.sentences.[sentenceId] span then
                                None
                            else
                                let referent = da.sentences.[sentenceId].dep.words.[ span.[0] .. span.[1] ] |> String.concat " "
                                //check if span started a sentence; if so lowercase
                                if span.[0] = 0 then
                                    Some(sentenceId, referent |> firstLetterLower )
                                else
                                    Some(sentenceId, referent )
                        )
                    //Return the earliest nominal referent for this chain as the resolved referent (this is OK for short chains, but for long chains with any error, does not work well)
                    //nominalReferents |> Array.tryHead

                    //Display all possibilities. Difficult to read for long chains
                    //nominalReferents |> String.concat "+" |> Some
                    //Display all possibilities, counting duplicates. Helps but still hard to read
                    // nominalReferents |> Array.countBy id |> sprintf "%A" |> Some

                    // Return the most often occuring referent in the chain. Ok for short chains, but for long chains with any error does not work well.
                    // match nominalReferents |> Array.countBy id with
                    // | [||]-> None
                    // | x -> x |> Array.maxBy snd |> sprintf "%A" |> Some

                    // Disallow resolution outside sentence if sentence contains a possible referent
                    let referentInSentence = if nominalReferents |> Array.exists( fun (i,w) -> i = sa.id ) then true else false

                    // Return the closest preceeding referent (locality-sensitive resolution)
                    match referentInSentence, nominalReferents with
                    | _, [||]-> None
                    | true, _ -> None
                    | false, x -> 
                        match x |> Array.sortBy fst |> Array.tryFindBack( fun (i,w) -> i < sa.id ) with //Could allow equality in case best referent is already within the sentence, but this seems to introduce more errors than not
                        | None -> None
                        | Some(_,w) -> Some(w)
                   
                )
            // Transform the original sentence by splicing in these referents -> IFF span does not contain nominal AND referent is Some
            let indexedWords = sa.dep.words |> Array.copy 
            // Set a flag so we only resolve one word in the sentence
            let mutable notResolved = true
            for i = 0 to sa.cor.spans.Length - 1 do
                // has noun is not enough; does not address cases like "this system"
                // let spanHasNoun = sa.dep.pos.[  sa.cor.spans.[i].[0] ..  sa.cor.spans.[i].[1] ] |> Array.exists( fun pos -> pos.StartsWith("NN") )
                // let spanPronominal = 
                //     sa.dep.pos.[  sa.cor.spans.[i].[0] ].StartsWith("PRP") ||
                //     demonstrativeRegex.IsMatch( sa.dep.words.[  sa.cor.spans.[i].[0] ] )

                let originalWords = sa.dep.words.[  sa.cor.spans.[i].[0] ..  sa.cor.spans.[i].[1] ] |> String.concat " "
                // if we have a clusterReferent for this span
                if spanIsPronominalNotPossessive sa sa.cor.spans.[i] && clusterReferents.[i].IsSome && notResolved then
                    // replace the first word of span with clusterReferent (which could be multiword)
                    indexedWords.[ sa.cor.spans.[i].[0] ] <- clusterReferents.[i].Value
                    //for debugging, see original + alternatives
                    // indexedWords.[ sa.cor.spans.[i].[0] ] <- "(" + originalWords + "|" + clusterReferents.[i].Value + ")"
                    // blank out the remaining words in the span
                    for j = sa.cor.spans.[i].[0] + 1 to sa.cor.spans.[i].[1] do //TODO check end inclusive
                        indexedWords.[ j ] <- ""
                    notResolved <- false
                // if there is no cluster referent, keep the original words
                else
                    ()

            // Return the indexed words as a string, without blanks
            indexedWords 
            |> Array.filter( fun w -> w.Length > 0 ) 
            |> String.concat " " 
            |> removePrePunctuationSpaces
            |> String.mapi( fun i c -> match i with | 0 -> (Char.ToUpper(c)) | _ -> c) //uppercase first letter as needed

        )
    //
    resolvedSentences

/// All in one that parses text and resolves reference
let ResolveTextReferents ( inputText : string ) =
    promise {
        //Get a DocumentAnnotation if one wasn't passed in
        let! nlpResult = GetNLP None inputText 

        match nlpResult with
        | Ok(da) ->
            let resolvedSentences = da |> resolveReferents
            return Ok( {|resolvedSentences = resolvedSentences; documentAnnotation = da|} ) //NOTE: anonymous type
        | Error(e) -> 
            return Error(e)
    }