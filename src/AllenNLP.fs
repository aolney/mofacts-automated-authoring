module AllenNLP

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch
//for Json; might be cleaner way
//TODO: possibly replace with this: https://github.com/thoth-org/Thoth.Fetch
// open Fable.SimpleHttp

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
        //http requires on-campus IP address
        SRL = "http://141.225.12.235:8002/predict/semantic-role-labeling"
        Coreference = "http://141.225.12.235:8002/predict/coreference-resolution"
        DependencyParser = "http://141.225.12.235:8002/predict/dependency-parsing"
        SentenceSplitter = "http://141.225.12.235:8001/sents"
        // TODO: every once and a while getting a bogus cors error from caddy, but suspect it is due to caddy getting flooded
        // SRL = "https://allennlp.olney.ai/predict/semantic-role-labeling"
        // Coreference = "https://allennlp.olney.ai/predict/coreference-resolution"
        // DependencyParser = "https://allennlp.olney.ai/predict/dependency-parsing"
        // SentenceSplitter = "https://spacy.olney.ai/sents"
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
        return! Fetch.tryPost( endpoints.Coreference , input, caseStrategy = SnakeCase)
    }

///Get SRL from AllenNLP.
let GetSRL( input: string ) : JS.Promise<Result<SRL,FetchError>> =
    // PostAPI { sentence = input } endpoints.SRL "application/json"
    promise {
        return! Fetch.tryPost( endpoints.SRL , input, caseStrategy = SnakeCase)
    }

///Get a parse from AllenNLP.
let GetDependencyParse( input: string ) : JS.Promise<Result<DependencyParse,FetchError>> =
    // PostAPI { sentence = input } endpoints.DependencyParser "application/json"
    promise {
        return! Fetch.tryPost( endpoints.DependencyParser , input, caseStrategy = SnakeCase)
    }

///Get split sentences from Spacy. HACK: passing in this encoding to avoid CORS preflight 
let GetSentences( input: string ) : JS.Promise<Result<string[],FetchError>> =
    // PostAPI { text = input; model = "en" } endpoints.SentenceSplitter "text/plain"
    promise {
        // return! Fetch.tryPost( endpoints.SentenceSplitter , input, properties = [Fetch.Types.RequestProperties.Mode Fetch.Types.RequestMode.Cors], headers = [Fetch.Types.HttpRequestHeaders.ContentType "text/plain"], caseStrategy = SnakeCase)
        return! Fetch.tryPost( endpoints.SentenceSplitter , input, caseStrategy = SnakeCase)
    }

///Call services with sentences to generate a seq of promises; remap to promise of seq
let GetForSentences (service: string -> JS.Promise<Result<'t,FetchError>>) (sentences:string[]) =
    sentences 
    |> Seq.map( fun sentence -> sentence |> service )
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
    |> RegexReplace "\([^\)]+\)" "" //This one is a bit strong: we remove ALL parenthetical material (creates a mess with cloze)
    |> RegexReplace "\s+" " "
    |> RegexReplace " \.$" "." //replacements leave spaces before final period
    |> transliteration.transliterate

/// Call all NLP functions either for chunks of text, where each chunk represents a semantic grouping 
/// (i.e. chapter section) of the same type, OR for a single piece of text. If both are passed, chunks have priority.
/// Service failures create meaningful error messages and stop execution.
let GetNLP( chunksJsonOption : string option ) ( inputText : string )=
    promise {
        let chunks = 
            match chunksJsonOption with
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
                        [| "OrderGroup:" + i.ToString() |] ,sen  ) 
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
