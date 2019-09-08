module Process

open System
open Fable.Core
open Thoth.Json //for Json; might be cleaner way
open Fable.SimpleHttp

//Fable 2 transition
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)

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
        clusters : int[][]
        document : string[]
        predicted_antecedents : int[]
        top_spans : int[][]
    }

type SentenceAnnotation = 
    {
        sen : string
        srl : SRLResult
        dep : DependencyParseResult
    }

type DocumentAnnotation = 
    {
        sentences : SentenceAnnotation[]
        coreference : CoreferenceResult
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
    //NOTE: this throws away status
    |> Seq.map( fun sentence -> sentence |> service  |> Promise.map snd) 
    |> Promise.all

///Call all NLP functions for a piece of text
let GetNLP( input : string) =
    // let sentencesPromise = input |> GetSentences
    promise {
        //start with a promise for sentences, throwing away status
        let! sentences = input |> GetSentences |> Promise.map( snd >> ofJson<string[]> )
        //call various services with sentences
        let! srlJsons = sentences |> GetForSentences GetSRL
        let! depJsons = sentences |> GetForSentences GetDependencyParse
        // let! corJsons = sentences |> GetForSentences GetCoreference
        //construct the composite NLP object for each sentence
        let sentenceAnnotations = 
            seq {
                for i = 0 to sentences.Length - 1 do
                    let srl = srlJsons.[i] |> ofJson<SRLResult>
                    let dep = depJsons.[i] |> ofJson<DependencyParseResult>
                    // let cor = corJsons.[0] |> ofJson<CoreferenceResult>
                    yield { sen=sentences.[i] ; srl=srl; dep=dep }
            }
            |> Seq.toArray

        //call global services not requiring sentences
        let! corJson = input |> GetCoreference |> Promise.map snd
        let cor = corJson |> ofJson<CoreferenceResult>

        let documentAnnotation = { sentences=sentenceAnnotations; coreference=cor}
        //old
        // //for each nlp service that uses sentences get a per sentence result, throwing away status
        // let nlpPromises = seq{ 
        //     for sentence in sentences do
        //         let srlJson = sentence |> GetSRL  |> Promise.map snd
        //         yield srlJson
        // } 
        // //convert the sequence of promises to a promise of sequence and await
        // let! result = nlpPromises |> Promise.all
        //return as json b/c our return type is string elsewhere
        return 1,documentAnnotation |> toJson 
    }
    // sentence |> GetSRL  |> Promise.map snd
    //     return sentencesJson |> ofJson<string[]>
    // }
    //map to promises for NLP annotations
    // |> Promise.( fun sentence ->
    //     GetSRL sentence
    // )

///Reverse a string. Test of fable library imports
let DoSimpleComputation( input : string ) =
    input.ToCharArray() |> Seq.rev |> Seq.cast |> String.concat ""