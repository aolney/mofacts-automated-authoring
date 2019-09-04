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

type SentenceRequest =
    {
        sentence : string
    }

type DocumentRequest =
    {
        document : string
    }

let endpoints =
    {
        SRL = "http://141.225.12.235:8000/predict/semantic-role-labeling"
        Coreference = "http://141.225.12.235:8000/predict/coreference-resolution"
        DependencyParser = "http://141.225.12.235:8000/predict/dependency-parsing"
    }

///Function template for gets from AllenNLP. Requires on-campus IP address. We assume Promise will give better meteor compatibility
let GetAllenNLP (input:obj) endpoint =
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
    GetAllenNLP { document = input } endpoints.Coreference

///Get SRL from AllenNLP.
let GetSRL( input: string ) =
    GetAllenNLP { sentence = input } endpoints.SRL

///Get a parse from AllenNLP.
let GetParse( input: string ) =
    GetAllenNLP { sentence = input } endpoints.DependencyParser

///Reverse a string. Test of fable library imports
let DoSimpleComputation( input : string ) =
    input.ToCharArray() |> Seq.rev |> Seq.cast |> String.concat ""