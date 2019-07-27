module Process

open System
open Fable.Core
// open Browser
// open Fable.Core.JS
open Fable.SimpleHttp

///Get a parse from AllenNLP. Requires on-campus IP address. We assume Promise will give better meteor compatibility
let GetParse( input: string ) =
    async {
        let requestData = "{\"sentence\": \"" + input + "\"}"
        let endPoint = "http://141.225.12.235:8000/predict/semantic-role-labeling"
        let! response = 
            Http.request endPoint
            |> Http.method POST
            |> Http.content (BodyContent.Text requestData)
            |> Http.header (Headers.contentType "application/json")
            |> Http.send
        return response.statusCode,response.responseText
    }
    |> Async.StartAsPromise

///Reverse a string. Test of fable library imports
let DoSimpleComputation( input : string ) =
    input.ToCharArray() |> Seq.rev |> Seq.cast |> String.concat ""