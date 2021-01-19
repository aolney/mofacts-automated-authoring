module Paraphrase
// The vision of this module is to connect to a paraphrase service and retrieve paraphrases per sentence.
// For historical and performance reasons, we have both precomputed (cached) and live versions. The cached 
// versions currently exist in the stim files.

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch

//for node compatibility
importSideEffects "isomorphic-fetch"

let paraphraseEndpoint = "https://paraphrase.olney.ai/api/"

/// Get a list of paraphrases (TODO: could update with optional n-best parameter (k : int option) )
/// TODO: select the "best" paraphrase in terms of diversity, etc (may not be top-k result)
let getParaphrases(sentence: string): JS.Promise<Result<string,FetchError>> =
    promise {
        return! Fetch.tryPost( paraphraseEndpoint + "getParaphrase", {| sentence=sentence;  |}, caseStrategy = SnakeCase)
    }

/// a map from source sentence to backtranslated paraphrase
let mutable backtranslation = Map.empty

/// Call to initialize the backtranslations, passing in tab-separated value text
let InitializeParaphraseCache ( text : string ) =
    try
        backtranslation <-  
            text.Split('\n')
            |> Seq.map( fun l -> 
                let s = l.Split('\t') 
                s.[2],s.[3] //file format is chapterid\tsentenceid\tsource\ttarget
            )
            |> Map.ofSeq
        promise{ return Ok( null ) }
    with
    | e -> promise{ return Error( e.Message ) }

/// Using the source sentence as key (sa.sen), get a paraphrase
let getCachedParaphrase( sentence : string) =
    match backtranslation.TryFind sentence with
    | Some(paraphrase) -> paraphrase
    | None -> sentence