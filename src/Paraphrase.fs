module Paraphrase
// The vision of this module is to connect to a paraphrase service and retrieve paraphrases per sentence.
// However, at this stage of development, the paraphrases are being pregenerated using backtranslation.
// This is not a problem since the runtime system is only using stims precomputed offline anyways.

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch

//for node compatibility
importSideEffects "isomorphic-fetch"


/// NOTE THIS IS NOT CURRENTLY FUNCTIONAL 
let paraphraseEndpoint = "https://paraphrase.olney.ai/api/"

/// NOTE NOT CURRENTLY FUNCTIONAL Get a list of paraphrases with optional n-best parameter k
let getParaphrases(sentence: string)(k : int option) : JS.Promise<Result<string,FetchError>> =
    promise {
        return! Fetch.tryPost( paraphraseEndpoint + "getParaphrases", {| sentence=sentence; k=k;  |}, caseStrategy = SnakeCase)
    }

/// a map from source sentence to backtranslated paraphrase
let mutable backtranslation = Map.empty

/// Call to initialize the backtranslations, passing in tab-separated value text
let InitializeBacktranslations ( text : string ) =
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
/// ?TODO key off sa.srl.words?
let getParaphrase( sentence : string) =
    //TODO: call getParaphrases and select the "best" paraphrase in terms of diversity, etc (may not be top-k result)
    match backtranslation.TryFind sentence with
    | Some(paraphrase) -> paraphrase
    | None -> sentence