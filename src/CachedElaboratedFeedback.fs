module CachedElaboratedFeedback

open Thoth.Json 

//Fable 2 transition 
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)

// This module caches elaborated feedback generated offline for common errors
// It backs off to definitional feedback if nothing is found in the cache.

type Tag =
    /// Did we use definitional feedback
    | DefinitionalFeedback
    /// Did we use cached elaborated feedback
    | CachedElaboratedFeedback
    /// Debug information
    | Trace of string

type Feedback =
    {
        Feedback : string
        Tags: Tag[]
    }


/// Map of definitions to EntryGloss
let mutable cache = Map.empty

/// Initialize the cache using a JSON object of string[][] because ofJson doesn't like tuples as keys
let Initialize jsonDictionary =
    try
        cache <- jsonDictionary |> ofJson<string[][]> |> Array.choose( fun arr -> if arr.[2].Trim() <> "" then Some((arr.[0],arr.[1]), arr.[2]) else None) |> Map.ofArray
        promise{ return Ok( null ) }
    with
    | e -> promise{ return Error( e.Message ) }

let firstLetterUpper (input : string) =
    input.Substring(0,1).ToUpper() + input.Substring(1)

let correctnessStatement incorrectAnswer correctAnswer =
    firstLetterUpper( incorrectAnswer ) + " is not right. The right answer is " + correctAnswer  + ". " 

/// Generate elaborated feedback from the cache, backing off to definitional feedback
let GenerateFeedback incorrectAnswer correctAnswer =
    promise {
        let tags = ResizeArray<Tag>();
        /// Search for incorrect,correct and correct,incorrect at once, preferring the incorrect,correct order consistent with the cache design
        /// and backing off to definitional feedback if neither is found
        let elaboratedFeedback =
            match cache.TryFind( incorrectAnswer,correctAnswer ), cache.TryFind( correctAnswer,incorrectAnswer ) with
            | Some(ef),_ -> ef |> Some
            | None, Some(ef) -> ef |> Some
            | None, None -> None

        if elaboratedFeedback.IsSome then
            tags.Add(CachedElaboratedFeedback)
            let cs = correctnessStatement incorrectAnswer correctAnswer
            return Ok( {Feedback = cs + elaboratedFeedback.Value; Tags=tags.ToArray() } )
        else 
            let! dfResult = DefinitionalFeedback.GenerateFeedback incorrectAnswer correctAnswer
            match dfResult with
            | Ok(df) -> 
                tags.Add(DefinitionalFeedback)
                return Ok( {Feedback = df.feedback; Tags=tags.ToArray() } )
            // ~~DefinitionalFeedback.GenerateFeedback doesn't return errors, so this case exists to avoid compiler warnings~~
            | Error( e ) -> 
                // let trace = Trace <| "Unable to generate feedback. Elaborated feedback cache is " + if cache.IsEmpty then "empty" else "full" + ". Definition feedback is " + if DefinitionalFeedback.definitionMap.IsEmpty then "empty." else "full."
                // tags.Add(trace)
                // return  Ok( {Feedback = null; Tags=tags.ToArray() } )
                let message = "Unable to generate elaborated feedback from " + if cache.IsEmpty then "empty" else "loaded" + " cache. " + e
                return  Error( message )
    }

type HarnessFeedbackRequest =
    { 
        CorrectAnswer : string
        IncorrectAnswer : string
    }
    static member InitializeTest() = {CorrectAnswer="nervous system"; IncorrectAnswer ="spinal cord"; }
 
/// This function should only be called by the test harness GUI. It wraps GenerateFeedback to match the test harness API
let HarnessGenerateFeedback jsonFeedbackRequest =
    let fr = jsonFeedbackRequest |> ofJson<HarnessFeedbackRequest>
    GenerateFeedback fr.IncorrectAnswer fr.CorrectAnswer
