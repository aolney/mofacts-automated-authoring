module DefinitionalFeedback

open Thoth.Json //for Json; might be cleaner way
open SpellingCorrector

//Fable 2 transition 
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)

// a simple implementation of elaborated feedback 
// that uses the definitions of the wrong answer and correct answer
// simple spelling correction is used to match wrong answers

// So for example:

// Cloze: The brain connects to the spinal cord through the __________ .

// Student: nervous system

// Feedback:
// Nervous system is not right. The right answer is brain stem.
// The difference is that (the nervous system is a network of cells that
// sense and respond to stimuli in ways that maintain homeostasis), and
// (the brain stem is a portion of the brain that includes the midbrain,
// pons, and medulla oblongata.)

// () delimit the start/edn of the definitions for example purposes

type Feedback =
    {
        feedback : string
    }

/// Defines an entry in our glossary. Tagging is needed for surface realization.
type EntryGloss =
    {
        entry : string[]
        entryTag : string[]
        gloss : string[]
        glossTag : string[]
    }

let firstLetterUpper (input : string) =
    input.Substring(0,1).ToUpper() + input.Substring(1)

let lower ( s : string ) = s.ToLower()

/// Map of definitions to EntryGloss
let mutable definitionMap = Map.empty
/// Map of definitions to possible determiners
let mutable determinerMap = Map.empty
/// Set of all words in definitions so we can distinguish b/w words that don't take a det and words we've never seen 
let mutable wordSet = Set.empty

/// Initialize the definition dictionary using a JSON object of string*EntryGloss
/// Construct the determiner map with this information
let Initialize jsonDictionary =
    try
        definitionMap <- jsonDictionary |> ofJson<Map<string,EntryGloss>>
        wordSet <- definitionMap |> Map.toSeq |> Seq.collect( fun (k,v) -> v.gloss |> Array.map lower ) |> Set.ofSeq

        //Use only the first word of an entry when looking for determiner matches; may be too strong an assumption
        let entryFirstWordSet = definitionMap |> Map.toSeq |> Seq.map( fun (k,v) -> v.entry.[0].ToLower() ) |> Set.ofSeq
        determinerMap <-
            definitionMap
            |> Map.toSeq
            |> Seq.collect( fun (k,v) -> Seq.zip (v.gloss |> Array.map lower) v.glossTag )
            |> Seq.pairwise
            |> Seq.choose( fun ( (w1,t1),(w2,t2) ) ->
                let w1Lower = w1.ToLower()
                match t1,entryFirstWordSet.Contains(w2) with
                | "DT", true when w1Lower = "a" || w1Lower = "an" || w1Lower = "the" -> Some(w2,w1) //entry first word and determiner
                | _ -> None
            )
            |> Seq.groupBy fst
            //Get the most frequent determiner
            |> Seq.map( fun (ent,detTuples) -> ent, detTuples |> Seq.countBy snd |> Seq.maxBy snd |> fst )
            |> Map.ofSeq

        promise{ return Ok( null ) }
    with
    | e -> promise{ return Error( e.Message ) }

// /// This function should only be called by the test harness GUI. It wraps Initialize to match the test harness API
// let HarnessInitialize ( jsonOption : string option) ( _ : string) =
//     promise {
//         match jsonOption with
//         | Some(json) -> 
//             json |> Initialize
//             return "ok", "{}"
//         | None -> return "error", """{"message":"missing dictionary of EntryGloss"}"""
//     }

let isAcronym ( word : string ) = word = word.ToUpper() 

let trim ( s : string ) = s.Trim()

let tokensToString tokens =
    System.Text.RegularExpressions.Regex.Replace( tokens |> String.concat " " |> trim , @" ([^\w])", "$1")

/// Gets a determiner phrase, which could be subject or object
let getDeterminerPhraseFromTokens (tokens : string[]) =
    let token0Lower = tokens.[0] |> lower
    let det =
        match determinerMap.TryFind( token0Lower ) with
        | Some(det) -> det
        | None -> 
            //If it is a word we've never seen OR we've seen the plural (which implies a count noun); assume it takes "a"/"an"; TODO: better job of plurals, etc
            if not <| wordSet.Contains( token0Lower) || wordSet.Contains( token0Lower + "s" ) then
                match token0Lower.Substring(0,1) with
                | "a" | "e" | "i" | "o" | "u" -> "an"
                | _ -> "a"
            else
                "" 

    //don't lowercase acronyms
    let correctCaseToken0 = if isAcronym(tokens.[0]) then tokens.[0] else token0Lower 
    //return the det, which may be empty, a lowercased first token, and the rest
    [| yield det; yield correctCaseToken0; for i = 1 to tokens.Length - 1 do yield tokens.[i] |] |> tokensToString

let getDeterminerPhrase ( text : string ) =
    text.Split(' ') |> getDeterminerPhraseFromTokens

/// Gets a predicate (verb + object phrase) for the entry
let getPredicate entry =
    //Acronyms are singular
    //let isAcronym = entry.entry.[0] = entry.entry.[0].ToUpper() 
    //We partly rely on the consistency of the glosses for marking adjective entries (gloss starts with Pertaining)
    let nounEntry = entry.glossTag.[0] <> "Pertaining" && ( entry.entryTag |> Array.last ).StartsWith("N")
    let pluralEntry = nounEntry  && (entry.entryTag |> Array.last ).EndsWith("S")
    let needsDeterminer = entry.entryTag.[0].StartsWith("N") || entry.entryTag.[0].StartsWith("J") //noun or adj; all other cases need nothing
    
    let verb = 
        match nounEntry, pluralEntry, isAcronym( entry.entry.[0] ) with
        | _, _, true -> "is"
        | true,true,_ -> "are"
        | true,false,_ -> "is"
        | _ -> "means"

    let completion =
        if needsDeterminer then
            entry.gloss |> getDeterminerPhraseFromTokens
        else
            entry.gloss |> tokensToString
    
    verb + " " + completion.Trim([|' ';'.'|])

type HarnessFeedbackRequest =
    { 
        CorrectAnswer : string
        IncorrectAnswer : string
    }
    static member InitializeTest() = {CorrectAnswer="ADH"; IncorrectAnswer ="acetylcholine"; }
 
/// Generates simple definitional feedback given a json object representing a FeedbackRequest
let GenerateFeedback incorrectAnswer correctAnswer =
    promise {
        //TODO: check why this seems to require 8GB of RAM
        //let incorrectAnswerSpellingMatch = incorrectAnswer |> CorrectSpelling
        let incorrectAnswerSpellingMatch = incorrectAnswer

        let feedback =
            match definitionMap.TryFind( incorrectAnswerSpellingMatch ), definitionMap.TryFind( correctAnswer ) with
            | Some( incorrectEntry ), Some( correctEntry ) -> 
                firstLetterUpper( incorrectAnswerSpellingMatch ) + " is not right. The right answer is " + correctAnswer  + ". " +
                "The difference is that " + getDeterminerPhrase( incorrectAnswerSpellingMatch )  + " " + getPredicate( incorrectEntry ) + 
                ", and " + getDeterminerPhrase( correctAnswer ) + " " + getPredicate( correctEntry ) + "."
            | _ -> null
        return Ok( {feedback = feedback } )
    }

/// This function should only be called by the test harness GUI. It wraps GenerateFeedback to match the test harness API
let HarnessGenerateFeedback jsonFeedbackRequest =
    let fr = jsonFeedbackRequest |> ofJson<HarnessFeedbackRequest>
    GenerateFeedback fr.IncorrectAnswer fr.CorrectAnswer

/// Very simplistic approach to turning non-sentential glossary entries into sentences
let GetDefinitionFromGlossary( term : string) =
    match definitionMap.TryFind( term ) with
    | Some(entry) -> (getDeterminerPhrase( term ) |> firstLetterUpper) + " " + getPredicate( entry ) +  "." |> Some
    | None -> None

/// Find the term or successive words then do a Very simplistic approach to turning non-sentential glossary entries into sentences
let GetDefinitionFromGlossaryHighRecall( term : string )=
    //use the whole term then the first word of the term ; using  following words tends to create drift, e.g. digestive tract goes to tract (neuron)
    // let candidateTerms = (Array.append [|term|] (term.Split( ' ' )) ) |> Array.filter( fun t -> definitionMap.ContainsKey( t ) )
    let candidateTerms = [|term ; term.Split( ' ' ).[0] |] |> Array.filter( fun t -> definitionMap.ContainsKey( t ) )
    if candidateTerms.Length > 0 then 
        GetDefinitionFromGlossary candidateTerms.[0]
    else
        None