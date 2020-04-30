module SpellingCorrector

// from http://www.fssnip.net/6j/title/Norvigs-Spelling-Corrector
// Norvig's Spelling Corrector: http://norvig.com/spell-correct.html
open System.IO open System.Text.RegularExpressions

let edits1 (word : string) = 
    let splits = [for i in 0 .. word.Length do yield (word.[0..i-1], word.[i..])]
    let deletes = [for a, b in splits do if b <> "" then yield a + b.[1..]]
    let transposes = [for a, b in splits do if b.Length > 1 then yield a + string b.[1] + string b.[0] + b.[2..]]
    let replaces = [for a, b in splits do for c in 'a'..'z' do if b <> "" then yield a + string c + b.[1..]]
    let inserts = [for a, b in splits do for c in 'a'..'z' do yield a + string c + b]
    deletes @ transposes @ replaces @ inserts |> Set.ofList

let mutable NWORDS = Map.empty

/// Call to initialize the spelling corrector, passing in unstructured text
let Initialize ( text : string) =
    try
        NWORDS <-  
            text |> (Regex "[a-zA-Z]+").Matches |> Seq.cast 
            |> Seq.map (fun (m:Match) -> m.Value.ToLower()) |> Seq.countBy id |> Map.ofSeq
        promise{ return Ok( null ) }
    with
    | e -> promise{ return Error( e.Message ) }
    
/// This function should only be called by the test harness GUI. It wraps Initialize to match the test harness API
// let HarnessInitialize ( textOption : string option ) ( _ : string) =
//     promise {
//         match textOption with
//         | Some(text) -> 
//             text |> Initialize
//             return "ok", "{}"
//         | None -> return "error", """{"message":"missing raw text file defining correctly spelled words"}"""
//     }

let knownEdits2 word = [for e1 in edits1(word) do for e2 in edits1(e1) do if Map.containsKey e2 NWORDS then yield e2] |> Set.ofList
let known words = [for w in words do if Map.containsKey w NWORDS then yield w] |> Set.ofList

let (<||>) (first : Lazy<_>) (second : Lazy<_>) : Lazy<_> = lazy(if Set.isEmpty first.Value then second.Value else first.Value)

/// YOU MUST INITIALIZE NWORDS BEFORE CALLLING THIS FUNCTION (InitializeNWORDS)
/// Corrects a word using the following rules
/// The original word, if it is known; otherwise
/// The list of known words at edit distance one away, if there are any; otherwise
/// The list of known words at edit distance two away, if there are any; otherwise
/// The original word, even though it is not known. 
let CorrectSpelling word = 
    (lazy known([word]) <||> lazy known(edits1(word)) <||> lazy knownEdits2(word) <||> lazy Set.singleton word).Value 
    |> Seq.sortBy (fun w -> -NWORDS.[w]) |> Seq.head

// Example
// correct "speling"