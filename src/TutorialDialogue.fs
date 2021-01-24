module TutorialDialogue

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch
open AllenNLP
open QuestionGenerator


/// Stateless types do not require tracking across turns
/// Questions are the primary stateful type 
[<StringEnum>]
type StatelessDialogueMoveType =
    | ElaborationMarker
    | ShiftMarker
    | PositiveFeedback
    | NeutralPositiveFeedback
    | NeutralFeedback
    | NeutralNegativeFeedback
    | NegativeFeedback
    | Elaboration

let dialogueBags =
    [
        (ElaborationMarker, [ 
            "Remember that"; 
            "It's important to remember that";  
            "It is significant that";
            "We've established that"
            ])
        (ShiftMarker, [
            "Moving on."; 
            "Let's move on.";
            "Let's keep going.";
            "Let's continue.";
        ])
        (PositiveFeedback, [
            "Yes.";
            "Good.";
            "Yes!";
            "Yay!";
            "Wow!";
            "Right.";
            "Cool.";
            "Okay.";
            "Good!";
            "Yeah!";
            "Great!";
            "Right!";
            "Sweet!";
            "Super!";
            "Bingo!";
            "Perfect!";
            "Ok good.";
            "Got it.";
            "Correct.";
            "Awesome!";
            "Exactly!";
            "Ok, good.";
            "Good job!";
            "Very good!";
            "Excellent.";
            "That's it.";
            "Good call.";
            "Okay good.";
            "Yep, good.";
            "That's it!";
            "Ok, super!";
            "Yes siree.";
            "Absolutely.";
            "There we go.";
            "That's good.";
            "Super duper!";
            "That's right.";
            "You're right.";
            "Yeah exactly.";
            "There you go!";
            "Yeah, awesome!";
            "Exactly, yeah.";
            "Good, awesome.";
            "Perfect. good.";
            "Ok, very good.";
            "Alright, cool!";
            "That's perfect.";
            "That's awesome!";
            "Alright, sweet.";
            "Good! good job!";
            "That's correct.";
            "You're correct.";
            "Right, exactly.";
            "Yep, excellent.";
            "That's terrific.";
            "Good, very good.";
            "Good, that's it.";
            "That was perfect.";
            "Absolutely right.";
            "Good, you got it.";
            "That is fantastic.";
            "Yes, that's right.";
            "Yeah, you're right.";
            "There you go, you got it."
        ])
        (NeutralPositiveFeedback, [
            "Close.";
            "Sort of.";
            "That's close.";
            "Almost.";
            "Kind of."
        ])
        (NeutralFeedback, [
            "Oh. hmm.";
            "Um.";
            "Hmm.";
            "Huh.";
            "Umm.";
            "Well. um";
        ])
        (NeutralNegativeFeedback, [
            "Not quite.";
            "Not exactly.";
            "Not really.";
        ])
        (NegativeFeedback, [
            "No.";
            "Nope.";
            "Oh, no.";
            "Uh, no.";
            "Well, no.";
            "Oh, no.";
            "Not good.";
            "Well, no.";
            "Um, nope.";
            "Hmm, nope.";
            "Actually no.";
            "That's not it.";
            "No, that's not it.";
        ])
    ] |> Map.ofList
let random = System.Random()
let getRandomMove (dm : StatelessDialogueMoveType) = dialogueBags.[dm].[random.Next(dialogueBags.[dm].Length-1)]

/// A wrapper of text and type, mostly for logging purposes
type DialogueMove =
    {
        Text : string
        Type : StatelessDialogueMoveType
    } with
    static member GetRandom( aType : StatelessDialogueMoveType ) = { Text = aType |> getRandomMove; Type = aType }
    static member Create( text : string, aType : StatelessDialogueMoveType ) = { Text = text; Type = aType }

type DialogueState =
    {
        ///The cloze item presented, e.g. "Water is _____"
        ClozeItem:string
        ///Answer to the cloze item, e.g. "wet"
        ClozeAnswer:string
        ///Questions from QuestionGenerator
        Questions: Question[] option
        ///The question we just asked the student that they responded to with StudentAnswer
        LastQuestion : Question option
        ///The student answer to the LastQuestion
        LastStudentAnswer:string option
        ///The feedback in response to the LAST question/answer, which we are saying now
        CurrentFeedback : DialogueMove option
        //The elaborated feedback we are putting to the student now
        CurrentElaboration : DialogueMove[] option
        ///The question we are asking the student now
        CurrentQuestion : Question option
        ///What the client should display now
        Display : string option
        ///Is the dialogue finished
        Finished : bool option
    } with
    static member Initialize( clozeItem )( clozeAnswer) = { ClozeItem = clozeItem; ClozeAnswer = clozeAnswer; Questions = None; LastQuestion = None; LastStudentAnswer = None; CurrentFeedback = None; CurrentElaboration = None; CurrentQuestion = None; Display = None; Finished = None }
    // static member InitializeTest() = DialogueState.Initialize "The supraspinatus is located in the depression above the spine of the scapula on its _______ _______." "posterior surface"
    // static member InitializeTest() = DialogueState.Initialize "Human anatomy and physiology are the studies of the _______ _______ and how it works." "human body"
    // static member InitializeTest() = DialogueState.Initialize """Cells are specialized to take on specific and "necessary responsibilities", and together they maintain an environment within _____ _____ in which they can all live.""" "the body"
    // static member InitializeTest() = DialogueState.Initialize """Parts of the axial portion of the body can be described using terms similar to the names of the __________ within them.""" "cavities"
    static member InitializeTest() = DialogueState.Initialize """The interstitial fluid, which bathes cells in the body, is the environment to which those cells are most directly exposed, but the composition of the interstitial fluid is in equilibrium with the composition of the blood plasma, so both contribute to the ______ ______""" "internal environment"


// BEHAVIOR IN MOFACTS AS OF 1/20/21
// - student gets item wrong
// System: "Incorrect, the correct answer is <correct answer>".
// System does a transition statement:
// [
// "That wasn’t right, so to help you build the knowledge let’s chat about it
// for a little.",
// "That wasn’t the answer we are looking for. To help you construct the
// understanding, let’s have a short discussion.",
// "Sorry, but that wasn’t quite right. Let’s talk through this item.",
// "Incorrect. Lets help you build that knowledge with a brief discussion.",
// "The right answer is different. To get you started learning it, let’s chat."
// ,
// "Your answer was incorrect. Let’s talk about this some more.",
// "Not quite. I’m going to ask you some follow up questions."
// ]
// System uses GetDialogue to generate the remaining tutorial dialogue

/// Given a dialogue state, generate a new dialogue state. This is meant to be called repeatedly until Finished is false.
let GetDialogue (state:DialogueState) =
    promise{
        //Accumulate errors
        let errors = ResizeArray<obj>()

        // let! saOption = 
        //     "hi" |> GetNLP None
        // sa |> GetQuestions

        // let resultsToType (resultsArr : Result<'t,'e>[] )  = resultsArr |> Array.choose( fun r -> match r with | Ok(r) -> Some(r) | Error(_) -> None ) 
        
        //The cloze in complete sentence form
        let text = System.Text.RegularExpressions.Regex.Replace(state.ClozeItem, "(_ _|_)+", state.ClozeAnswer) //multi word cloze requires _ _ first
 
        //To make promises cleaner, pull them all here; can't use pattern matching without nested promises
        //Prepare for question generation by getting NLP; doing a no-op if not needed (TODO make cleaner?)
        let! daResult = 
            if state.Questions.IsNone then 
                text |> GetNLP None 
            else 
                DocumentAnnotation.CreateEmpty() |> Promisify
        //Assess student answers, doing a no-op if not needed (TODO make cleaner?)
        let! teResult = 
            if state.LastQuestion.IsSome && state.LastStudentAnswer.IsSome then 
                GetTextualEntailment state.LastQuestion.Value.Answer state.LastStudentAnswer.Value
            else
                Entailment.CreateEmpty() |> Promisify

        //convenience functions for unwrapping/mapping results
        let isOK ( r : Result<'t,'e> ) = match r with | Ok(r) -> true | Error(e) -> false
        let resultToTypeOption (r : Result<'t,'e> )  = match r with | Ok(r) -> Some(r) | Error(_) -> None 
        let resultToErrorOption (r : Result<'t,'e> )  = match r with | Ok(r) -> None | Error(e) -> Some(e)

        //if no errors in service calls
        if daResult |> isOK && teResult |> isOK then

            //unwrap results to options
            let daOption = daResult |> resultToTypeOption
            let teOption = teResult |> resultToTypeOption

            //Prepare to accumulate text to display
            let display = ResizeArray<string>()

            //Generate questions if needed
            let questions = 
                match state.Questions, daOption with
                | Some(q), _ -> q
                | None, Some(da) -> da.sentences |> Array.head |> GetQuotedQuestions state.ClozeAnswer
                | _,_ -> Array.empty //this is logically impossible

            //Select hint, prompt, or elaboration depending on last question
            //remove question from available questions so it doesn't get selected again
            let currentQuestionOption,newQuestions =
                //TODO add more sophisticated selection logic
                let hintOption = questions |> Array.tryFind(fun q -> q.QuestionType = Hint )
                let prompts = questions |> Array.filter(fun q -> q.QuestionType = Prompt )
                match state.LastQuestion, hintOption, prompts with
                //Initial move, we have a hint, so do hint
                | None, Some(h), _ -> h |> Some, questions |> Array.filter( fun q -> q <> h )
                //Last was hint, we have a prompt, so do prompt
                | Some(lastQ),_,ps when lastQ.QuestionType = Hint && ps.Length > 0 -> 
                    // Score available prompts according to how much their focus matches the answer of the last question, so giving a clue
                    let lastSet = lastQ.Answer.Split( ' ' ) |> Set.ofArray
                    let p = ps |> Array.maxBy( fun ap ->
                        let candidateSet = ap.Focus.Split( ' ' ) |> Set.ofArray
                        let intersection = Set.intersect candidateSet lastSet
                        intersection.Count
                    )
                    p |> Some, questions |> Array.filter( fun q -> q <> p )
                //Any other case (e.g. last question was prompt or we are out of questions) set current question to None
                | _ -> None, questions

            //Provide feedback if there is someting to assess
            let feedbackOption  = 
                match state.LastQuestion,state.LastStudentAnswer,teOption with
                | Some( lq ),Some (sa),Some(te) ->
                    //feedback polarity determined by whether entailment is > contradiction
                    let polarity = if te.label_probs.[0] > te.label_probs.[1] then 1 else -1
                    //feedback strength determined by amount of uncertainty (3 levels) NOTE: these are arbitrary thresholds
                    let strength = if te.label_probs.[2] > 0.66 then 1 elif te.label_probs.[2] > 0.33 then 2 else 3
                    let feedback =
                        //Neutral on empty answers
                        if sa.Trim() = "" then
                            DialogueMove.GetRandom(  NeutralFeedback )
                        //Use TE on all other answers
                        else 
                            match polarity,strength with
                            | 1,2 -> DialogueMove.GetRandom(  NeutralPositiveFeedback )
                            | 1,3 -> DialogueMove.GetRandom(  PositiveFeedback )
                            | -1,2 -> DialogueMove.GetRandom(  NeutralNegativeFeedback )
                            | -1,3 -> DialogueMove.GetRandom(  NegativeFeedback )
                            | _,_ -> DialogueMove.GetRandom(  NeutralFeedback )
                    //to display feedback
                    display.Add( feedback.Text )
                    //return structured feedback
                    feedback |> Some
                | _ -> None

            // Separate into function to remove redundancy in branches; might be a ?better? way using active patterns: https://stackoverflow.com/questions/31710260/the-two-sides-of-this-or-pattern-bind-different-sets-of-variables
            let makeElaboration() =
                let elaboration = [| DialogueMove.GetRandom( ElaborationMarker) ; DialogueMove.Create( text, Elaboration ); DialogueMove.GetRandom(ShiftMarker) |] 
                //to display elaboration
                display.AddRange( elaboration |> Array.map( fun e -> e.Text ))
                //return structured elaboration
                elaboration

            //Provide any feedback elaborations if needed (currently only if we have ended our question sequence)
            //Case 1: Hint received positive feedback, so complete
            //Case 2: We have tried a hint and a prompt, so bail
            let elaborationOption =
                match currentQuestionOption,feedbackOption with
                | _ , Some(x) when x.Type = PositiveFeedback -> makeElaboration() |> Some
                | None,_ -> makeElaboration() |> Some
                | _ -> None

            //Add question to display, but only if we don't have an elaboration ; TODO: add goal semantics to avoid structural rules like this
            match currentQuestionOption,elaborationOption with
            | Some( q ), None -> display.Add( q.Text )
            | _ -> ()

            //If we have done an elaboration, we have finished
            let finishedOption = 
                match elaborationOption with
                | Some(x) -> true |> Some
                | None -> false |> Some

            //TODO add wrapping for logging purposes
            return Ok( 
                { state with 
                    Questions=newQuestions |> Some; 
                    LastQuestion=currentQuestionOption; 
                    CurrentFeedback=feedbackOption;
                    CurrentElaboration=elaborationOption;
                    CurrentQuestion = currentQuestionOption;
                    Display = display |> String.concat " " |> Some;
                    Finished = finishedOption
                })
        else
             //collect all errors; avoid duplicates
            let errorPayload = ResizeArray<string>()
            errorPayload.AddRange( [ daResult ] |> List.choose resultToErrorOption |> List.map (sprintf "document annotation error: %A")  )
            errorPayload.AddRange( [ teResult ] |> List.choose resultToErrorOption |> List.map (sprintf "textual entailment error: %A")  )
            return Error(errorPayload |> String.concat "\n") 
    }


/// This function should only be called by the test harness GUI. It wraps GetDialogue to match the test harness API
let HarnessGetDialogue jsonState =
    let state = jsonState |> ofJson<DialogueState>
    GetDialogue state


/// Get a DialogueState initialized by elaborated feedback.
/// GetDialogue can then be called with this state.
let GetElaboratedDialogueState correctAnswer incorrectAnswer clozeItem =
    promise {
        // Spring 2021: we are only using cached elaborated feedback for performance issues
        //NOTE: ideally we'd be working with an annotated object here (including coreferece) instead of raw text for performance reasons
        let! efResult = CachedElaboratedFeedback.GenerateFeedback incorrectAnswer correctAnswer
        match efResult with
        | Ok(ef) ->
            //remove correctness statement from ef
            let cs = CachedElaboratedFeedback.correctnessStatement incorrectAnswer correctAnswer
            // OPTION: use a JS sentence segmenter: https://www.npmjs.com/package/cldr-segmentation
            let candidateSentences = ef.Feedback.Replace( cs, "").Split('.')

            // Search for incorrect/correct answers in the elaborated feedback
            let jointOption = candidateSentences |> Array.tryFind( fun s -> s.ToLower().Contains( incorrectAnswer.ToLower() ) && s.ToLower().Contains( correctAnswer.ToLower() ) )
            let iaOption = candidateSentences |> Array.tryFind( fun s -> s.ToLower().Contains( incorrectAnswer.ToLower() ) )
            let caOption = candidateSentences |> Array.tryFind( fun s -> s.ToLower().Contains( correctAnswer.ToLower() ) )

            // Create a proxy for the cloze item/answer required by GetDialogue, starting with a sentence that contains both, 
            // backing off to incorrect answer, backing of to correct answer, backing off to cloze item
            let pseudoCloze,pseudoClozeAnswer = 
                match jointOption,iaOption,caOption with
                | Some(j), _, _ -> j + ".", incorrectAnswer
                | _, Some(i), _ -> i + ".", incorrectAnswer
                | _, _, Some(c) -> c + ".", correctAnswer
                | _ -> clozeItem, correctAnswer

            return Ok( DialogueState.Initialize pseudoCloze pseudoClozeAnswer )

        | Error(e) -> return Error(e)
    }
    

type HarnessElaboratedDialogueState =
    { 
        CorrectAnswer : string
        IncorrectAnswer : string
        ClozeItem : string
    }
    static member InitializeTest() = { CorrectAnswer = "cerebellum" ; IncorrectAnswer = "cerebrum"; ClozeItem= "Small amounts enter the central canal of the spinal cord, but most CSF circulates through the subarachnoid space of both the brain and the spinal cord by passing through openings in the wall of the fourth ventricle near the cerebellum ."}
  
let HarnessGetElaboratedDialogueState jsonState = 
    let state = jsonState |> ofJson<HarnessElaboratedDialogueState>
    GetElaboratedDialogueState state.CorrectAnswer state.IncorrectAnswer state.ClozeItem