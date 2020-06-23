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
    }

let GetDialogue (state:DialogueState) =
    promise{
        //Accumulate errors
        let errors = ResizeArray<obj>()

        // let! saOption = 
        //     "hi" |> GetNLP None
        // sa |> GetQuestions

        // let resultsToType (resultsArr : Result<'t,'e>[] )  = resultsArr |> Array.choose( fun r -> match r with | Ok(r) -> Some(r) | Error(_) -> None ) 
        
        //The cloze in complete sentence form
        let text = System.Text.RegularExpressions.Regex.Replace(state.ClozeItem, "_+", state.ClozeAnswer)

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
                | None, Some(da) -> da.sentences |> Array.head |> GetQuestions
                | _,_ -> Array.empty //this is logically impossible

            //Select hint, prompt, or elaboration depending on last question
            //remove question from available questions so it doesn't get selected again
            let currentQuestionOption,newQuestions =
                //TODO add more sophisticated selection logic
                let hintOption = questions |> Array.tryFind(fun q -> q.QuestionType = Hint )
                let promptOption = questions |> Array.tryFind(fun q -> q.QuestionType = Prompt )
                match state.LastQuestion, hintOption, promptOption with
                //Initial move, we have a hint, so do hint
                | None, Some(h), _ -> h |> Some, questions |> Array.filter( fun q -> q <> h )
                //Last was hint, we have a prompt, so do prompt
                | Some(lastQ),_,Some(p) when lastQ.QuestionType = Hint -> p |> Some, questions |> Array.filter( fun q -> q <> p )
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

// Example test JSON for App InputText
// {
//   "ClozeItem": "John ate a _____ because he was hungry.",
//   "ClozeAnswer": "hamburger"
// }