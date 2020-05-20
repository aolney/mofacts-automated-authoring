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

/// A wrapper of text and type, mostly for logging purposes
type DialogueMove =
    {
        Text : string
        Type : StatelessDialogueMoveType
    }

let dialogueBags =
    [
        (ElaborationMarker, [ 
            "Remember that"; 
            "It's important to remember that";  
            "It is significant that";
            "We've established that"
            ])
        (ShiftMarker, [
            "Moving on"; 
            "Let's move on";
            "Let's keep going";
            "Let's continue";
        ])
        (PositiveFeedback, [
            "yes.";
            "good.";
            "yes!";
            "yay!";
            "wow!";
            "right.";
            "cool.";
            "okay.";
            "good!";
            "yeah!";
            "great!";
            "right!";
            "sweet!";
            "super!";
            "bingo!";
            "perfect!";
            "ok good.";
            "got it.";
            "correct.";
            "awesome!";
            "exactly!";
            "ok, good.";
            "good job!";
            "very good!";
            "excellent.";
            "that's it.";
            "good call.";
            "okay good.";
            "yep, good.";
            "that's it!";
            "ok, super!";
            "yes siree.";
            "absolutely.";
            "there we go.";
            "that's good.";
            "super duper!";
            "that's right.";
            "you're right.";
            "yeah exactly.";
            "there you go!";
            "yeah, awesome!";
            "exactly, yeah.";
            "good, awesome.";
            "perfect. good.";
            "ok, very good.";
            "alright, cool!";
            "that's perfect.";
            "that's awesome!";
            "alright, sweet.";
            "good! good job!";
            "that's correct.";
            "you're correct.";
            "right, exactly.";
            "yep, excellent.";
            "that's terrific.";
            "good, very good.";
            "good, that's it.";
            "that was perfect.";
            "absolutely right.";
            "good, you got it.";
            "that is fantastic.";
            "yes, that's right.";
            "yeah, you're right.";
            "there you go, you got it."
        ])
        (NeutralPositiveFeedback, [
            "close.";
            "sort of.";
            "that's close.";
            "almost.";
            "kind of."
        ])
        (NeutralFeedback, [
            "oh. hmm.";
            "um.";
            "hmm.";
            "huh.";
            "umm.";
            "well. um";
        ])
        (NeutralNegativeFeedback, [
            "not quite.";
            "not exactly.";
            "not really.";
        ])
        (NegativeFeedback, [
            "no.";
            "nope.";
            "oh, no.";
            "uh, no.";
            "well, no.";
            "oh, no.";
            "not good.";
            "well, no.";
            "um, nope.";
            "hmm, nope.";
            "actually no.";
            "that's not it.";
            "no, that's not it.";
        ])
    ] |> Map.ofList
let random = System.Random()
let getRandomMove (dm : StatelessDialogueMoveType) = dialogueBags.[dm].[random.Next(dialogueBags.[dm].Length-1)]

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
        CurrentElaboration : DialogueMove option
        ///The question we are asking the student now
        CurrentQuestion : Question option
        ///What the client should display now
        Display : string option
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
        //Prepare for question generation by getting NLP; doing a no-op if not needed to improve performance (TODO check improvement)
        let! daResult = 
            if state.Questions.IsNone then 
                text |> GetNLP None 
            else 
                "" |> GetNLP None
        //Assess student answers, doing a no-op if not needed to improve performance (TODO check improvement)
        let! teResult = 
            if state.LastQuestion.IsSome && state.LastStudentAnswer.IsSome then 
                GetTextualEntailment state.LastQuestion.Value.Answer state.LastStudentAnswer.Value
            else
                GetTextualEntailment "" ""

        //convenience functions for unwrapping/mapping results
        let isOK ( r : Result<'t,'e> ) = match r with | Ok(r) -> true | Error(e) -> false
        let resultToTypeOption (r : Result<'t,'e>[] )  = match r with | Ok(r) -> Some(r) | Error(_) -> None 
        let resultToErrorOption (r : Result<'t,'e>[] )  = match r with | Ok(r) -> None | Error(e) -> Some(e)

        //if no errors in service calls
        if daResult |> isOK && teResult |> isOK then
            //unwrap results to options
            let daOption = daResult |> resultToTypeOption
            let teOption = teResult |> resultToTypeOption

            //Generate questions if needed
            let questions = 
                match state.Questions, daOption with
                | Some(q), _ -> q
                | None, Some(da) -> da.sentences |> Array.head |> GetQuestions

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
                    let polarity = if te.label_probs[0] > te.label_probs[1] then 1 else -1
                    //feedback strength determined by amount of uncertainty (3 levels) TODO: these are arbitrary thresholds
                    let strength = if te.label_probs[2] > 0.66 then 1 elif te.label_probs[2] > 0.33 then 2 else 3
                    let feedback =
                        match polarity,strength with
                        | 1,2 -> NeutralPositiveFeedback |> getRandomMove
                        | 1,3 -> PositiveFeedback |> getRandomMove
                        | -1,2 -> NeutralNegativeFeedback |> getRandomMove
                        | -1,3 -> NegativeFeedback |> getRandomMove
                    feedback |> Some
                | _ -> None

            //Provide any feedback elaborations if needed (currently only if we have ended our question sequence)
            //TODO: should we provide correct answer to missed questions here?
            let elaborationOption =
                match currentQuestionOption with
                | None -> [getRandomMove(ElaborationMarker) ; text; getRandomMove(ShiftMarker)] |> String.concat " " |> Some
                | _ -> None

            //TODO: assemble these as we go so we don't need to unpack again
            //Assemble the complete turn: feedback, elaboration, question 
            let display = [ feedbackOption; elaborationOption ] |> List.choose id |> ResizeArray<string>
            match currentQuestionOption with
            | Some( q ) -> display.Add( q.Text )
            | None -> ()

            //TODO add wrapping for logging purposes
            return Ok( { state with Questions=questions |> Some; LastQuestion=currentQuestionOption; CurrentFeedback})

    }