module ElaboratedFeedback

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch
open LongformQA
open Wikifier

//Fable 2 transition 
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)

//convenience functions for unwrapping/mapping results
let isOK ( r : Result<'t,'e> ) = match r with | Ok(r) -> true | Error(e) -> false
let resultToTypeOption (r : Result<'t,'e> )  = match r with | Ok(r) -> Some(r) | Error(_) -> None 
let resultToErrorOption (r : Result<'t,'e> )  = match r with | Ok(r) -> None | Error(e) -> Some(e)
let allOK (resultsArr : Result<'t,'e>[] ) = resultsArr |> Array.forall( fun r -> match r with | Ok(r) -> true | Error(e) -> false )
let resultsToType (resultsArr : Result<'t,'e>[] )  = resultsArr |> Array.choose( fun r -> match r with | Ok(r) -> Some(r) | Error(_) -> None ) 
let resultsToError (resultsArr : Result<'t,'e>[] )  = resultsArr |> Array.choose( fun r -> match r with | Ok(r) -> None | Error(e) -> Some(e) ) 
let PromisifyOk ( input:'t ) =
    promise{ return Ok(input) }

[<StringEnum>]
type QuestionType =
    /// Was the synthetic question "What is the difference between X and Y"
    | DifferenceQuestion
    /// Was the synthetic question "What is the relationship between X and Y"
    | RelationshipQuestion

type Tag =
    /// Were definitions inserted as context documents in the order of words in question
    | DefinitionsUsed of int
    /// Was the cloze sentence used as a context document
    | ClozeUsed of bool
    /// Number of Elasticsearch documents returned
    | ElasticDocumentsFound of int
    /// Number of Elasticsearch documents with both answers
    | ElasticDocumentsContainBothKeys of int
    /// Number of Elasticsearch documents used (for whatever reason)
    | ElasticDocumentsUsed of int
    /// Number of answer sentences with one key term
    | AnswerSentencesContainOneKey of int
    /// Was coreference used to clean up answer
    | CoreferenceFilteredSentences of bool
    /// What was the synthetic question?
    | SyntheticQuestion of QuestionType
    /// Debug information
    | Trace of string

/// Capture manipulations of Elasticsearch results going into QA
type ElasticProcess = 
    {
        /// Documents retrieved from Elasticsearch AP book using the synthetic question
        ElasticDocuments : Document[]
        /// Index of ElasticDocuments that contain both IncorrectAnswer and CorrectAnswer
        MatchingElasticDocumentIndices : int[]
        /// Output of this process to be used as query context
        OutputContext : string[]
    }
/// Capture manipulations using coreference resolution on answer
type CoreferenceProcess = 
    {
        /// The unprocessed answer sentences, may contain garbage sentences
        CandidateAnswerSentences : string[];
        /// Coref resolved candidate answer sentences
        CorefCandidateAnswerSentences : string[];
        /// Index of CorefCandidateAnswer sentences used
        SelectedSentenceIndices : int[];
        /// Output of this process to be used as the answer
        OutputAnswer : string
    }
/// The source of a definition
[<StringEnum>]
type DefinitionSource =
    /// Book glossary, constructed per domain
    | Glossary
    /// Wikipedia, totally dynamic
    | Wikipedia

/// A definition with associated metadata
type Definition =
    {
        /// The text of the definition
        Text : string 
        /// Source of the definition
        Source : DefinitionSource
    }
    //Project the text of a Definition option as an option
    static member TextOption ( def : Definition option) =  match def with | Some(d) -> Some <| d.Text | None -> None 

/// Capture manipulations of definitions
type DefinitionProcess = 
    {
        /// The definition we retrieved for the CorrectAnswer
        CorrectAnswerDefinition : Definition option
        /// The definition we retrieved for the IncorrectAnswer
        IncorrectAnswerDefinition : Definition option
        /// Wikipedia metadata associated with definitions from that source
        WikiExtracts : WikiTermEntityExtracts option
        /// Output of this process to be used as query context
        OutputContext : string[]
    }
/// An elaborated feedback based on the longform-qa API
type ElaboratedFeedback =
    {
        /// The output elaborated feedback as a string
        ElaboratedFeedback : string
        /// The incorrect answer to the cloze item provided by the student
        IncorrectAnswer : string
        /// The correct answer to the cloze item
        CorrectAnswer : string
        /// The sentence that the cloze item was generated from, i.e. without a blank
        ClozeSentence : string option;
        /// Manipulations of Elasticsearch results going into query
        ElasticProcess : ElasticProcess
        /// Manipulations of definition results going into query
        DefinitionProcess : DefinitionProcess option
        /// Documents provided as longform QA context
        ContextDocuments : string[]
        /// Manipulations using coreference resolution on answer
        CoreferenceProcess : CoreferenceProcess option
        /// The synthethic question used, e.g What is the difference between X and Y?
        SyntheticQuestion : string;
        /// List of tags for analysis and debugging
        Tags : Tag[]
    }

/// An entailment comparison between elaborated feedbacks
type EntailmentComparison =
    {
        Premise : ElaboratedFeedback
        Hypothesis : ElaboratedFeedback
        Entailment : float
        Contradiction : float
        Neutral : float
    }

let lower ( s : string ) = s.ToLower()
let lowContains ( a : string ) ( b : string ) =
    a.ToLower().Contains( b.ToLower () )


type Configuration =
    {
        UseCloze : bool;
        /// Implies glossary by default
        UseGlossaryDefinitions : bool;
        /// Requires UseDefintions to be true
        UseWikipediaDefinitionsForMissingGlossaryDefinitions : bool;
        ElasticDocsContainBothKeys : bool;
        MaxElasticDocs : int;
        UseAnswerCoreferenceFilter : bool;
        SyntheticQuestion : QuestionType;
    }
    static member Default() = { UseCloze = true; UseGlossaryDefinitions = true; UseWikipediaDefinitionsForMissingGlossaryDefinitions = true; ElasticDocsContainBothKeys = true; MaxElasticDocs = 3; UseAnswerCoreferenceFilter = true ; SyntheticQuestion = RelationshipQuestion}

/// Given a term, find the associated definitions in WikiTermEntityExtracts
let getWikiDefinition (wikiExtracts: WikiTermEntityExtracts) ( term : string ) =
    let wtemOption =
        wikiExtracts.WikiTermEntityMatches
        |> Array.tryFind( fun wtem -> wtem.Term = term )
    // get page ids of the first candidate for eacy entity (top ranked candidate)
    let pageIdsOption =
        match wtemOption with
        | Some(wtem) -> 
            wtem.EntityMatches 
            // Rank order em by candidate probability so most probable gets most consideration
            |> Array.map( fun em -> 
                let max = em.Entity.candidates |> Array.maxBy( fun c -> c.score) 
                max.score,max.wikiId 
            )
            |> Array.sortByDescending fst
            |> Array.map snd
            |> Array.distinct
            |> Some
            // OLD: assumes candidates are already sorted by descending, which isn't always true
            // |> Array.sortByDescending( fun em -> em.Entity.candidates.[0].score)
            // |> Array.map( fun em -> em.Entity.candidates.[0].wikiId) |> Some
        | None -> None
    // find definitions with associated pageIds, if they exist, avoiding duplicates
    match pageIdsOption with
    | Some(pageIds) ->
        let definitions =  
            [| for pageId in pageIds do
                for page in wikiExtracts.Pages do
                    if pageId = page.pageid then
                        //simplistic - use first sentence marked by period
                        yield page.extract.Split('.').[0]
            |] |> String.concat ". "
        if definitions.Length > 0 then 
            Some <| { Text = definitions ; Source = Wikipedia }
        else None
    | None -> None
    
/// Get a glossary definition
let getGlossaryDefinition (term:string) = 
    match term |> DefinitionalFeedback.GetDefinitionFromGlossaryHighRecall with
    | Some(d) -> Some <| { Text = d; Source = Glossary }
    | None -> None

/// Get definition process (definitions and aspects of construction)
let getDefinitionProcess correctAnswer incorrectAnswer text config =
    // promise {
        //Log our progress using tags
        let tags = ResizeArray<Tag>()

        // We have multiple options for definitions. 
        // We prefer the glossary entry if it exists
        // An alternative is Wikipedia 
        if config.UseGlossaryDefinitions then
            let glossaryIncorrectDefinitionOption = incorrectAnswer |> getGlossaryDefinition
            let glossaryCorrectDefinitionOption = correctAnswer |> getGlossaryDefinition
            //if we fail to get glossary definitions, query wikipedia
            if config.UseWikipediaDefinitionsForMissingGlossaryDefinitions && (glossaryIncorrectDefinitionOption.IsNone || glossaryCorrectDefinitionOption.IsNone) then
                promise {
                    // Try to get definitions for the terms we are missing from Wikipedia
                    // Because only the correct answer is in the text for certain, we create a synthetic text where we've replaced the correct answer with the correct answer
                    let syntheticText = text + " " + text.Replace( correctAnswer, incorrectAnswer)
                    let! wikiResult = GetWikiExtractsForTerms syntheticText [| incorrectAnswer ; correctAnswer |] 
                    match wikiResult with
                    | Ok( wikiExtracts ) -> 
                        let wikiIncorrectDefinitionOption = getWikiDefinition wikiExtracts incorrectAnswer
                        let wikiCorrectDefinitionOption = getWikiDefinition wikiExtracts correctAnswer
                        // Use the glossary definition, backing off to the wiki definition
                        let finalIncorrectDefinition = 
                            match glossaryIncorrectDefinitionOption, wikiIncorrectDefinitionOption with
                            | Some(_), _ -> glossaryIncorrectDefinitionOption
                            | None, Some(_) -> wikiIncorrectDefinitionOption
                            | None, None -> None
                        let finalCorrectDefinition = 
                            match glossaryCorrectDefinitionOption, wikiCorrectDefinitionOption with
                            | Some(_), _ -> glossaryCorrectDefinitionOption
                            | None, Some(_) -> wikiCorrectDefinitionOption
                            | None, None -> None


                        let definitionContext = [| finalIncorrectDefinition; finalCorrectDefinition |] |> Array.choose Definition.TextOption
                        tags.Add(DefinitionsUsed(definitionContext.Length))

                        let dp = { CorrectAnswerDefinition = finalCorrectDefinition; IncorrectAnswerDefinition = finalIncorrectDefinition; WikiExtracts = Some <| wikiExtracts; OutputContext = definitionContext }
                        return Ok( Some <| dp, tags)
                    | Error(e) -> return Error(e)
                }
            else
                //Note incorrect precedes correct: this should match the synthetic question word order
                let definitionContext = [| glossaryIncorrectDefinitionOption ; glossaryCorrectDefinitionOption |] |> Array.choose Definition.TextOption
                tags.Add(DefinitionsUsed(definitionContext.Length))
                let dp = { CorrectAnswerDefinition = glossaryCorrectDefinitionOption; IncorrectAnswerDefinition = glossaryIncorrectDefinitionOption; WikiExtracts = None; OutputContext = definitionContext }      
                PromisifyOk ( Some <| dp ,tags)
        else
            tags.Add(DefinitionsUsed(0))
            PromisifyOk ( None,tags )
    // }

/// Get Elastic process (documents and aspects of construction)
let getElasticProcess (docs : Document[]) correctAnswer incorrectAnswer config =
    //Log our progress using tags
    let tags = ResizeArray<Tag>()
    tags.Add(ElasticDocumentsFound(docs.Length))

    //tuple for accessing indices later; skip synthetic question doc
    let docTuples =  docs |> Array.skip 1 |> Array.mapi( fun i d -> i,d )

    // Apply selection criteria (if any) to elastic documents
    let filteredDocuments = 
        //Remove documents that don't contain both incorrect and correct answers
        if config.ElasticDocsContainBothKeys then 
            let fd = docTuples |> Array.filter( fun (_,d) ->  (lowContains d.Text incorrectAnswer) && (lowContains d.Text correctAnswer) )
            tags.Add(ElasticDocumentsContainBothKeys(fd.Length))
            fd
        else
            docTuples
    
    let retainedDocIndices = filteredDocuments |> Array.map fst

    //Heuristic limit on number of elastic documents to use (idea is that too much irrelevant context is harmful)
    let elasticDocLimit = config.MaxElasticDocs
    let tryTake i (arr:string[]) = if i > arr.Length then arr else arr.[0..i-1]
    let finalElasticDocs = filteredDocuments |> Array.map( fun (_,d) -> d.Text) |> tryTake elasticDocLimit
    tags.Add(ElasticDocumentsUsed(finalElasticDocs.Length))

    { ElasticDocuments = docs; MatchingElasticDocumentIndices = retainedDocIndices; OutputContext = finalElasticDocs},tags

/// Get coreference process (coref filtered answer and aspects of construction)
let getCoreferenceProcess answer correctAnswer incorrectAnswer config = 
    promise {       
        //Log our progress using tags
        let tags = ResizeArray<Tag>()

        if config.UseAnswerCoreferenceFilter then
            //Remove sentences that do not contain one of our terms; we get better recall by using coref resolution to do this
            let! answerWithResolvedReferents = answer |> AllenNLP.ResolveTextReferents
            match answerWithResolvedReferents with
            | Ok(res) ->
                // let sentences = ans.answer.Split('.') //hacking sentence segmentation
                //align before filtering
                let sentenceTuples =  res.documentAnnotation.sentences |> Array.zip res.resolvedSentences |> Array.mapi( fun i (r,s) -> i, r, s.sen)
                let candidateSentences = sentenceTuples |> Array.map( fun (i,r,s) -> s)
                //filter
                let filteredTuples = sentenceTuples |> Array.filter( fun (i,r,s) ->  (lowContains r incorrectAnswer) || (lowContains r correctAnswer) )
                //project results
                let corefSentences = filteredTuples |> Array.map( fun (i,r,s) -> r)
                let answerSentences = filteredTuples |> Array.map( fun (i,r,s) -> s)
                let retainedSentenceIndices = filteredTuples |> Array.map( fun (i,r,s) -> i)
                tags.Add(AnswerSentencesContainOneKey(answerSentences.Length))
                tags.Add(CoreferenceFilteredSentences(true))
                let finalAnswer = answerSentences |> String.concat " "
                
                return Ok(  Some <| {CandidateAnswerSentences = candidateSentences; CorefCandidateAnswerSentences = corefSentences; SelectedSentenceIndices=retainedSentenceIndices; OutputAnswer=finalAnswer}, tags ) 
            // Error on resolving coreference
            | Error(e) -> 
                return Error( FetchError.DecodingFailed(e) ) //HACK: must be a fetch error to match type, so we choose a FetchError that wraps string
        else
            return Ok( None, tags ) 
          
    }

let GetElaboratedFeedback( incorrectAnswer : string ) (correctAnswer : string) ( clozeSentence : string ) ( configurationOption : Configuration option)=
    promise {
        let config =
            match configurationOption with
            | Some(c) -> c
            | None -> Configuration.Default()

        //Log our progress using tags
        let tags = ResizeArray<Tag>()

        //Get definitions
        let! dpResult = getDefinitionProcess correctAnswer incorrectAnswer clozeSentence config
        match dpResult with
        | Ok(definitionProcess,dpTags) -> 
            tags.AddRange(dpTags)
                
            
            //Create synthetic question
            let question = 
                match config.SyntheticQuestion with
                | RelationshipQuestion -> "What is the relationship between the " + incorrectAnswer + " and the " + correctAnswer + "?"
                | DifferenceQuestion -> "What is the difference between the " + incorrectAnswer + " and the " + correctAnswer + "?"
            tags.Add( SyntheticQuestion( config.SyntheticQuestion ))

            //Get elastic docs; see tutorial dialogue for example of handling promises if things get more complicated
            let! documents = question |> getDocuments
            match documents with
            | Ok(docs) ->

                // Process elastic docs
                let elasticProcess, epTags = getElasticProcess docs correctAnswer incorrectAnswer config
                tags.AddRange(epTags)
                
                //Construct full custom context for query
                let definitionContext = 
                    match definitionProcess with
                    | Some(dp) -> dp.OutputContext
                    | None -> [||]
                let clozeContext = 
                    if config.UseCloze then  
                        tags.Add(ClozeUsed(true))
                        [| clozeSentence |] 
                    else Array.empty
                let context = Array.concat [ definitionContext; clozeContext; elasticProcess.OutputContext ]               
                
                // Process query and results
                let! answer = getAnswerWithContext question context
                match answer with
                | Ok(ans) -> 
                    let! cpResult = getCoreferenceProcess ans.answer correctAnswer incorrectAnswer config
                    match cpResult with
                    | Ok(coreferenceProcess,cpTags) -> 
                        tags.AddRange(cpTags)

                        let finalAnswer =
                            match coreferenceProcess with 
                            | Some(cp) -> cp.OutputAnswer
                            | None -> ans.answer

                        let elaboratedFeedback = 
                            {
                                ElaboratedFeedback = finalAnswer;
                                IncorrectAnswer = incorrectAnswer;
                                CorrectAnswer = correctAnswer;
                                ClozeSentence = if config.UseCloze then Some <| clozeSentence else None;
                                ElasticProcess = elasticProcess;
                                DefinitionProcess = definitionProcess;
                                ContextDocuments = context;
                                CoreferenceProcess = coreferenceProcess;                           
                                SyntheticQuestion = question;
                                Tags = tags.ToArray()
                            }
                        return Ok( elaboratedFeedback ) 
                    // Error on resolving coreference
                    | Error(e) -> 
                       return Error(e)            
                // Error on getting answer from longform qa
                | Error(e) -> 
                    return Error(e)
            // Error on getting docs from elasticsearch
            | Error(e) -> 
                return Error(e)
        // Error on getting definitions
        | Error(e) -> 
            return Error(e)
    }

/// Request for the test harness UI
type HarnessElaboratedFeedbackRequest =
    { 
        CorrectAnswer : string
        IncorrectAnswer : string
        ClozeSentence : string
    }
    // static member InitializeTest() = {CorrectAnswer="cerebellum"; IncorrectAnswer ="cerebrum"; ClozeSentence="Small amounts enter the central canal of the spinal cord, but most CSF circulates through the subarachnoid space of both the brain and the spinal cord by passing through openings in the wall of the fourth ventricle near the cerebellum ."}
    static member InitializeTest() = {CorrectAnswer="digestive tract"; IncorrectAnswer ="digestive system"; ClozeSentence="Other organs that produce hormones include the pineal gland; the thymus; reproductive organs; and certain cells of the digestive tract, the heart, and the kidneys."}

/// This function should only be called by the test harness GUI. It wraps GenerateFeedback to match the test harness API
let HarnessGetElaboratedFeedback jsonRequest =
    let request = jsonRequest |> ofJson<HarnessElaboratedFeedbackRequest>
    let config = 
        None
        //Some <| { UseCloze = false; UseGlossaryDefinitions = true;  UseWikipediaDefinitionsForMissingGlossaryDefinitions = true;  ElasticDocsContainBothKeys = true; MaxElasticDocs = 3; UseAnswerCoreferenceFilter = true ; SyntheticQuestion = RelationshipQuestion}
    GetElaboratedFeedback request.IncorrectAnswer request.CorrectAnswer request.ClozeSentence config 


// ABLATION EXPERIEMENTS: CALLING FROM NODE WITH TIMEOUTS TO AVOID BLOWING UP CUDA
[<StringEnum>]
type AblationCondition = 
    /// No cloze, no definitions, but everything else
    | [<CompiledName("NoClozeNoDefinitions")>] NoClozeNoDefinitions
    /// No cloze, but glossary definitions and everything else
    | [<CompiledName("NoClozeGlossaryDefinitions")>] NoClozeGlossaryDefinitions
    /// The full system!
    | [<CompiledName("Full011720")>] Full011720

let ElaboratedFeedbackCondition( row : string)(condition : AblationCondition) =
    let config = 
        match condition with
        | NoClozeNoDefinitions -> Some <| { UseCloze = false; UseGlossaryDefinitions = false; UseWikipediaDefinitionsForMissingGlossaryDefinitions = false; ElasticDocsContainBothKeys = true; MaxElasticDocs = 3; UseAnswerCoreferenceFilter = true ; SyntheticQuestion = RelationshipQuestion}
        | NoClozeGlossaryDefinitions -> Some <| { UseCloze = false; UseGlossaryDefinitions = true;  UseWikipediaDefinitionsForMissingGlossaryDefinitions = false;  ElasticDocsContainBothKeys = true; MaxElasticDocs = 3; UseAnswerCoreferenceFilter = true ; SyntheticQuestion = RelationshipQuestion}
        | Full011720 -> Some <| { UseCloze = true; UseGlossaryDefinitions = true;  UseWikipediaDefinitionsForMissingGlossaryDefinitions = true;  ElasticDocsContainBothKeys = true; MaxElasticDocs = 3; UseAnswerCoreferenceFilter = true ; SyntheticQuestion = RelationshipQuestion}
  
    let s = row.Split('\t')
    GetElaboratedFeedback s.[0] s.[1] s.[2] config
       
// ISSUES WITH THIS BLOWING UP CUDA WHEN CALLED FROM IJSKERNEL
// [<Emit("async function doSleep() { await new Promise(res => setTimeout(res, $0)); }")>]
// let emitSleep ( milliseconds : int) = jsNative
// [<Emit("doSleep()")>]
// let doSleep () = jsNative
// /// Currently specific to /z/aolney/research_projects/mofacts/analysis/2020-01-13-error-analysis-for-content-generation/termConfusionCountNoQuotes.tsv
// let BatchElaboratedFeedback( multiLineTabDelimitedText : string ) ( condition : BatchCondition )=
//     promise {
//         match condition with
//         | TermOnly -> 
//             let config = Some <| { UseCloze = false; UseDefinitions = true; ElasticDocsContainBothKeys = true; MaxElasticDocs = 3; UseAnswerCoreferenceFilter = true ; SyntheticQuestion = RelationshipQuestion}
//             let rows = multiLineTabDelimitedText.Split('\n') |> Array.skip 1 |> Array.take 20 //DEBUG TAKE


//             // let! r1 = 
//             //     promise {
//             //         let feedbacks = new ResizeArray<JS.Promise<Result<ElaboratedFeedback,FetchError>>>()
//             //         for r in rows do
//             //             let s = r.Split('\t')
//             //             feedbacks.Add(GetElaboratedFeedback s.[0] s.[1] "" config)
//             //             let timeout = 30
//             //             printfn "sleeping for %A" timeout
//             //             do! Promise.sleep (1000 * timeout) // pad sleep to avoid CUDA OOM
//             //         return feedbacks |> Promise.all;
//             //     } 
//             // let! results = r1 
            
//             // This works, but I can't insert a sleep 
//             let timeout = 30
//             emitSleep(1000 * timeout) // pad sleep to avoid CUDA OOM
                  
//             let! results = 
//                 rows 
//                 |> Seq.map( fun r -> 
//                     let s = r.Split('\t')
//                     doSleep()
//                     GetElaboratedFeedback s.[0] s.[1] "" config;
//                     )
//                 |> Promise.all
                

//             let whitespaceRegex = System.Text.RegularExpressions.Regex("\s+")
//             if allOK <| results  then
//                 let newCols = 
//                     results
//                     |> resultsToType
//                     |> Array.map ( fun ef ->  
//                         let json = whitespaceRegex.Replace( toJson <| ef, " ")
//                         ef.ElaboratedFeedback + "|" + json)
                
//                 let outputBody = 
//                     Array.zip rows newCols
//                     |> Array.map( fun (r,nc) -> r.Replace("\t","|") + "|" + nc)
//                     |> String.concat "\n"

//                 let outputHeader = "term1|term2|count|def1|def2|feedback|json\n"
//                 return Ok(outputHeader + outputBody)

//             else
//                 let errorPayload = ( results |> resultsToError |> Array.map (sprintf " error: %A")  )
//                 return Error(errorPayload |> String.concat "\n")
//     }