module QuestionGenerator

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch
open AllenNLP


/// Replace indices in the parse with a string (which may be multiple words).
/// Potentially a many/many correspondence.
type QuestionPlan =
    {
        SourceIndices : int[]
        ReplacementString : string
    } with 
    static member Create(indices, aString ) = { SourceIndices = indices; ReplacementString = aString }
 
[<StringEnum>]
type QuestionType =
    | Hint 
    | Prompt 

/// A rendered question
type Question =
    {
        QuestionType : QuestionType
        Text : string
        Answer : string
    } with 
    static member Create(qType, text, answer ) = { QuestionType = qType; Text = text; Answer = answer }

// NOTE: skipping transformation verification questions; seem pedagogically pointless in this context; TODO use concept map approach to create near-miss verification questions

/// Assumes proper nouns and pronouns are people. TODO add NER
let isPerson ( index : int ) ( sa : SentenceAnnotation ) =
    match sa.dep.pos.[index] with
    | "NNP" | "NNP" -> true
    | "PRP" -> true
    | _ -> false


/// Return the appropriate wh for a specified token, given a nominative/accusative case
/// Examples: John is happy -> Who is happy? ; The cat ate a snack -> What ate a snack?
/// NOTE: This approach ignores queried modifiers where they, not the subject/object, would be replaced,
/// e.g. The green car hit a pole -> What car hit a pole?
let wh ( index : int ) ( isNominative : bool ) ( sa : SentenceAnnotation ) = 
    if sa |> isPerson index then
        if isNominative then
            "who"
        else
            "whom"
    else
        //not super confident about pobj replacing LTH function tag DIR
        if sa.dep.predicted_dependencies.[index] = "pobj" then
            "where"
        else
            "what"

/// Returns a wh substitution for an arbitrary DEPENDENCY clause identified by the passed function
/// Example: make a SUBJECT position prompt question 
let whDependencySubstitution ( getIndex ) ( isNominative : bool ) ( sa : SentenceAnnotation ) = 
    // Find an index according to the getIndex function
    match sa |> getIndex with
    | Some( index ) ->
        // Get the dependent indices of the found index
        let dependentIndices =  sa |> getDependentIndices index
        // Map a wh
        let whString = sa |> wh index isNominative
        // Wrap in a substitution
        QuestionPlan.Create(dependentIndices,whString) |> Some
    | None -> None

/// Returns a wh/subject substitution, i.e. to make a SUBJECT position prompt question 
let whSubjectSubstitution ( sa : SentenceAnnotation ) = 
    sa |> whDependencySubstitution getSubjectIndex true
    
/// Returns a wh/object substitution, i.e. to make an OBJECT position prompt question 
let whObjectSubstitution ( sa : SentenceAnnotation ) = 
    sa |> whDependencySubstitution getPredicateIndex false
    
/// Returns wh/adjunct substitutions
/// TODO: add auxiliary verbs?
let whAdjunctSubstitutions ( sa : SentenceAnnotation ) = 
    sa.srl.verbs
    //Create a map from tag to indices for each predicate in the SRL parse
    |> Array.map( fun verb -> verb.tags |> srlArgToIndexMap |> Map.toArray )
    |> Array.collect( fun map -> 
        map
        |> Array.map( fun (adjuct,indexTuples) -> 
            let indices = indexTuples |> Array.map snd
            match adjuct with
            | "ARGM-CAU" -> QuestionPlan.Create( indices, "why") |> Some
            | "ARGM-DIR" -> QuestionPlan.Create( indices, "where") |> Some
            | "ARGM-LOC" -> QuestionPlan.Create( indices, "where") |> Some
            | "ARGM-MNR" -> QuestionPlan.Create( indices, "how") |> Some
            | "ARGM-TMP" -> QuestionPlan.Create( indices, "when") |> Some
            | _ -> None
        )
    ) |> Array.toList

/// Make prompt plans for substitutions for everything we can, e.g. subjects and predicates
/// TODO: add more targets?
let getQuestionPlans ( sa : SentenceAnnotation ) = 
    (sa |> whSubjectSubstitution)
    ::
    (sa |> whObjectSubstitution)
    ::
    (sa |> whAdjunctSubstitutions)
    |> List.choose id

let prompt( sa : SentenceAnnotation ) ( sub : QuestionPlan) =
    //Set to look up indices of substituted words
    let subIndiceSet = Set sub.SourceIndices
    
    //Drop the subsituted words except one, which we exchange with replacement string
    let text =
        sa.dep.words
        //Add indices for checking set
        |> Array.mapi( fun i w -> i,w ) 
        |> Array.choose( fun (i,w) -> 
            //Arbitrarily use the min element as the replacement point
            if i = subIndiceSet.MinimumElement then
                sub.ReplacementString |> Some
            //Drop anything else in the sub set
            elif subIndiceSet.Contains i then
                None
            //Pass on the original word unchanged
            else
                w |> Some
            ) 
        |> String.concat " "
    Question.Create( QuestionType.Prompt, text, sub.ReplacementString)
    
let mutable hintIndex = 0
// Once we add concept map info, we can generate less generic hints by edge type, see /z/aolney/research_projects/guru/code/initial_prototype_system/Databases/questionTemplates.txt
let hint( sa : SentenceAnnotation ) (sub : QuestionPlan) =
    let hintTemplates = 
        [
            "And what do we know about #"
            "What can you say about #"
            "Tell me about #"
            "Tell me what you know about #"
            "Can you tell me about #"
            "What do you know about #"
        ]
    hintIndex <- hintIndex + 1
    let template = hintTemplates.[ hintIndex % (hintTemplates.Length - 1) ]
    let filler = sub.SourceIndices |> Array.map( fun i -> sa.dep.words.[i]) |> String.concat " "
    let text = template.Replace("#", filler)
    Question.Create( QuestionType.Hint, text, sub.ReplacementString)

/// Get questions from a sentence annotation. Uses all generators. 
/// Not all generators need to use the same plans, but plans can sometimes be reused 
/// across generators.
let GetQuestions ( sa : SentenceAnnotation ) =
    let plans = sa |> getQuestionPlans
    //
    (plans |> List.map (prompt sa))
    @
    (plans  |> List.map (hint sa))
    