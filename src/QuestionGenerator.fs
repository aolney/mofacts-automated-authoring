module QuestionGenerator

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch
open AllenNLP

///z/aolney/research_projects/braintrust/code/braintrust-ace-generator
///z/aolney/repos/FreeformMapGenerator

/// Replace indices in the parse with a string (which may be multiple words).
/// Potentially a many/many correspondence.
/// When movement is required (wh or aux) we substitute to negative indicies
type Substitution =
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
        Substitution.Create(dependentIndices,whString) |> Some
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
            | "ARGM-CAU" -> Substitution.Create( indices, "why") |> Some
            | "ARGM-DIR" -> Substitution.Create( indices, "where") |> Some
            | "ARGM-LOC" -> Substitution.Create( indices, "where") |> Some
            | "ARGM-MNR" -> Substitution.Create( indices, "how") |> Some
            | "ARGM-TMP" -> Substitution.Create( indices, "when") |> Some
            | _ -> None
        )
    ) |> Array.toList

/// Make prompt plans for substitutions for everything we can, e.g. subjects and predicates
/// TODO: add more targets?
let getSubstitutions ( sa : SentenceAnnotation ) = 
    (sa |> whSubjectSubstitution)
    ::
    (sa |> whObjectSubstitution)
    ::
    (sa |> whAdjunctSubstitutions)
    |> List.choose id

/// TODO: UNFINISHED, WAITING TO SEE IF THIS IS CONSIDERED A REQUIRED FEATURE
/// FOLLOW UP WOULD BE QUESTIONS.FS IN FREEFORMMAPGENERATOR, WOULD NEED LEMMINFLECT CALLS
/// Aux movement/insertion is necessary for dobj type questions with wh movement
/// Example: John ate chicken -> John ate what ; this requires no aux and has no movement
/// Example: John ate chicken -> What did John eat ; this has wh movement and requires a dummy do
let auxSubstitution ( sa : SentenceAnnotation ) =
    let beIndex = sa |> getBeRootIndex
    let auxIndex = sa |> getInvertAuxIndex
    match auxIndex,beIndex with
    | Some(aux),_ -> () //move the aux, be stays in place
    | None, Some(be) -> () //move the be
    | None, None -> () //insert a do
    //aux
    //be
    //else do

/// Extract token substring for sentence annotation using given indices
let indicesToSubstring ( sa : SentenceAnnotation ) ( indices : int[]) =
    indices |> Array.map( fun i -> sa.dep.words.[i]) |> String.concat " "

let trimPunctuation ( text : string ) = 
    text.Trim([|'.';' '|])

/// Make first letter uppercase and change punctuation to ?
let questionCase ( text : string ) =
    (text |> String.mapi( fun i c -> match i with | 0 -> (Char.ToUpper(c)) | _ -> c) |> trimPunctuation) + "?"
 
/// Genearte a prompt question
let prompt( sa : SentenceAnnotation ) ( sub : Substitution) =
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
    Question.Create( QuestionType.Prompt, text |> questionCase, sub.SourceIndices |> indicesToSubstring sa)
    
let mutable hintIndex = 0
// Once we add concept map info, we can generate less generic hints by edge type, see /z/aolney/research_projects/guru/code/initial_prototype_system/Databases/questionTemplates.txt
/// Generate a hint
let hint( sa : SentenceAnnotation ) (sub : Substitution) =
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
    let subIndiceSet = Set sub.SourceIndices
    //NOTE: could possibly use entire sentence as the completion, but that would violate the multi-component property of hints
    //Could make completion a complete proposition?
    Question.Create( QuestionType.Hint, text |> questionCase, [| 0 .. sa.dep.words.Length - 1 |] |> Array.filter(subIndiceSet.Contains >> not) |> indicesToSubstring sa |> trimPunctuation)

/// Get questions from a sentence annotation. Uses all generators. 
/// Not all generators need to use the same plans, but plans can sometimes be reused 
/// across generators.
let GetQuestions ( sa : SentenceAnnotation ) =
    let plans = sa |> getSubstitutions
    (plans |> List.map (prompt sa))
    @
    (plans  |> List.map (hint sa))
    |> List.toArray
    

// let HintQuestionFromPredicates ( parse : LthWrapper.ParseResult ) =
//     let hintQuestionList = new System.Collections.Generic.List<QARecord>()

//     for predicate in parse.Predicates do
//         if IsVerb( predicate.node.pos ) then //no nominal predicates
//             for argument in predicate.args do
//                 if  argument.argLabel = LthWrapper.Arg.A0 || 
//                     argument.argLabel = LthWrapper.Arg.A1 ||
//                     argument.argLabel = LthWrapper.Arg.A2  then

//                     //the phrase we inject into the template
//                     let dependentPhrase = GetDependentPhrase( argument.arg ).ToLower()
                
//                     //fill out a random template
//                     let hint = FillHintTemplate( dependentPhrase ) 

//                     //create a dummy record with the answer/fact as all fields
//                     let qaRecord = new QARecord( parse.Nodes, parse.Nodes )

//                     //update the record with the hint question
//                     qaRecord.QuestionString <- hint 

//                     hintQuestionList.Add( qaRecord )

//     //return
//     hintQuestionList
