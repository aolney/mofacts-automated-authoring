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
        /// The NLP information for the sentence involved in the substitution
        SentenceAnnotation : SentenceAnnotation
        /// The index for the first token in the output
        Start : int
        /// The index for the last token in the output
        Stop : int
        /// The indices that will be replaced by the substitution
        ReplacementIndices : int[]
        /// The string the will replace the aforementioned indices
        ReplacementString : string
    } with 
    static member Create(sa, start, stop, indices, aString ) = { SentenceAnnotation=sa; Start=start; Stop=stop; ReplacementIndices = indices; ReplacementString = aString }
 
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

/// Always false; TODO add NER
let isPerson ( index : int ) ( sa : SentenceAnnotation ) =
    match sa.dep.pos.[index] with
    | "NNP" | "NNP" -> false //true
    | "PRP" -> false //true
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
        Substitution.Create(sa, 0, sa.dep.words.Length, dependentIndices,whString) |> Some
    | None -> None

/// Returns a wh/subject substitution, i.e. to make a SUBJECT position prompt question 
let whSubjectSubstitution ( sa : SentenceAnnotation ) = 
    sa |> whDependencySubstitution getSubjectIndex true
    
/// Returns a wh/object substitution, i.e. to make an OBJECT position prompt question 
let whObjectSubstitution ( sa : SentenceAnnotation ) = 
    sa |> whDependencySubstitution getPredicateIndex false
    
/// Returns wh/adjunct substitutions
/// Deprecated: adjunct substitutions can be buried deep in structure; without wh movement + aux this makes "bad" questions
let whAdjunctSubstitutions ( sa : SentenceAnnotation ) = 
    sa.srl.verbs
    //Create a map from tag to indices for each predicate in the SRL parse
    |> Array.map( fun verb -> verb.tags |> srlArgToIndexMap |> Map.toArray )
    |> Array.collect( fun map -> 
        map
        |> Array.map( fun (adjuct,indexTuples) -> 
            let indices = indexTuples |> Array.map snd
            //disallow adjucts that are WRB, e.g. "how" in "how it works," by checking first index
            if sa.dep.pos.[indices.[0]] <> "WRB" then
                match adjuct with
                | "ARGM-CAU" -> Substitution.Create( sa, 0, sa.dep.words.Length, indices, "why") |> Some
                | "ARGM-DIR" -> Substitution.Create( sa, 0, sa.dep.words.Length,indices, "where") |> Some
                | "ARGM-LOC" -> Substitution.Create( sa, 0, sa.dep.words.Length,indices, "where") |> Some
                | "ARGM-MNR" -> Substitution.Create( sa, 0, sa.dep.words.Length,indices, "how") |> Some
                | "ARGM-TMP" -> Substitution.Create( sa, 0, sa.dep.words.Length,indices, "when") |> Some
                | _ -> None
            else
                None
        )
    ) |> Array.toList

/// True if indices contain a DOBJ or NSUBJX that spans the indices
let nominalSpanRootOption sa indices =
    //find the first DOBJ IOBJ or NSUBJX
    //let rootIndex = sa.dep.predicted_dependencies |> Array.findIndex( fun h -> h.StartsWith("nsubj") || h = "dobj" || h = "iobj" ) 
    let rootIndexOption = indices |> Array.tryFind( fun i -> 
        let d = sa.dep.predicted_dependencies.[i]
        d.StartsWith("nsubj") || d = "dobj" || d = "iobj" ) 
    match rootIndexOption with
    | Some(rootIndex) ->
        let dependentIndices = sa |> getDependentIndices rootIndex
        // if indices = dependentIndices then
        // allow dependent indices to include or equal indices
        if (Set.ofArray dependentIndices).IsSupersetOf( Set.ofArray indices ) then
            Some(rootIndex)
        else
            None
    | None -> None
    

/// For each SRL frame, return a wh question for ARGN IFF
/// 1. There is more than 1 ARGN in the frame
/// 2. ARGN contains a DOBJ or NSUBJX
/// 3. The DOBJ or NSUBJX spans the entire ARGN (exclude verb particles, forces more specific questions/no overlap)
/// ORIGINAL TODO: rework this so that
/// 1. questions are scoped to srl frames
/// 2. srl frames are analyzed to identify best overlap, e.g. Cells are specialized to take on specific and "necessary responsibilities", and together they maintain an environment within the body in which they can all live.
/// 3. (likely) questions have wh movement with aux so that wh is not buried in a complex clause (TODO: specifically for adjuncts)
let whSrlSubstitutions ( sa : SentenceAnnotation ) = 
    sa.srl.verbs
    //Create a map from tag to indices for each predicate in the SRL parse
    |> Array.map( fun verb -> verb.tags |> srlArgToIndexMap |> Map.toArray )
    //Select SRL frames for question generation according to our criteria
    |> Array.choose( fun map -> 
        //Count the ARGNs, ignoring ARGM/adjuncts
        let argNs = map |> Array.filter( fun (argType,_) -> argType.StartsWith("ARG") && not <| argType.StartsWith("ARGM") ) |> Array.distinctBy fst 
        //filter out frames without at least 2 ARGN
        if argNs.Length  < 2 then
            None
        // TODO: rewrite to allow non-nominal spans but only generate questions from nominal spans
        //filter out frames unless all the ARGNs are nominal spans (i.e. contain a DOBJ or NSUBJX spans the entire ARGN (exclude verb particles, forces more specific questions/no overlap)
        else
            //Get roots of nominals spans if they exist
            let nominalSpanRoots = argNs |> Array.choose( fun (_, indexTuples ) -> indexTuples |> Array.map snd |> nominalSpanRootOption sa )
            //If all ARGNs are nominal spans, lengths should match 
            if argNs.Length <> nominalSpanRoots.Length then
                None
            else 
                //start/stop are frame level
                let indices = argNs |> Array.collect( fun (_,indexTuples) -> indexTuples |> Array.map snd )
                let start = indices |> Array.min
                let stop = indices |> Array.max
                //for each argN, create a substitution
                argNs 
                |> Array.mapi( fun i (argN, indexTuples ) ->
                    let argNIndices = indexTuples |> Array.map snd 
                    let whString = sa |> wh nominalSpanRoots.[i] ( argN.EndsWith("0") ) //assume ARG0 is nominative; TODO: clean up wh with NER or similar
                    Substitution.Create(sa, start, stop, argNIndices,whString) |> Some
                ) |> Some
                
    )
    |> Array.collect id
    |> Array.toList

/// Make prompt plans for substitutions for everything we can, e.g. subjects and predicates
/// TODO: add more targets?
let getSubstitutions ( sa : SentenceAnnotation ) = 
    // whSubject/whObject are included in SRL approach
    // (sa |> whSubjectSubstitution)
    // ::
    // (sa |> whObjectSubstitution)
    // ::
    // whAdjunct is deprecated for now
    //(sa |> whAdjunctSubstitutions)
    (sa |> whSrlSubstitutions)
    |> List.choose id

/// If getSubstitutions fails, we can back off to this method which uses the dependency parse
/// Otherwise we're in a position where we won't generate any questions!
let getSubstitutionsFailSafe ( sa : SentenceAnnotation ) = 
    (sa |> whSubjectSubstitution)
    ::
    [sa |> whObjectSubstitution]

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
    let subIndiceSet = Set sub.ReplacementIndices
    
    //Something very bad has happened and we can't work with this Substitution
    if subIndiceSet.IsEmpty then
        None
    else
        //Drop the subsituted words except one, which we exchange with replacement string
        let text =
            [| for i = sub.Start to sub.Stop do yield i,sa.dep.words.[i] |]
            //Add indices for checking set
            // |> Array.mapi( fun i w -> i,w ) 
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
        Some <| Question.Create( QuestionType.Prompt, text |> questionCase, sub.ReplacementIndices |> indicesToSubstring sa)
        
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
    //prevent single pronoun hints, e.g. Tell me about indexTuples
    if sub.ReplacementIndices.Length = 1 &&  sa.dep.pos.[ sub.ReplacementIndices.[0] ].StartsWith("PRP") then
        None
    else
        let filler = sub.ReplacementIndices |> Array.map( fun i -> if i > 0 then sa.dep.words.[i] else sa.dep.words.[i].ToLower() ) |> String.concat " "
        let text = template.Replace("#", filler)
        let subIndiceSet = Set sub.ReplacementIndices
        //NOTE: could possibly use entire sentence as the completion, but that would violate the multi-component property of hints
        //Could make completion a complete proposition?
        Some <| Question.Create( QuestionType.Hint, text |> questionCase, [| 0 .. sa.dep.words.Length - 1 |] |> Array.filter(subIndiceSet.Contains >> not) |> indicesToSubstring sa |> trimPunctuation)

/// Get questions from a sentence annotation. Uses all generators. 
/// Not all generators need to use the same plans, but plans can sometimes be reused 
/// across generators.
let GetQuestions ( sa : SentenceAnnotation ) =
    let plans = 
        match sa |> getSubstitutions with
        | [] -> sa |> getSubstitutionsFailSafe
        | s -> s

    (plans |> List.choose (prompt sa))
    @
    (plans  |> List.choose (hint sa))
    |> List.toArray
    
// Get questions but quote around the clozed text
let GetQuotedQuestions ( clozeAnswer : string ) ( sa : SentenceAnnotation ) =
    sa
    |> GetQuestions
    |> Array.map( fun q -> 
        { q with Text = System.Text.RegularExpressions.Regex.Replace(q.Text,"\\b" + clozeAnswer + "\\b", "\"" + clozeAnswer + "\"") }
    )

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
