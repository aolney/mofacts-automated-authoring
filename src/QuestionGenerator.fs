module QuestionGenerator

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch
open AllenNLP

///z/aolney/research_projects/braintrust/code/braintrust-ace-generator
///z/aolney/repos/FreeformMapGenerator
/// 

/// Logging information
type Tag =
    | IsPerson
    | WhDependency of string
    | IsLocation
    | WhTarget of int * string
    | FocusTarget of int * string
    | Adjunct of string
    | WhArg of string
    | DisfluentArg
    /// Debug information
    | Trace of string

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
        /// The indices that are the focus of the question (i.e. present in the question)
        FocusIndices : int[]
        /// List of tags for analysis and debugging
        Tags : Tag[]
    } with 
    static member Create(sa, start, stop, replacementIndices, replacementString, focusIndices, tags ) = { SentenceAnnotation=sa; Start=start; Stop=stop; ReplacementIndices = replacementIndices; ReplacementString = replacementString; FocusIndices = focusIndices; Tags=tags }
 
[<StringEnum>]
type QuestionType =
    /// E.g. What can you say about gravity?
    | Hint 
    /// E.g. The force of gravity is ____ ?
    | Prompt 

/// A rendered question
type Question =
    {
        /// E.g. hint or prompt
        QuestionType : QuestionType
        /// The question itself
        Text : string
        /// The focus of the question, a component of the Text
        Focus : string
        /// The answer to the question
        Answer : string
        /// List of tags for analysis and debugging
        Tags : Tag[]
    } with 
    static member Create(qType, text, focus, answer,tags ) = { QuestionType = qType; Text = text; Focus=focus; Answer = answer; Tags=tags }

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
    let tags = ResizeArray<Tag>()
    tags.Add( WhDependency <| sa.dep.predicted_dependencies.[index])
    tags.Add(WhTarget(index, sa.dep.words.[index] ) )
    let wh = 
        if sa |> isPerson index then
            tags.Add(IsPerson)
            if isNominative then
                "who"
            else
                "whom"
        else
            //not super confident about pobj replacing LTH function tag DIR
            if sa.dep.predicted_dependencies.[index] = "pobj" then
                tags.Add(IsLocation)
                "where"
            else
                "what"
    wh,tags

// TODO: see below for whSyntacticArgSubstitution
/// Returns a wh substitution for an arbitrary DEPENDENCY clause identified by the passed function
/// Example: make a SUBJECT position prompt question 
// let whDependencySubstitution  ( isNominative : bool ) ( sa : SentenceAnnotation ) indexOption = 
//     let tags = ResizeArray<Tag>()
//     // Find an index according to the getIndex function
//     match indexOption with //sa |> getIndex with
//     | Some( index ) ->
//         // Get the dependent indices of the found index
//         let dependentIndices =  sa |> getDependentIndices index
//         // Map a wh
//         let whString,whTags = sa |> wh index isNominative
//         tags.AddRange(whTags)
//         // Wrap in a substitution
//         Substitution.Create(sa, 0, sa.dep.words.Length, dependentIndices, whString, tags.ToArray()) |> Some
//     | None -> None

/// Check if an index is the root; zero index the predicted head
let isRoot sa index =
    sa.dep.predicted_heads.[index - 1] = 0

/// Ad hoc for Stanford Dependencies
let whSyntacticSubjectFilter( subOption : Substitution option) =
    match subOption with
    | Some(sub) ->
        let sa = sub.SentenceAnnotation
        let mutable filterOut = false
        // Disallow subject substitution when be is the specifier of a comp, e.g. The difference is that...
        filterOut <- sa.dep.predicted_dependencies |> Array.mapi( fun i x -> i,x) |> Array.exists( fun (i,d) -> 
            d.EndsWith("comp") && sa.dep.predicted_heads.[i] |> isRoot sa ) 
        if filterOut then None else subOption
    | None -> None

///TODO: needs to be redone in include question focus, so subject/object need to be done simultaneously like SRL is as of 1/24/21
/// Returns substitutions for wh/subject and wh/objects for SUBJECT/OBJECT prompt questions
// let whSyntacticArgSubstitution ( sa : SentenceAnnotation ) = 
//     //NOTE: rejecting substitutions in some situations in an ad hoc way; TODO: make this more rule like
//     // Disallow subject substitution when be is the specifier of a comp, e.g. The difference is that...
//     let subjectSubstitution = sa |> getSubjectIndex |> whDependencySubstitution true sa |> whSyntacticSubjectFilter
//     let objectSubstitutions = sa |> getObjectIndices |> Array.map(whDependencySubstitution false sa)
//     let subs = ResizeArray<Substitution option>()
//     subs.Add( subjectSubstitution )
//     subs.AddRange(objectSubstitutions)
//     subs
    
    // if subjectSubstitution.IsSome && objectSubstitutions.Count > 0 then
    //     objectSubstitutions.Add( subjectSubstitution )
    //     objectSubstitutions
    // else 
    //     [] |> ResizeArray
 

// /// Returns a wh/subject substitution, i.e. to make a SUBJECT position prompt question 
// let whSubjectSubstitution ( sa : SentenceAnnotation ) = 
//     sa |> getSubjectIndex |> whDependencySubstitution true sa
    
// /// Returns a wh/object substitution, i.e. to make an OBJECT position prompt question 
// let whObjectSubstitution ( sa : SentenceAnnotation ) = 
//     sa |> getObjectIndices |> Array.map(whDependencySubstitution false sa)
    
/// Returns wh/adjunct substitutions
/// Deprecated: adjunct substitutions can be buried deep in structure; **TODO: without wh movement + aux this makes "bad" questions**
// let whAdjunctSubstitutions ( sa : SentenceAnnotation ) = 
//     let tags = ResizeArray<Tag>()
//     sa.srl.verbs
//     //Create a map from tag to indices for each predicate in the SRL parse
//     |> Array.map( fun verb -> verb.tags |> srlArgToIndexMap |> Map.toArray )
//     |> Array.collect( fun map -> 
//         map
//         |> Array.map( fun (adjuct,indexTuples) -> 
//             let indices = indexTuples |> Array.map snd
//             //disallow adjucts that are WRB, e.g. "how" in "how it works," by checking first index
//             // if sa.dep.pos.[indices.[0]] <> "WRB" then
//             match adjuct with
//             | "ARGM-CAU" -> tags.Add( Adjunct <| "ARGM-CAU"); Substitution.Create( sa, 0, sa.dep.words.Length, indices, "why", tags.ToArray()) |> Some
//             | "ARGM-DIR" -> tags.Add( Adjunct <| "ARGM-DIR"); Substitution.Create( sa, 0, sa.dep.words.Length,indices, "where",tags.ToArray()) |> Some
//             | "ARGM-LOC" -> tags.Add( Adjunct <| "ARGM-LOC"); Substitution.Create( sa, 0, sa.dep.words.Length,indices, "where",tags.ToArray()) |> Some
//             | "ARGM-MNR" -> tags.Add( Adjunct <| "ARGM-MNR"); Substitution.Create( sa, 0, sa.dep.words.Length,indices, "how",tags.ToArray()) |> Some
//             | "ARGM-TMP" -> tags.Add( Adjunct <| "ARGM-TMP"); Substitution.Create( sa, 0, sa.dep.words.Length,indices, "when",tags.ToArray()) |> Some
//             | _ -> None
//             // else
//             //     None
//         )
//     ) |> ResizeArray //|> Array.toList

/// True if indices contain a DOBJ or NSUBJX that spans the indices
let nominalSpanRootOption sa indices =
    let spanWords = indices |> Array.map( fun i -> sa.dep.words.[i] ) |> String.concat " "
    indices |> Array.tryFind( fun i -> 
        let d = sa.dep.predicted_dependencies.[i]
        if d.StartsWith("nsubj") || d = "dobj" || d = "iobj" then
            let dependentIndices = sa |> getDependentIndices i
            (Set.ofArray dependentIndices).IsSupersetOf( Set.ofArray indices )
        else
            false
        )

/// For each SRL frame, return a wh question for ARGN IFF
/// 1. There is more than 1 ARGN in the frame
/// 2. ARGN contains a DOBJ or NSUBJX
/// 3. The DOBJ or NSUBJX spans the entire ARGN (exclude verb particles, forces more specific questions/no overlap)
/// ORIGINAL TODO: rework this so that
/// 1. questions are scoped to srl frames
/// 2. srl frames are analyzed to identify best overlap, e.g. 
/// Cells are specialized to take on specific and "necessary responsibilities", and together they maintain an environment within the body in which they can all live.
/// 3. (likely) questions have wh movement with aux so that wh is not buried in a complex clause (TODO: specifically for adjuncts)
let whSrlSubstitutions ( sa : SentenceAnnotation ) = 
    sa.srl.verbs
    //Create a map from tag to indices for each predicate in the SRL parse
    |> Array.map( fun verb -> verb.tags |> srlArgToIndexMapWithCollapsedReferents |> Map.toArray )
    //Select SRL frames for question generation according to our criteria
    |> Array.choose( fun mapArr -> 
        let frameTags = ResizeArray<Tag>()

        //for debug
        let debugAlignment = mapArr |> Array.collect snd |> Array.sortBy snd |> Array.map( fun (a,i) -> {| arg=a ; word=sa.dep.words.[i] ; pos=sa.dep.pos.[i] |}  )

        //Count the ARGNs, ignoring ARGM/adjuncts
        let argNs = mapArr |> Array.filter( fun (argType,_) -> argType.StartsWith("ARG") && not <| argType.StartsWith("ARGM") ) |> Array.distinctBy fst 

        //Filtering concept: 1/24/21
        //Sometimes we wish to disallow argN substitutions based on the properties of other argNs in the same frame, rather than properties of the argN itself
        //For SRL, we identify arguments that begin or end with IN/WDT as being WH substitutable but **not** being acceptable when other arguments are WH substituted
        //therefore, when one of these exist, we remove all other argNs from consideration
        let disfluentArgN = argNs |> Array.filter( fun (_, indexTuples ) -> 
            let argNIndices = indexTuples |> Array.map snd 
            let startPos = sa.dep.pos.[argNIndices.[0]]
            let stopPos = sa.dep.pos.[argNIndices |> Array.last ]
            startPos = "IN" || startPos = "WDT" || stopPos = "IN" || stopPos = "WDT"
            )
    
        //If we have no disfluent arguments, proceed with all arguments
        //If we have exactly one disfluent argument, keep it for substitution and throw the rest away
        //If we have more than one disfluent argument, throw away the entire frame. TODO: transformations would let us salvage this case
        let finalArgNs = 
            match disfluentArgN.Length with
            | 0 -> argNs
            | 1 -> frameTags.Add(DisfluentArg) ; disfluentArgN
            | _ -> [||]
        
        //filter out frames without at least 2 ARGN; NOTE: 1/21 why?
        // if argNs.Length  < 2 then
        //     None
        // TODO: rewrite to allow non-nominal spans but only generate questions from nominal spans
        //filter out frames unless all the ARGNs are nominal spans (i.e. contain a DOBJ or NSUBJX spans the entire ARGN (exclude verb particles, forces more specific questions/no overlap)
        // else
        //Get roots of nominals spans if they exist
        // let nominalSpanRoots = argNs |> Array.choose( fun (_, indexTuples ) -> indexTuples |> Array.map snd |> nominalSpanRootOption sa )
        //If all ARGNs are nominal spans, lengths should match 
        // NOTE: things are blowing up here b/c of Stanford Dependencies
        // TODO rewrite to filter out ARGN that don't have nominal span roots, rather than using this length check
        // if argNs.Length <> nominalSpanRoots.Length then
        //     None
        // else 
        //start/stop are frame level
        let indices = argNs |> Array.collect( fun (_,indexTuples) -> indexTuples |> Array.map snd )
        let start = indices |> Array.min
        let stop = indices |> Array.max

        //to get question focus, split finalArgNs into the replacement argument and the rest
        let replaceFocusTuples = [|
            for a in finalArgNs do
                let argNsList = argNs |> ResizeArray
                //argNsList now fluent b/c we removed one and only disfluent arg
                argNsList.Remove(a) |> ignore
                yield a,argNsList
        |]
        //for each tuple, create a substitution
        replaceFocusTuples 
        |> Array.mapi( fun i ((argN, indexTuples ),fans) ->
            let tags = ResizeArray<Tag>()
            tags.AddRange(frameTags)
            tags.Add(WhArg <| argN)
            let argNIndices = indexTuples |> Array.map snd 
            let argNRoot = getRootOfSpan argNIndices.[0] (argNIndices |> Array.last) sa
            let whString, whTags = sa |> wh argNRoot ( argN.EndsWith("0") ) //assume ARG0 is nominative; TODO: clean up wh with NER or similar
            tags.AddRange(whTags)
            //If we have > 2 arguments, we have a choice of what to focus. As a general rule, the lower args are more central
            let focusIndices = 
                if fans.Count > 0 then
                    let indices = fans.[0] |> snd |> Array.map snd
                    let focusIndex = getRootOfSpan indices.[0] (indices |> Array.last) sa
                    let focusWord = sa.dep.words.[focusIndex]
                    tags.Add( FocusTarget(focusIndex,focusWord) )
                    indices
                else
                    [||]
            
            Substitution.Create(sa, start, stop, argNIndices,whString,focusIndices,tags.ToArray()) |> Some
        ) |> Some
            
    )
    |> Array.collect id
    |> ResizeArray //Array.toList


let getSubstitutions ( sa : SentenceAnnotation ) = 
    // let whSyn = sa |> whSyntacticArgSubstitution
    // TODO: requires aux and movement to be functional
    // let whAdj = sa |> whAdjunctSubstitutions
    let whSrl = sa |> whSrlSubstitutions

    let subs = new ResizeArray<Substitution option>()
    // subs.AddRange( whSyn )
    // subs.AddRange( whAdj )
    subs.AddRange(whSrl)
    subs |> Seq.choose id |> Seq.toArray

// /// Make substitution plans using various annotations. Each plan contains information to generate questions via surface transformation
// let getSubstitutions ( sa : SentenceAnnotation ) = 
//     let whSrl = sa |> whSrlSubstitutions

//     let subs = new ResizeArray<Substitution option>()
//     subs.AddRange(whSrl)

//     subs |> Seq.choose id |> Seq.toArray

// /// If getSubstitutions fails, we can back off to this method which uses the dependency parse
// /// Otherwise we're in a position where we won't generate any questions!
// let getSubstitutionsFailSafe ( sa : SentenceAnnotation ) = 
//     // let whSubj = sa |> whSubjectSubstitution
//     // let whObj = sa |> whObjectSubstitution

//     let subs = new ResizeArray<Substitution option>()
//     subs.AddRange( sa |> whSyntacticArgSubstitution )
//     // subs.Add( whSubj)
//     // subs.AddRange(whObj)

//     subs |> Seq.choose id |> Seq.toArray
    

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
    indices |> Array.map( fun i -> if i = 0 then sa.dep.words.[i].ToLower() else sa.dep.words.[i]) |> String.concat " "

let trimPunctuation ( text : string ) = 
    text.Trim([|'.';' ';','|])

/// Make first letter uppercase and change punctuation to ?
let questionCase ( text : string ) =
    (text |> String.mapi( fun i c -> match i with | 0 -> (Char.ToUpper(c)) | _ -> c) |> trimPunctuation) + "?"
 
/// Generate a prompt question
let prompt( sa : SentenceAnnotation ) ( sub : Substitution) =
    let tags = ResizeArray<Tag>()
    tags.AddRange(sub.Tags)

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
        Some <| Question.Create( QuestionType.Prompt, text |> questionCase,  sub.FocusIndices |> indicesToSubstring sa, sub.ReplacementIndices |> indicesToSubstring sa, tags.ToArray())
        
let mutable hintIndex = 0
// TODO if we had predicate WSD, we could generate less generic hints by type, see /z/aolney/research_projects/guru/code/initial_prototype_system/Databases/questionTemplates.txt
// NOTE 1/24/21: the disfluency logic on srl means that we need to flip the logic here: the replacement indices should **not** be shown, because they could be disfluent
/// Generate a hint
let hint( sa : SentenceAnnotation ) (sub : Substitution) =
    let tags = ResizeArray<Tag>()
    tags.AddRange(sub.Tags)

    let hintTemplates = 
        [|
            "And what do we know about #"
            "What can you say about #"
            "Tell me about #"
            "Tell me what you know about #"
            "Can you tell me about #"
            "What do you know about #"
        |]
    hintIndex <- hintIndex + 1
    let template = hintTemplates.[ hintIndex % (hintTemplates.Length - 1) ]
    //Filters
    // Can't hint without a focus, so skip
    if sub.FocusIndices = Array.empty || 
        //Long hints are functionally prompts; skip when focus length is above threshold
        sub.FocusIndices.Length > 15 ||
        //prevent hints without focus and prevent single pronoun hints, e.g. Tell me about it
        (sub.ReplacementIndices.Length = 1 &&  sa.dep.pos.[ sub.ReplacementIndices.[0] ].StartsWith("PRP") ) then
        None
    else
        // let filler = sub.ReplacementIndices |> Array.map( fun i -> if i > 0 then sa.dep.words.[i] else sa.dep.words.[i].ToLower() ) |> String.concat " "
        let focusString = sub.FocusIndices |> indicesToSubstring sa //sub.FocusIndices |> Array.map( fun i -> if i > 0 then sa.dep.words.[i] else sa.dep.words.[i].ToLower() ) |> String.concat " "
        let text = template.Replace("#", focusString)
        // let subIndiceSet = Set sub.ReplacementIndices
        let subIndiceSet = Set sub.FocusIndices
        //NOTE: what to use as answer/completion? Options:
        // 1. Could possibly use entire sentence as the completion, but that would violate the multi-component property of hints
        // 2. Could remove focus from sentence and use that as completion, but that requires a lot of student production for long sentences
        // i.e. a 40 word sentence should not be an expectation, but that is effectively what we are doing right now in dialogue
        // let answer = [| 0 .. sa.dep.words.Length - 1 |] |> Array.filter(subIndiceSet.Contains >> not) |> indicesToSubstring sa |> trimPunctuation
        // 3. Use the replacement as the completion. This is more realistic for student production and helps us use cohesion between previous question
        // answers and current question focus to give students clues during dialogue
        let answer = sub.ReplacementIndices |> indicesToSubstring sa |> trimPunctuation
        Some <| Question.Create( QuestionType.Hint, text |> questionCase, focusString, answer,  tags.ToArray())


let GetQuestions ( sa : SentenceAnnotation ) =
    let plans =  
        sa 
        |> getSubstitutions
        // promote subs that have a rare focus to avoid generic/nonspecific questions
        |> Array.sortBy( fun s ->
            s.FocusIndices |> Array.map( fun i -> sa.dep.words.[i] |> WordFrequency.Get )|> Array.min 
        )

    // generate questions and remove duplicates by type
    let prompts = plans |> Array.choose (prompt sa) |> Array.distinctBy( fun p -> p.Answer)
    let hints = plans  |> Array.choose (hint sa) |> Array.distinctBy( fun h -> h.Answer)
    
    let questions = ResizeArray<Question>()
    questions.AddRange(prompts)
    questions.AddRange(hints)
    questions |> Seq.toArray

// /// Get questions from a sentence annotation using substitution plans. 
// let GetQuestions ( sa : SentenceAnnotation ) =
//     let plans = 
//         match sa |> getSubstitutions with
//         // | [||] -> sa |> getSubstitutionsFailSafe
//         | s -> s 

//     let prompts = plans |> Array.choose (prompt sa)
//     let hints = plans  |> Array.choose (hint sa)
    
//     let questions = ResizeArray<Question>()
//     questions.AddRange(prompts)
//     questions.AddRange(hints)
//     questions |> Seq.toArray
    
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

let HarnessGetQuestions sentence = 
    promise {
        let! daResult = AllenNLP.GetNLP None sentence
        match daResult with 
        | Ok(da) -> 
            let questions = da.sentences |> Array.head |> GetQuestions
            return Ok(questions)
        | Error(e) -> return Error(e)
    }

let InitializeTest() = 
    "The difference is that the cerebrum is a part of the brain in the upper part of the cranial cavity that provides higher mental functions, and the cerebellum is a part of the brain that coordinates skeletal muscle movement."
