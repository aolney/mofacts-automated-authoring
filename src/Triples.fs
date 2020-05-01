module Triples

//=======================================================

// THIS MODULE IS CURRENTLY NON-FUNCTIONAL

//=======================================================

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json //for Json; might be cleaner way
//TODO: possibly replace with this: https://github.com/thoth-org/Thoth.Fetch
// open Fable.SimpleHttp
open AllenNLP
open DependencyCollapser

//-- Internal ------------------------------
/// A tag such that we can create an object literal (a pojo) from a list of tags
type Tag =
    /// Debug information
    | Trace of string

/// A triple we have generated
/// Rather than treating the triple as a primary representation, we treat it as another annotation on the sentence
/// Other ideas: probability/confidence, score/importance
type Triple =
    {
        /// Start node, token indices
        start : int[] 
        /// Edge node, token indices
        edge : int[]
        /// Stop node, token indices
        stop : int[]
        /// Is the edge negated
        negated : bool
        /// Collected messages reflecting our decision making; primarily for debug purposes (obj b/c some are Tag)
        trace : Tag list
    }

///All data we have collected and will use for final creation of cloze items
type InternalAPI =
    {
        sentences : SentenceAnnotation[]
        coreference : Coreference
        triples : Triple[][]
    }

//---------------------------------------------------------------------

    

let collapseDependencies (sa : SentenceAnnotation) = 
    let ruleTokens = 
        sa.dep.words 
        |> Array.mapi( fun i w -> 
            Rules.Token.Create( i, w, sa.dep.pos.[i], sa.dep.predicted_dependencies.[i], sa.dep.predicted_heads.[i])
        ) |> Array.toList

    let dependencies, dependenciesCC = Collapser.CollapseTokens( ruleTokens )
    //
    dependenciesCC

// MIGRATION NOTES
// 1. Heavily symbolic approach, complex sentences 
// /z/aolney/repos/CGA3/CGA3/CGAExtractor/TriplesFromParse

// /z/aolney/repos/CGA3/CGA3/CGAExtractor/TripleFilter
// 2. Tidy example of high level API; references CGA3 library
// /z/aolney/research_projects/braintrust/materials/NEETS/concept-maps-option-e.bkr

// 3. Modernized freeform approach, uses EDUs
// /z/aolney/research_projects/braintrust/code/NeetsToHtmlAndTasks
// /z/aolney/repos/FreeformMapGenerator

// LTH uses PTB tags and **extended** PTB dependencies 
// http://www.surdeanu.info/mihai/teaching/ista555-fall13/readings/PennTreebankConstituents.html
// https://wacky.sslmit.unibo.it/lib/exe/fetch.php?media=papers:conll-syntax.pdf
// Allen uses PTB tags and standard Stanford dependencies (not universal) https://nlp.stanford.edu/software/dependencies_manual.pdf


/// Get a list of dependent indices from start index
let GetDependentNodes ( start : int ) ( sa : SentenceAnnotation ) =
    let dependents = ResizeArray<int>()
    for h in sa.dep.predicted_heads do
        let mutable hbar = h
        while hbar <> start && hbar <> 0 do
            hbar <- sa.dep.predicted_heads.[hbar]
        if hbar = start then dependents.Add(h)
    //
    dependents.ToArray()

// NOTE: A more direct, though simplified, version of CGA3 follows
// /// Get the subject predicate index of the parse
// /// Had to modify from LTH b/c of different formalism
// /// NSUBJ seems pretty reliable but can sometimes be WH: It was John who (nsubj) came ; in this case "It" is chosen
// let SubjectNode ( sa : SentenceAnnotation ) = 
//     let rootIndex = sa.dep.predicted_heads |> Array.findIndex( fun h -> h = 0) //assuming there is always a root...
//     //the first nsubj preceding the root
//     sa.dep.predicted_dependencies.[0 .. rootIndex] |> Array.tryFindIndexBack( fun h -> h = "nsubj") 
// /// Returns the index of a "be" or copular verb when it can be viewed as the root (anti Stanford, which views copular complement as root)
// /// This check subsumes verb chaining because verb chains are broken and otherwise marked as "aux" rather than "cop"
// let BeRoot ( sa : SentenceAnnotation ) =
//     let rootIndex = sa.dep.predicted_heads |> Array.findIndex( fun h -> h = 0) //assuming there is always a root...
//     //the first copular child of the root
//     sa.dep.predicted_heads 
//     |> Array.tryFindIndex( fun h -> h = rootIndex && sa.dep.predicted_dependencies.[h] = "cop" )
// /// Get the root predicate index of the parse
// /// Had to modify from LTH b/c of different formalism
// /// If ROOT is VB, then take first DOBJ of ROOT : John kissed (root) Mary (dobj) on the head
// /// If ROOT is anything else, then take ROOT: It was John (root) who came ; Sally was happy (root) to see her
// let PredicateNode ( sa : SentenceAnnotation ) = 
//     let rootIndex = sa.dep.predicted_heads |> Array.findIndex( fun h -> h = 0) //assuming there is always a root...
//     if sa.dep.pos.[rootIndex].StartsWith("VB") then
//         //the first child of the verb that is a dobj (starting from the beginning shouldn't matter)
//         sa.dep.predicted_heads 
//         |> Array.tryFindIndex( fun h -> h = rootIndex && sa.dep.predicted_dependencies.[h] = "dobj" )
//     else
//         rootIndex |> Some //TODO: SPAN WILL DOMINATE S
// /// Returns if copula, subject, and predicate exist. **Heavily** simplified from LTH
// let IsATripleDependencyOnly ( sa : SentenceAnnotation ) =
//     let s = sa |> SubjectNode
//     let p = sa |> PredicateNode
//     let b = sa |> BeRoot
//     match s,b,p with
//     | Some(subj),Some(be),Some(pred) -> 
//         {
//             start = sa |> GetDependentNodes subj
//             edge = [| be |]
//             stop = sa |> GetDependentNodes pred
//             trace = [ ]
//         } |> Some
//     | _ -> None

/// Convert SRL BIO tags to a map with key the tag without BIO and value a list of tag/index tuples for that tag
let srlArgToIndexMap (srlTags : string[]) =
    srlTags 
    |> Array.mapi( fun i t -> t.Substring( t.IndexOf("-") ),i)
    |> Array.groupBy fst
    |> Map.ofArray

/// Convert SRL BIO tags to indices for start, edge, stop 
/// Uses a simplistic assumption that lowest ARG is the start node and next ARG is the stop node
let tripleIndicesFromSrlTags (srlTags : string[]) =
    let map = srlTags |> srlArgToIndexMap
    let sortedArgs = map |> Map.toArray |> Array.map fst |> Array.filter( fun a -> a.StartsWith("ARG" ))
    if sortedArgs.Length >= 2 then
        Some(map.[sortedArgs.[0]]), map.TryFind("V"), Some(map.[sortedArgs.[0]])
    else
        None,None,None

//CGA3 implementation was entirely dependency based and allowed for only 1 root be form
//AllenNLP SRL works for be-forms, so we have option of nicely retrieving multiple in a single SA and then using dependencies to fix errors.
/// Returns all be-form triples by jointly using SRL and dependencies
// let IsATriples ( sa : SentenceAnnotation ) =
//     let copTuples = sa.dep.predicted_dependencies |> Array.indexed |>  Array.filter (fun (i, x) -> x = "cop") 
//     let candidateTriples = sa.srl.verbs |> Array.map( fun v -> v.tags |> tripleIndicesFromSrlTags )
//     //Assume the SRL has labeled arguments correctly. TODO error catching when this fails
//     copTuples
//     |> Array.collect( fun (copIndex,_) ->
//         // let s = sa |> SubjectNode
//         // let p = sa |> PredicateNode
//         // let b = sa |> BeRoot
//         candidateTriples
//         |> Array.choose( fun (startOption,edgeOption,stopOption) ->
//             match startOption,edgeOption,stopOption with
//             //Find all three components
//             | Some(start),Some(edge),Some(stop) -> 
//                 //check the edge indices contain the copula index
//                 if edge |> Array.exists( fun (_,i) -> i = copIndex) then
//                     {
//                         start = start |> Array.map snd
//                         edge = edge |> Array.map snd
//                         stop = stop |> Array.map snd //TODO enforce nominal
//                         trace = [ Trace( "copIndex:" + string(copIndex)) ]
//                     } |> Some
//                 else
//                     None
//             | _ -> None
//         )
//     )

// FEEDBACK FOCAL EXAMPLE
// Blood can floow from the atria down into the ______ because there are openings in the horizontal septum that separate them.
// <Student>: artery
// Artery is not right. The right answer is ventricles. 
// You may be confused about the difference between blood vessels and chambers of the heart. 
// Arteries and veins are blood vessesl that connect to the heart. 
// Atria and ventricles are chambers of the heart. The names are similar so they are easily confused.
// IDEAS: 
// - build indexes for key dependencies, loop over srl, do all discovery at once. will condense repetitive actions across code and unify ideas
// - use te to type edges. could be a vector of edge types
// - use soundex for similar sounding words as errors; https://www.google.com/search?q=f%23+soundex
// - include ditransitive verbs
// - abandon triples for vectors. Would help search not explanation/text generation.
// Compare response with correct using all attributes of each (similar)
// Then focus on most salient dimensions in which they are different
// http://verbs.colorado.edu/propbank/EPB-Annotation-Guidelines.pdf
// TODO: 
// - amod, nn, num, (property)
// - appos (isa)
// - cop (isa or property)
// - coref argument substitution
// - ccomp and AM as possible worlds?

/// Get all triples from a sentence using an SRL-first approch
let triplesFromSentence ( sa : SentenceAnnotation ) =
    let copTuples = sa.dep.predicted_dependencies |> Array.indexed |>  Array.filter (fun (i, x) -> x = "cop") 
    let candidateTriples = sa.srl.verbs |> Array.map( fun v -> v.tags |> tripleIndicesFromSrlTags )
    //Assume the SRL has labeled arguments correctly. TODO error catching when this fails
    copTuples
    |> Array.collect( fun (copIndex,_) ->
        // let s = sa |> SubjectNode
        // let p = sa |> PredicateNode
        // let b = sa |> BeRoot
        candidateTriples
        |> Array.choose( fun (startOption,edgeOption,stopOption) ->
            match startOption,edgeOption,stopOption with
            //Find all three components
            | Some(start),Some(edge),Some(stop) -> 
                //check the edge indices contain the copula index
                if edge |> Array.exists( fun (_,i) -> i = copIndex) then
                    {
                        start = start |> Array.map snd
                        edge = edge |> Array.map snd
                        stop = stop |> Array.map snd //TODO enforce nominal
                        trace = [ Trace( "copIndex:" + string(copIndex)) ]
                        negated = false; //TODO: fix
                    } |> Some
                else
                    None
            | _ -> None
        )
    )

/// Returns triples; accepts a serialized NLP object option, a chunksJson option, and an inputText 
let GetTriples (nlpJsonOption: string option) ( chunksJsonOption : string option) ( inputText : string ) =
    promise {
        //Get a DocumentAnnotation if one wasn't passed in
        let! nlpResult = 
            match nlpJsonOption with
            | Some(nlpJson) -> nlpJson |> ofJson<DocumentAnnotation> |> Promisify 
            | None -> GetNLP chunksJsonOption inputText

        match nlpResult with
        | Ok(da) ->
            //Make triples for every sentence 
            let (triples : Triple[][]) = da.sentences |> Array.map triplesFromSentence
      
            let apiResponse = {sentences = da.sentences; coreference = da.coreference; triples = triples}
            return Ok( apiResponse )
        | Error(e) -> 
            return Error(e)
    }