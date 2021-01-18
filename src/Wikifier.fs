module Wikifier

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch
open System.Text.RegularExpressions

//for node compatibility
importSideEffects "isomorphic-fetch"

let allOK (resultsArr : Result<'t,'e>[] ) = resultsArr |> Array.forall( fun r -> match r with | Ok(r) -> true | Error(e) -> false )
let resultsToType (resultsArr : Result<'t,'e>[] )  = resultsArr |> Array.choose( fun r -> match r with | Ok(r) -> Some(r) | Error(_) -> None ) 
let resultsToError (resultsArr : Result<'t,'e>[] )  = resultsArr |> Array.choose( fun r -> match r with | Ok(r) -> None | Error(e) -> Some(e) ) 


//Fable 2 transition 
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)

// type HappyError =
//     | Fetch of FetchError
//     | Other of string

type Candidate =
    {
        /// Wikifier confidence
        score : float
        /// Wikipedia title (at one time) for this entity
        wikiTitle : string
        /// Wikipedia id (fairly stable) for this entity
        wikiId : int
        /// category/topic type attributes associated with this entity, tab delimited
        attributes : string
    }
/// Wikipedia entity alignment
type Entity =
    {
        /// Surface realization of input text corresponding to this entity
        surfaceForm : string
        /// Character start position of surface realization in input text
        start : int
        /// Character stop position of surface realization in input text
        stop :int
        /// Candidate Wikipedia entities for this Entity
        candidates : Candidate[]
    }
/// A wikification based on the illinois-wikifier API
type Wikification =
    {
        /// Input text
        inputText : string
        /// Wikipedia entities found in the input text
        entities : Entity[]
    }

let endpoint = "http://127.0.0.1:8800/"
// let endpoint = "https://wikifier.olney.ai/"


type WikificationRequest = { text : string }

/// Get a wikification for the input text
let GetWikification(text: string): JS.Promise<Result<Wikification,FetchError>> =
    promise {
        return! Fetch.tryPost( endpoint + "wikify", { text=text;  } ) //caseStrategy = SnakeCase) //capitalization on the document fields seems to blow up the case strategy
    }

/// The target is A and the query is B, so we score the overlap with respect to A
let overlap startA stopA startB stopB =
    if startA < stopB && startB < stopA then
        let maxStart = if startA < startB then startB else startA
        let minStop = if stopA < stopB then stopA else stopB
        float(minStop - maxStart) / float(stopA-startA) 
        // float((stopB-startA) + (stopA-startB)) / float(stopA-startA) 
    else
        0.0

/// A match result for a query term with a Wikipedia entity in a text 
type EntityMatch = 
    {
        /// Percentage of many characters of the WP entity align with this term
        Coverage : float
        /// The Wikipeida entity the term aligns with
        Entity : Entity
    }

type WikiTermEntityMatch =
    {
        /// The term we are trying to match
        Term : string
        /// Character start position of the term
        Start : int
        /// Character stop position of the term
        Stop : int
        /// Array of Wikipedia entity match results
        EntityMatches : EntityMatch[]
    }

/// Get wikipedia entities that overlap with terms
/// We don't care about tracking precise correspondence
let GetWikiEntitiesForTerms( text : string) ( terms : string[]) = 
    promise {
        let! wikification = text |> GetWikification
        match wikification with
        | Ok(w) ->
            let wikiTermEntityMatches = [|
                for t in terms do
                    for m in Regex.Matches(text, @"\b" + t + @"\b", RegexOptions.IgnoreCase) |> Seq.cast<Match>  do
                        let startA = m.Index
                        let stopA = (m.Index + m.Length)
                        let entityMatches = [|
                            for e in w.entities do
                                let coverage = overlap startA stopA e.start e.stop
                                if coverage > 0.0 then
                                    yield { Coverage=coverage; Entity= e } 
                        |]
                        if entityMatches.Length > 0 then
                            yield { Term = t ; Start=startA; Stop = stopA; EntityMatches = entityMatches }
            |]
            return Ok(wikiTermEntityMatches)
        | Error(e) -> return Error(e)

    }

// Request for the test harness UI
type HarnessWikifyAlignRequest =
    { 
        Text : string
        Terms : string[]
    }
    static member InitializeTest() = {Text="Emotional stress can either increase or decrease TRH and TSH secretion, depending upon circumstances."; Terms = [| "TSH secretion"; "TRH"; "emotional stress" |] }
  
/// This function should only be called by the test harness GUI. It wraps GetWikiEntitiesForTerms to match the test harness API
let HarnessWikiAlign jsonRequest =
    let request = jsonRequest |> ofJson<HarnessWikifyAlignRequest>
    GetWikiEntitiesForTerms request.Text request.Terms

type FromTo = { from : string ; ``to`` : string  }
type Page = { pageid : int; ns : int; title : string; extract : string }
type WikipediaQuery = { normalized : FromTo[] option; redirects : FromTo[] option; pages : Map<string,Page> }
type WikipediaExtractResult = { batchcomplete : string ; query : WikipediaQuery }

/// Get a the first paragraph of a wikipedia page by id
let GetWikipediaPageFirstParagraph( pageId: int): JS.Promise<Result<WikipediaExtractResult,FetchError>> =
    promise {
        do! Promise.sleep 100 // be nice and throttle requests
        let wikipediaQuery = "https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&origin=*&pageids=" + pageId.ToString()
        return! Fetch.tryGet( wikipediaQuery ) //  , [Fetch.Types.RequestProperties.Mode Fetch.Types.RequestMode.Nocors])
    }

type WikiTermEntityExtracts =
    {
        WikiTermEntityMatches : WikiTermEntityMatch[]
        Pages : Page[]
    }

let GetWikiExtractsForTerms( text : string) ( terms : string[]) = 
    promise {
        let! wikiTermEntityMatches = GetWikiEntitiesForTerms text terms
        match wikiTermEntityMatches with
        | Ok( wtems ) -> 
            //for each entity match, get the extract for the top candidate
            let! extractResults = 
                wtems
                |> Seq.collect( fun wtem -> 
                    wtem.EntityMatches
                    |> Seq.map( fun em ->
                        //candidates are sorted so best match is first
                        let max = em.Entity.candidates |> Array.maxBy( fun c -> c.score) 
                        max.wikiId 
                    )
                    // Be nice to WP and avoid duplicates
                    |> Seq.distinct
                    |> Seq.map GetWikipediaPageFirstParagraph
                )
                |> Promise.all
            if allOK <| extractResults then
                let pages = 
                    extractResults 
                    |> resultsToType
                    |> Array.collect( fun wer -> wer.query.pages |> Map.toArray |> Array.map snd)
                    // Remove duplicates, i.e. if similar page queries returned overlapping sets of pages
                    |> Array.distinctBy( fun p -> p.pageid)
                    // |> Array.map( fun wer -> 
                    //    (Map.toList wer.query.pages) |> List.map snd |> List.head ) //NOTE: only taking first page!
                return Ok( { WikiTermEntityMatches = wtems; Pages = pages } )
            else 
                //We have a choice here to extend Fetch to report set of errors at once OR we can stay within Fetch and report the first such error
                //We choose the later on the suspicion that all errors in this batch will be of the same type
                // let errorPayload = (extractResults |> resultsToError |> Array.map (sprintf "wikipedia extract query error: %A") |> Array.distinct )
                // return Error(errorPayload |> String.concat "\n" |> HappyError.Other ) 
                return Error( extractResults |> resultsToError |> Array.head )
    
        | Error(e) -> return Error( e ) //HappyError.Fetch(e) )
    }

/// This function should only be called by the test harness GUI. It wraps GetWikiExtractsForTerms to match the test harness API
let HarnessWikiExtracts jsonRequest =
    let request = jsonRequest |> ofJson<HarnessWikifyAlignRequest>
    GetWikiExtractsForTerms request.Text request.Terms
