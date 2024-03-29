module App

open Fable.Core
open Fable.Core.JsInterop

open Browser

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Elmish.Debug
open Elmish.HMR
open Thoth.Json

open Fulma
open Fable.FontAwesome

//Fable 2 transition
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)

//Alert for long-running process
[<Emit("(new Audio('data:audio/wav;base64,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qJrx4KOQjahZxWbcZgztj2c49nKmkId44S71j0c8eV9yDK6uPRzx5X18eDvjvQ6yKo9ZSS6l//8elePK/Lf//IInrOF/FvDoADYAGBMGb7FtErm5MXMlmPAJQVgWta7Zx2go+8xJ0UiCb8LHHdftWyLJE0QIAIsI+UbXu67dZMjmgDGCGl1H+vpF4NSDckSIkk7Vd+sxEhBQMRU8j/12UIRhzSaUdQ+rQU5kGeFxm+hb1oh6pWWmv3uvmReDl0UnvtapVaIzo1jZbf/pD6ElLqSX+rUmOQNpJFa/r+sa4e/pBlAABoAAAAA3CUgShLdGIxsY7AUABPRrgCABdDuQ5GC7DqPQCgbbJUAoRSUj+NIEig0YfyWUho1VBBBA//uQZB4ABZx5zfMakeAAAAmwAAAAF5F3P0w9GtAAACfAAAAAwLhMDmAYWMgVEG1U0FIGCBgXBXAtfMH10000EEEEEECUBYln03TTTdNBDZopopYvrTTdNa325mImNg3TTPV9q3pmY0xoO6bv3r00y+IDGid/9aaaZTGMuj9mpu9Mpio1dXrr5HERTZSmqU36A3CumzN/9Robv/Xx4v9ijkSRSNLQhAWumap82WRSBUqXStV/YcS+XVLnSS+WLDroqArFkMEsAS+eWmrUzrO0oEmE40RlMZ5+ODIkAyKAGUwZ3mVKmcamcJnMW26MRPgUw6j+LkhyHGVGYjSUUKNpuJUQoOIAyDvEyG8S5yfK6dhZc0Tx1KI/gviKL6qvvFs1+bWtaz58uUNnryq6kt5RzOCkPWlVqVX2a/EEBUdU1KrXLf40GoiiFXK///qpoiDXrOgqDR38JB0bw7SoL+ZB9o1RCkQjQ2CBYZKd/+VJxZRRZlqSkKiws0WFxUyCwsKiMy7hUVFhIaCrNQsKkTIsLivwKKigsj8XYlwt/WKi2N4d//uQRCSAAjURNIHpMZBGYiaQPSYyAAABLAAAAAAAACWAAAAApUF/Mg+0aohSIRobBAsMlO//Kk4soosy1JSFRYWaLC4qZBYWFRGZdwqKiwkNBVmoWFSJkWFxX4FFRQWR+LsS4W/rFRb/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////VEFHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAU291bmRib3kuZGUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMjAwNGh0dHA6Ly93d3cuc291bmRib3kuZGUAAAAAAAAAACU=')).play();")>]
let beep () = jsNative

// Todo: tests
let randomFeature() = [1;2;3]

// Domain
// ---------------------------------------

[<StringEnum>]
type Service = 
  | SRL
  | Coreference
  | DependencyParser
  | SentenceSplitter
  | CleanText
  | Acronym
  | Reverse
  ///Composite service
  | NLP 
  | AllCloze
  | SelectCloze
  | SelectClozePercentage
  | Triples
  | Lemma //assumes UPOS noun
  | Inflection //assumes Penn NNS
  | ElaboratedFeedback
  | CachedElaboratedFeedback
  | DefinitionalFeedback
  | Paraphrase
  | ResolveTextReferents
  | GenerateQuestions
  | AnswerQuestion
  | Wikify
  | WikiAlign
  | WikiExtracts
  | InitializeCachedElaboratedFeedback
  | InitializeDefinitionalFeedback
  | InitializeSpellingCorrector
  | InitializeParaphraseCache
  | TutorialDialogue
  | ElaboratedTutorialDialogueState
  | Test

[<StringEnum>]
type UIMode = Simple | Expert

type Model = 
  {
    /// UI Mode: simple (get text, generate cloze items in json form) or expert (interface used for testing)
    Mode : UIMode
    ///Input string to test the service
    InputText : string
    ///Type of service we will call
    Service : Service
    ///Status of service call; typically standard HTTP code
    Status : string
    ///Result from the service called
    JsonResult : string
    ///Json loaded from file, e.g. a parse
    JsonInput : string option
    ///Json file name
    JsonFileName : string option
    ///Desired # sentences for external API
    DesiredSentences : string
    ///Desired # items for external API
    DesiredItems : string
    //To calculate word difficulty we need to return additional data from the GetSelectCloze API call
    // Trace : bool
    Percentage : string
    //for simple mode only
    ParseJson : string option
    ParseFileName : string option
    // Paraphrases : string[] option
    ParaphraseFileName : string option
  }

type Msg =
    | UpdateText of string
    | CallService
    | ServiceResult of string * string
    | ServiceChange of Service
    | DownloadJson
    | JsonToInput
    | LoadJsonFile of Browser.Types.FileList
    | SetJson of string
    | ClearJson
    | UpdateSentences of string
    | UpdateItems of string
    | UpdatePercentage of string
    | ExpertMode
    //for simple mode only
    | LoadJsonParseFile of Browser.Types.FileList
    | SetParseJson of string
    | ClearParseJson
    | LoadParaphraseFile of Browser.Types.FileList
    | SetParaphrases of string
    // | ClearParaphrases
    // | ErrorResult of int * string
        
let init () : Model * Cmd<Msg> =
  ( { 
      Mode = UIMode.Simple
      InputText = "Paste text here or leave blank and upload respective JSON files."
      Service = SelectClozePercentage
      Status = ""
      JsonResult = ""
      JsonInput = None
      JsonFileName = None
      DesiredSentences = ""
      DesiredItems = ""
      Percentage = "0.05"
      //for simple mode only
      ParseJson = None
      ParseFileName = None
      // Paraphrases = None
      ParaphraseFileName = None
    }, [] )


// Update
// ---------------------------------------
let ParseIntOption s =
  match System.Int32.TryParse(s) with
  | (true,int) -> Some(int)
  | _ -> None

/// Convert a Thoth.Fetch Result into a wrapped string tuple: (status,payload)
let inline makeServiceResult ( result : Result<'t,'e> ) =
  match result with
  | Ok(r:'t) -> ServiceResult(  "Execution completed!", Encode.Auto.toString(4, r ) )
  | Error(e) -> ServiceResult( "Error! ", e.ToString() ) //could unpack various error types if desired https://thoth-org.github.io/Thoth.Fetch/

/// Assumes all services accept a single input; additional arguments must be curried before calling
let makeCmd serviceCall input resultWrapper = 
  // let statusMessage = 
  //   if serviceCall <> Paraphrase.InitializeParaphraseCache then
  //     "Execution completed!"
  //   else
  //     ""
  Cmd.OfPromise.perform serviceCall input ( fun result -> result |> resultWrapper) 

let update msg (model:Model) =
  match msg with
  | UpdateText(input) ->
    ( {model with InputText=input}, [])
  | CallService ->

    /// Because different services emit different types, but all must resolve to the same type here, uses functions above to simplify construction
    let cmd =
      match model.Service with
      | SRL -> makeCmd AllenNLP.GetSRL model.InputText makeServiceResult
      | DependencyParser -> makeCmd AllenNLP.GetDependencyParse model.InputText makeServiceResult
      | Coreference -> makeCmd AllenNLP.GetCoreference model.InputText makeServiceResult
      | SentenceSplitter -> makeCmd AllenNLP.GetSentences model.InputText makeServiceResult
      | CleanText -> makeCmd (AllenNLP.CleanText >> AllenNLP.Promisify) model.InputText makeServiceResult
      | NLP -> makeCmd (AllenNLP.GetNLP model.JsonInput) model.InputText makeServiceResult
      | Acronym -> makeCmd (ClozeAPI.GetAcronymMap >> AllenNLP.Promisify) model.InputText makeServiceResult
      | Reverse -> makeCmd (ClozeAPI.DoSimpleComputation >> AllenNLP.Promisify) model.InputText makeServiceResult
      // Note we pass no chunks; input is used as a singleton chunk instead
      | AllCloze -> makeCmd (ClozeAPI.GetAllCloze model.JsonInput None) model.InputText makeServiceResult
      // Note we pass no chunks; input is used as a singleton chunk instead; "true" provides trace information
      | SelectCloze -> makeCmd (ClozeAPI.GetSelectCloze model.JsonInput (ParseIntOption <| model.DesiredSentences) (ParseIntOption <| model.DesiredItems) true None ) model.InputText makeServiceResult
      | SelectClozePercentage -> makeCmd (ClozeAPI.GetSelectClozePercentage ( model.Percentage |> float) model.JsonInput model.ParseJson) model.InputText makeServiceResult
      // Note we pass no chunks; input is used as a singleton chunk instead
      | Triples -> makeCmd (Triples.GetTriples model.JsonInput None) model.InputText makeServiceResult  
      | DefinitionalFeedback -> makeCmd (DefinitionalFeedback.HarnessGenerateFeedback) model.InputText makeServiceResult
      | ElaboratedFeedback -> makeCmd (ElaboratedFeedback.HarnessGetElaboratedFeedback) model.InputText makeServiceResult
      | CachedElaboratedFeedback -> makeCmd (CachedElaboratedFeedback.HarnessGenerateFeedback) model.InputText makeServiceResult
      | Lemma -> makeCmd LemmInflect.testGetLemma model.InputText makeServiceResult
      | Inflection -> makeCmd LemmInflect.testGetInflection model.InputText makeServiceResult
      | Paraphrase -> makeCmd Paraphrase.getParaphrases model.InputText makeServiceResult
      | ResolveTextReferents -> makeCmd AllenNLP.ResolveTextReferents model.InputText makeServiceResult
      | GenerateQuestions -> makeCmd QuestionGenerator.HarnessGetQuestions model.InputText makeServiceResult
      | AnswerQuestion -> makeCmd LongformQA.testAnswer model.InputText makeServiceResult
      | Wikify -> makeCmd Wikifier.GetWikification model.InputText makeServiceResult
      | WikiAlign -> makeCmd Wikifier.HarnessWikiAlign model.InputText makeServiceResult
      | WikiExtracts -> makeCmd Wikifier.HarnessWikiExtracts model.InputText makeServiceResult
      | InitializeCachedElaboratedFeedback -> makeCmd CachedElaboratedFeedback.Initialize model.JsonInput.Value makeServiceResult
      | InitializeDefinitionalFeedback -> makeCmd DefinitionalFeedback.Initialize model.JsonInput.Value makeServiceResult
      | InitializeSpellingCorrector -> makeCmd SpellingCorrector.Initialize model.JsonInput.Value makeServiceResult
      | InitializeParaphraseCache -> makeCmd Paraphrase.InitializeParaphraseCache model.JsonInput.Value makeServiceResult
      | TutorialDialogue -> makeCmd (TutorialDialogue.HarnessGetDialogue) model.InputText makeServiceResult
      | ElaboratedTutorialDialogueState -> makeCmd (TutorialDialogue.HarnessGetElaboratedDialogueState) model.InputText makeServiceResult
      // | TutorialDialogue -> makeCmd TutorialDialogue.GetDialogue (model.InputText |> ofJson<TutorialDialogue.DialogueState> ) makeServiceResult
      | Test -> makeCmd (AllenNLP.resolveReferents >> AllenNLP.Promisify) (model.JsonInput.Value |> ofJson<AllenNLP.DocumentAnnotation> ) makeServiceResult

    //we use the status code from the server instead of a separate error handler `Cmd.OfPromise.either`
    ( 
      {model with Status="Executing, please wait..."}, cmd
    )
  | ServiceResult(code,json)->

    //alert beep
    if json <> "null" then
      beep();beep();beep() 

    //for debug: chrome is freezing up, so trying to dump non-essential fields from the model
    // ( {model with InputText=""; JsonInput=None; JsonResult=json; Status=code.ToString()}, [])
    ( {model with JsonResult=json; Status=code.ToString()}, [])
  | ServiceChange(service) ->
    //Some services require json in the Input box
    let inputText = 
      match service with
      // For service that requires more than a string as input, we define a json object for the parameters and 
      // prepopulate the input box with exemplar json for the user to modify
      | TutorialDialogue -> TutorialDialogue.DialogueState.InitializeTest() |> toJson
      | GenerateQuestions -> QuestionGenerator.InitializeTest()
      | ElaboratedTutorialDialogueState -> TutorialDialogue.HarnessElaboratedDialogueState.InitializeTest() |> toJson
      | ElaboratedFeedback -> ElaboratedFeedback.HarnessElaboratedFeedbackRequest.InitializeTest() |> toJson
      | CachedElaboratedFeedback -> CachedElaboratedFeedback.HarnessFeedbackRequest.InitializeTest() |> toJson
      | DefinitionalFeedback -> DefinitionalFeedback.HarnessFeedbackRequest.InitializeTest() |> toJson
      | WikiAlign | WikiExtracts -> Wikifier.HarnessWikifyAlignRequest.InitializeTest() |> toJson
      | _ -> ""
    ( {model with Service=service; Status=""; InputText = inputText}, [])
  | JsonToInput ->
    ( {model with InputText=model.JsonResult; JsonResult=""}, [])
  | DownloadJson ->
      let a = document.createElement("a") :?> Browser.Types.HTMLLinkElement
      //need blobs for larger sizes
      let blob = Blob.Create( [| model.JsonResult |], jsOptions<Types.BlobPropertyBag>( fun o -> o.``type`` <- "data:text/plain;charset=utf-8") )
      a.href <- URL.createObjectURL( blob );
      //a.href <- "data:text/plain;charset=utf-8," + JS.encodeURIComponent( model.JsonResult )
      let filename = System.DateTime.Now.ToString("MM-dd-yy-HH-mm", System.Globalization.CultureInfo.InvariantCulture) + ".json"
      a.setAttribute("download", filename );
      a.click()
      ( model,[] )
  | LoadJsonFile(fileList) -> 
      let fileReadCommand dispatch =
        let fileReader = Browser.Dom.FileReader.Create ()
        fileReader.onload <- fun _ -> fileReader.result |> unbox<string> |> SetJson |> dispatch
        fileReader.readAsText fileList.[0]
      ( {model with JsonFileName = Some(fileList.[0].name)}, [fileReadCommand] )
  | SetJson(json) ->
    ( { model with JsonInput = Some(json)}, [])
  | ClearJson ->
    ( { model with JsonResult = ""; JsonInput = None}, [] )
  | UpdateSentences(input)->
    ( { model with DesiredSentences=input}, [] )
  | UpdateItems(input)->
    ( { model with DesiredItems=input}, [] )
  | UpdatePercentage(input)->
    ( { model with Percentage=input}, [] )
  | ExpertMode ->
    ( { model with Mode=UIMode.Expert}, [] )
  //for simple mode only
  | LoadJsonParseFile(fileList) -> 
    let fileReadCommand dispatch =
      let fileReader = Browser.Dom.FileReader.Create ()
      fileReader.onload <- fun _ -> fileReader.result |> unbox<string> |> SetParseJson |> dispatch
      fileReader.readAsText fileList.[0]
    ( {model with ParseFileName = Some(fileList.[0].name)}, [fileReadCommand] )
  | SetParseJson(json) ->
    ( { model with ParseJson = Some(json)}, [])
  | ClearParseJson ->
    ( { model with ParseJson = None}, [] )
  | LoadParaphraseFile(fileList) -> 
    let fileReadCommand dispatch =
      let fileReader = Browser.Dom.FileReader.Create ()
      fileReader.onload <- fun _ -> fileReader.result |> unbox<string> |> SetParaphrases |> dispatch
      fileReader.readAsText fileList.[0]
    ( {model with ParaphraseFileName = Some(fileList.[0].name)}, [fileReadCommand] )
  | SetParaphrases(text) ->
    let loadParaphrases = makeCmd Paraphrase.InitializeParaphraseCache text makeServiceResult
    ( model, loadParaphrases)


// View
// ---------------------------------------
let simpleButton txt action dispatch =
  div 
    [ ClassName "column is-narrow" ]
    [ a
        [ ClassName "button"
          OnClick (fun _ -> action |> dispatch) ]
    [ str txt ] ]

let simpleModeView model dispatch = 
  Section.section [] [
    Container.container [ Container.IsFluid ] [
      Heading.h2 [ ] [ str "MoFaCTS Automated Authoring"]
      Content.content [ ] [
        p [] [ str "A simple app for creating MoFaCTS cloze items from text. Click on the cat in the corner for more information." ]
      ]
      //editing and uploading data
      Fulma.Columns.columns [] [        
        Fulma.Column.column [ Column.Width  (Screen.All, Column.IsOneThird )  ] [
          Label.label [ ] [ str "Input" ]
          textarea [
            ClassName "input"
            Value model.InputText
            Size 100.0
            Style [
                Width "100%"
                Height "150px"
            ] 
            OnChange (fun ev ->  !!ev.target?value |> UpdateText|> dispatch )
          ] []
          // The only service in simple mode is Get Select Cloze
                // select [ DefaultValue model.Service ; OnChange (fun ev  -> ServiceChange( !!ev.Value ) |> dispatch) ] [ 
                //   option [ Value Service.SelectCloze ] [ str "Get Select Cloze" ]
                
          
          div [ ClassName "block" ] [
            Label.label [ ] [ str "Proportion Sentences to Use [0,1]" ] 
            Input.text [
                  Input.Color IsPrimary
                  Input.IsRounded
                  Input.Value ( model.Percentage.ToString() )
                  Input.Props [ OnChange (fun ev ->  !!ev.target?value |> UpdatePercentage|> dispatch ) ]
                ]
            // Label.label [ ] [ str "Optional Desired Sentences" ] 
            // Input.text [
            //       Input.Color IsPrimary
            //       Input.IsRounded
            //       Input.Value ( model.DesiredSentences.ToString() )
            //       Input.Props [ OnChange (fun ev ->  !!ev.target?value |> UpdateSentences|> dispatch ) ]
            //     ]
            // Label.label [ ] [ str "Optional Desired Items" ] 
            // Input.text [
            //       Input.Color IsPrimary
            //       Input.IsRounded
            //       Input.Value ( model.DesiredItems.ToString() )
            //       Input.Props [ OnChange (fun ev ->  !!ev.target?value |> UpdateItems|> dispatch ) ]
            //     ]
            ]
            
          // JSON list of sections
          div [ ClassName "block" ] [
            Label.label [ ] [ str "Optional JSON list of sections (list of strings)" ]
            Fulma.File.file [ 
              Fulma.File.HasName 
              Fulma.File.Props [ Key ( if model.JsonInput.IsSome then "loaded" else "empty"); OnChange (fun ev ->  LoadJsonFile !!ev.target?files  |> dispatch ) ] 
              ] [ 
              Fulma.File.Label.label [ ] [ 
                Fulma.File.input [ Props [ Accept ".json" ]]
                Fulma.File.cta [ ] [ 
                  Fulma.File.icon [ ] [ 
                    Fulma.Icon.icon [ ] [ 
                      Fa.i [ Fa.Solid.Upload ] [ ] 
                    ]
                  ]
                  Fulma.File.Label.span [ ] [ 
                    str "Choose a file..." 
                  ] 
                ]
                Fulma.File.name [ ] [ 
                  str (match model.JsonFileName with | Some(name) -> name | None -> "" )
                ]
              ] 
            ]
          ]
        
          // Parse
          div [ ClassName "block" ] [
              Label.label [ ] [ str "Optional parse JSON" ]
              Fulma.File.file [ 
                Fulma.File.HasName 
                Fulma.File.Props [ Key ( if model.ParseJson.IsSome then "loaded" else "empty"); OnChange (fun ev ->  LoadJsonParseFile !!ev.target?files  |> dispatch ) ] 
                ] [ 
                Fulma.File.Label.label [ ] [ 
                  Fulma.File.input [ Props [ Accept ".json" ]]
                  Fulma.File.cta [ ] [ 
                    Fulma.File.icon [ ] [ 
                      Fulma.Icon.icon [ ] [ 
                        Fa.i [ Fa.Solid.Upload ] [ ] 
                      ]
                    ]
                    Fulma.File.Label.span [ ] [ 
                      str "Choose a file..." 
                    ] 
                  ]
                  Fulma.File.name [ ] [ 
                    str (match model.ParseFileName with | Some(name) -> name | None -> "" )
                  ]
                ] 
              ]
            ]

          //Paraphrase
          div [ ClassName "block" ] [
            Label.label [ ] [ str "Optional paraphrases (tsv)" ]
            Fulma.File.file [ 
              Fulma.File.HasName 
              Fulma.File.Props [ Key ( if model.ParaphraseFileName.IsSome then "loaded" else "empty"); OnChange (fun ev ->  LoadParaphraseFile !!ev.target?files  |> dispatch ) ] 
              ] [ 
              Fulma.File.Label.label [ ] [ 
                Fulma.File.input [ Props [ Accept ".tsv" ]]
                Fulma.File.cta [ ] [ 
                  Fulma.File.icon [ ] [ 
                    Fulma.Icon.icon [ ] [ 
                      Fa.i [ Fa.Solid.Upload ] [ ] 
                    ]
                  ]
                  Fulma.File.Label.span [ ] [ 
                    str "Choose a file..." 
                  ] 
                ]
                Fulma.File.name [ ] [ 
                  str (match model.ParaphraseFileName with | Some(name) -> name | None -> "" )
                ]
              ] 
            ]
          ]

        ] //column
        Fulma.Column.column [ Column.Width (Screen.All, Column.IsOneThird) ] [
          Label.label [ ] [ str "Run" ]
          div [ ClassName "block" ] [
          Button.button [ 
            Button.Color IsPrimary
            Button.OnClick (fun _ -> dispatch CallService )
            ] [ str "Generate items" ]
          ]
          Text.p [ Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ] [
            str ( if model.JsonResult <> "null" then model.Status else "" )
          ]
          if model.Status = "Execution completed!"  && model.JsonResult <> "null" then
            div [ ClassName "block" ] [
            Button.button [ 
              Button.Color IsPrimary
              Button.OnClick (fun _ -> dispatch DownloadJson )
              ] [ str "Download JSON" ]
            ]
        ]

        Fulma.Column.column [ 
          Column.Modifiers [  
            // Fulma.Modifier.FlexDirection FlexDirection.Column; 
            Fulma.Modifier.FlexJustifyContent FlexJustifyContent.SpaceBetween 
            ];  
          Column.Width (Screen.All, Column.IsOneThird) ] [    
          Button.button [ 
            Button.Modifiers [ Fulma.Modifier.Spacing (Spacing.MarginTop, Spacing.IsAuto ) ]
            Button.Color IsDanger
            Button.OnClick (fun _ -> dispatch ExpertMode )
            ] [ str "Expert mode" ]
        ]
      ]
    ]
  ]

let expertModeView model dispatch = 
  Section.section [] [
    //spinner defined in sass
    //div [ ClassName "loading"; Hidden ( model.Mode = Mode.Coding || model.Mode = Mode.TextEdit )  ] []
    Container.container [ Container.IsFluid ] [
      Heading.h2 [ ] [ str "MoFaCTS Automated Authoring"]
      Content.content [ ] [
        p [] [ str "Expert mode is an interactive test suite for developing automated authoring components for MoFaCTS. It is not intended for general use. Click on the cat in the corner for more information." ]
      ]
      //editing and uploading data
      Fulma.Columns.columns [] [        
        Fulma.Column.column [ Column.Width  (Screen.All, Column.IsOneThird )  ] [
          Label.label [ ] [ str "Input" ]
          textarea [
            ClassName "input"
            Value model.InputText
            Size 100.0
            Style [
                Width "100%"
                Height "150px"
            ] 
            OnChange (fun ev ->  !!ev.target?value |> UpdateText|> dispatch )
          ] []

          Field.div [] [
            Label.label [ ] [ str "Service" ]
            Control.div [ ] [ 
              Select.select [  ] [ 
                select [ DefaultValue model.Service ; OnChange (fun ev  -> ServiceChange( !!ev.Value ) |> dispatch) ] [ 
                  option [ Value Service.SelectCloze ] [ str "Get Select Cloze" ]
                  option [ Value Service.AllCloze ] [ str "Get All Cloze" ]
                  option [ Value Service.TutorialDialogue ] [ str "Tutorial Dialogue" ]
                  option [ Value Service.ElaboratedTutorialDialogueState ] [ str "Initialize Elaborated Tutorial Dialogue State" ]
                  option [ Value Service.ElaboratedFeedback ] [ str "Elaborated Feedback" ]
                  option [ Value Service.CachedElaboratedFeedback ] [ str "Cached Elaborated Feedback" ]
                  option [ Value Service.DefinitionalFeedback ] [ str "Definitional Feedback" ]
                  option [ Value Service.InitializeCachedElaboratedFeedback ] [ str "Initialize Cached Elaborated Feedback" ]
                  option [ Value Service.InitializeDefinitionalFeedback ] [ str "Initialize Definitional Feedback" ]
                  option [ Value Service.InitializeSpellingCorrector ] [ str "Initialize Spelling Corrector" ]
                  option [ Value Service.InitializeParaphraseCache ] [ str "Initialize Paraphrase" ]
                  option [ Value Service.Triples ] [ str "Triples" ]
                  option [ Value Service.NLP ] [ str "Composite NLP" ]
                  option [ Value Service.Lemma ] [ str "Lemma (assumes noun)" ]
                  option [ Value Service.Inflection ] [ str "Inflection (assumes NNS)" ]
                  option [ Value Service.Paraphrase ] [ str "Paraphrase" ]
                  option [ Value Service.ResolveTextReferents ] [ str "Resolve Coreference" ]
                  option [ Value Service.AnswerQuestion ] [ str "Answer Question" ]
                  option [ Value Service.GenerateQuestions ] [ str "Generate Questions" ]
                  option [ Value Service.Wikify ] [ str "Wikify" ]
                  option [ Value Service.WikiAlign ] [ str "Wikify Align" ]
                  option [ Value Service.WikiExtracts ] [ str "Wiki Extracts" ]
                  option [ Value Service.SRL ] [ str "SRL Parse" ]
                  option [ Value Service.DependencyParser ] [ str "Dependency Parse" ]
                  option [ Value Service.Coreference ] [ str "Coreference Annotation" ] 
                  option [ Value Service.SentenceSplitter ] [ str "Sentence Splitter" ] 
                  option [ Value Service.CleanText  ] [ str "Clean Text" ] 
                  option [ Value Service.Acronym  ] [ str "Acronym" ] 
                  option [ Value Service.Reverse  ] [ str "Reverse" ] 
                  option [ Value Service.Test  ] [ str "Test" ] 
                ] 
              ]
            ]
          ]
          div [ Hidden ( 
                        model.Service <> Service.NLP && 
                        model.Service <> Service.SelectCloze && 
                        model.Service <> Service.AllCloze && 
                        model.Service <> Service.ElaboratedFeedback &&
                        model.Service <> Service.DefinitionalFeedback &&
                        model.Service <> Service.InitializeCachedElaboratedFeedback &&
                        model.Service <> Service.InitializeDefinitionalFeedback &&
                        model.Service <> Service.InitializeSpellingCorrector &&
                        model.Service <> Service.InitializeParaphraseCache &&
                        model.Service <> Service.Test
                        )] [
            // Label.label [ ] [ str "Optional JSON (e.g. parse)" ]
            // Fulma.File.file [ 
            //     Fulma.File.HasName 
            //     //Key allows us to reload a file after clearing it
            //     Fulma.File.Props [ Key ( if model.JsonInput.IsSome then "loaded" else "empty"); OnChange (fun ev ->  LoadJsonFile !!ev.target?files  |> dispatch ) ] 
            //     ] [ 
            //     Fulma.File.name [ ] [ 
            //       Fulma.File.input [ Props [ Accept ".json" ]]
            //       Fulma.File.cta [ ] [ 
            //         Fulma.File.icon [ ] [ 
            //           Icon.icon [ ] [ 
            //             Fa.i [ Fa.Solid.Upload ] []
            //             ]
            //         ]
            //         Fulma.File.name [ ] [ str "Load JSON" ] ]
            //       Fulma.File.name [ ] [ str (match model.JsonInput with | Some(_) -> "Status: Loaded" | None -> "Status: Empty" ) ] 
            //       Button.button [ 
            //         Button.Color IsPrimary
            //         Button.OnClick (fun _ -> dispatch ClearJson )
            //         ] [ str "Clear JSON" ]
            //     ] 
            //   ]
            //
            div [ ClassName "block" ] [
              Label.label [ ] [ str "Optional JSON (e.g. parse)" ]
              Fulma.File.file [ 
                Fulma.File.HasName 
                Fulma.File.Props [ Key ( if model.JsonInput.IsSome then "loaded" else "empty"); OnChange (fun ev ->  LoadJsonFile !!ev.target?files  |> dispatch ) ] 
                ] [ 
                Fulma.File.Label.label [ ] [ 
                  Fulma.File.input [ Props [ Accept ".json,.tsv" ]]
                  Fulma.File.cta [ ] [ 
                    Fulma.File.icon [ ] [ 
                      Fulma.Icon.icon [ ] [ 
                        Fa.i [ Fa.Solid.Upload ] [ ] 
                      ]
                    ]
                    Fulma.File.Label.span [ ] [ 
                      str "Choose a file..." 
                    ] 
                  ]
                  Fulma.File.name [ ] [ 
                    str (match model.JsonFileName with | Some(name) -> name | None -> "" )
                  ]
                ] 
              ]
            ]
              // 
          ]
          div [ Hidden ( model.Service <> Service.SelectCloze ) ] [
            Label.label [ ] [ str "Optional Desired Sentences" ] 
            Input.text [
                  Input.Color IsPrimary
                  Input.IsRounded
                  Input.Value ( model.DesiredSentences.ToString() )
                  Input.Props [ OnChange (fun ev ->  !!ev.target?value |> UpdateSentences|> dispatch ) ]
                ]
            Label.label [ ] [ str "Optional Desired Items" ] 
            Input.text [
                  Input.Color IsPrimary
                  Input.IsRounded
                  Input.Value ( model.DesiredItems.ToString() )
                  Input.Props [ OnChange (fun ev ->  !!ev.target?value |> UpdateItems|> dispatch ) ]
                ]
            ]

          Button.button [ 
          Button.Color IsPrimary
          Button.OnClick (fun _ -> dispatch CallService )
          ] [ str "Call Service" ]

        ]
        //Select and run service
        // Fulma.Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [

        // ]
        // View model state and get results
        Fulma.Column.column [ Column.Width (Screen.All, Column.IsTwoThirds) ] [
          Label.label [ ] [ str "Model State" ]
          Fulma.Columns.columns [] [     
            Fulma.Column.column [ ] [
              Button.button [ 
                Button.Color IsPrimary
                Button.OnClick (fun _ -> dispatch DownloadJson )
                ] [ str "Download JSON" ]
            ]
            Fulma.Column.column [ ] [
              Button.button [ 
                Button.Color IsPrimary
                Button.OnClick (fun _ -> dispatch JsonToInput )
                ] [ str "JSON to Input" ]
            ]
            // purely to bring buttons closer together
            Fulma.Column.column [ Column.Width (Screen.All, Column.IsTwoThirds) ] []
          ]
          //debuggy but also generally useful
          pre [  Style [FontSize 10.0 ] ] [  str <| (model |> toJson).Replace("\\n","\n").Replace("\\\"","\"") ]
          //span [  Style [FontSize 10.0; WhiteSpace "pre-wrap"] ] [  str <| (model |> toJson).Replace("\\n","\n").Replace("\\\"","\"") ]
        ]
      ]
    ]  
  ]

let view model dispatch =
  match model.Mode with
  | UIMode.Simple -> simpleModeView model dispatch
  | UIMode.Expert -> expertModeView model dispatch

// App
Program.mkProgram init update view
#if DEBUG
|> Program.withDebugger
|> Program.withConsoleTrace
//|> Program.withHMR //deprecated???
#endif
|> Program.withReactSynchronous "elmish-app" //batched makes input cursor jump to end of box
|> Program.run
