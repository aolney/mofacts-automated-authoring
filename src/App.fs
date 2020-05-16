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
  | Triples
  // | LemmInflect
  | DefinitionalFeedback
  | InitializeDefinitionalFeedback
  | InitializeSpellingCorrector

type Model = 
  {
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
    ///Desired # sentences for external API
    DesiredSentences : string
    ///Desired # items for external API
    DesiredItems : string
    //To calculate word difficulty we need to return additional data from the GetSelectCloze API call
    // Trace : bool
  }

type Msg =
    | UpdateText of string
    | CallService
    | ServiceResult of string * string
    | ServiceChange of Service
    | DownloadJson
    | LoadJsonFile of Browser.Types.FileList
    | SetJson of string
    | ClearJson
    | UpdateSentences of string
    | UpdateItems of string
    // | ErrorResult of int * string
        
let init () : Model * Cmd<Msg> =
  ( { 
      InputText = "GitHub makes it easy to scale back on context switching."
      Service = SelectCloze
      Status = ""
      JsonResult = ""
      JsonInput = None
      DesiredSentences = ""
      DesiredItems = ""
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
  | Ok(r:'t) -> ServiceResult( "ok", Encode.Auto.toString(4, r ) )
  | Error(e) -> ServiceResult( "error", e.ToString() ) //could unpack various error types if desired https://thoth-org.github.io/Thoth.Fetch/

let update msg (model:Model) =
  match msg with
  | UpdateText(input) ->
    ( {model with InputText=input}, [])
  | CallService ->

    /// Assumes all services accept a single input; additional arguments must be curried before calling
    let makeCmd serviceCall input resultWrapper = 
      Cmd.OfPromise.perform serviceCall input ( fun result -> result |> resultWrapper ) 

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
      // Note we pass no chunks; input is used as a singleton chunk instead
      | Triples -> makeCmd (Triples.GetTriples model.JsonInput None) model.InputText makeServiceResult  
      | DefinitionalFeedback -> makeCmd (DefinitionalFeedback.HarnessGenerateFeedback) model.InputText makeServiceResult
      | InitializeDefinitionalFeedback -> makeCmd DefinitionalFeedback.Initialize model.JsonInput.Value makeServiceResult
      | InitializeSpellingCorrector -> makeCmd SpellingCorrector.Initialize model.JsonInput.Value makeServiceResult

    //we use the status code from the server instead of a separate error handler `Cmd.OfPromise.either`
    ( 
      {model with Status=""}, cmd
    )
  | ServiceResult(code,json)->
    //for debug: chrome is freezing up, so trying to dump non-essential fields from the model
    // ( {model with InputText=""; JsonInput=None; JsonResult=json; Status=code.ToString()}, [])
    ( {model with JsonResult=json; Status=code.ToString()}, [])
  | ServiceChange(service) ->
    ( {model with Service=service; Status=""}, [])
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
      ( model, [fileReadCommand] )
  | SetJson(json) ->
    ( { model with JsonInput = Some(json)}, [])
  | ClearJson ->
    ( { model with JsonResult = ""; JsonInput = None}, [] )
  | UpdateSentences(input)->
    ( { model with DesiredSentences=input}, [] )
  | UpdateItems(input)->
    ( { model with DesiredItems=input}, [] )

// View
// ---------------------------------------
let simpleButton txt action dispatch =
  div 
    [ ClassName "column is-narrow" ]
    [ a
        [ ClassName "button"
          OnClick (fun _ -> action |> dispatch) ]
    [ str txt ] ]

let view model dispatch =
  Section.section [] [
    //spinner defined in sass
    //div [ ClassName "loading"; Hidden ( model.Mode = Mode.Coding || model.Mode = Mode.TextEdit )  ] []
    Container.container [ Container.IsFluid ] [
      Heading.h2 [ ] [ str "MoFaCTS Automated Authoring"]
      Content.content [ ] [
        p [] [ str "This is a simple app for developing automated authoring components for MoFaCTS. Click on the cat in the corner for more information." ]
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
                Height "75px"
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
                  option [ Value Service.DefinitionalFeedback ] [ str "Definitional Feedback" ]
                  option [ Value Service.InitializeDefinitionalFeedback ] [ str "Initialize Definitional Feedback" ]
                  option [ Value Service.InitializeSpellingCorrector ] [ str "Initialize Spelling Corrector" ]
                  option [ Value Service.Triples ] [ str "Triples" ]
                  option [ Value Service.NLP ] [ str "Composite NLP" ]
                  option [ Value Service.SRL ] [ str "SRL Parse" ]
                  option [ Value Service.DependencyParser ] [ str "Dependency Parse" ]
                  option [ Value Service.Coreference ] [ str "Coreference" ] 
                  option [ Value Service.SentenceSplitter ] [ str "Sentence Splitter" ] 
                  option [ Value Service.CleanText  ] [ str "Clean Text" ] 
                  option [ Value Service.Acronym  ] [ str "Acronym" ] 
                  option [ Value Service.Reverse  ] [ str "Reverse" ] 
                ] 
              ]
            ]
          ]
          div [ Hidden ( 
                        model.Service <> Service.SelectCloze && 
                        model.Service <> Service.AllCloze && 
                        model.Service <> Service.DefinitionalFeedback &&
                        model.Service <> Service.InitializeDefinitionalFeedback &&
                        model.Service <> Service.InitializeSpellingCorrector)] [
            Label.label [ ] [ str "Optional JSON (e.g. parse)" ]
            Fulma.File.file [ 
                Fulma.File.HasName 
                //Key allows us to reload a file after clearing it
                Fulma.File.Props [ Key ( if model.JsonInput.IsSome then "loaded" else "empty"); OnChange (fun ev ->  LoadJsonFile !!ev.target?files  |> dispatch ) ] 
                ] [ 
                Fulma.File.label [ ] [ 
                  Fulma.File.input [ Props [ Accept ".json" ]]
                  Fulma.File.cta [ ] [ 
                    Fulma.File.icon [ ] [ 
                      Icon.icon [ ] [ 
                        Fa.i [ Fa.Solid.Upload ] []
                        ]
                    ]
                    Fulma.File.label [ ] [ str "Load JSON" ] ]
                  Fulma.File.name [ ] [ str (match model.JsonInput with | Some(_) -> "Status: Loaded" | None -> "Status: Empty" ) ] 
                  Button.button [ 
                    Button.Color IsPrimary
                    Button.OnClick (fun _ -> dispatch ClearJson )
                    ] [ str "Clear JSON" ]
                ] 
              ] 
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
          Button.button [ 
            Button.Color IsPrimary
            Button.OnClick (fun _ -> dispatch DownloadJson )
            ] [ str "Download JSON" ]
          //debuggy but also generally useful
          pre [  Style [FontSize 10.0 ] ] [  str <| (model |> toJson).Replace("\\n","\n").Replace("\\\"","\"") ]
          //span [  Style [FontSize 10.0; WhiteSpace "pre-wrap"] ] [  str <| (model |> toJson).Replace("\\n","\n").Replace("\\\"","\"") ]
        ]
      ]
    ]  
  ]

// App
Program.mkProgram init update view
#if DEBUG
|> Program.withDebugger
|> Program.withConsoleTrace
//|> Program.withHMR //deprecated???
#endif
|> Program.withReactSynchronous "elmish-app" //batched makes input cursor jump to end of box
|> Program.run
