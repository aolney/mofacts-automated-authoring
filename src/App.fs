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
  | InternalAPI
  | ExternalAPI

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
    ///Json loaded from file
    JsonInput : string option
  }

type Msg =
    | UpdateText of string
    | CallService
    | ServiceResult of int * string
    | ServiceChange of Service
    | DownloadJson
    | LoadParseFile of Browser.Types.FileList
    | SetParseJson of string
    | ClearParse
    // | ErrorResult of int * string
        
let init () : Model * Cmd<Msg> =
  ( { 
      InputText = "GitHub makes it easy to scale back on context switching."
      Service = InternalAPI
      Status = ""
      JsonResult = ""
      JsonInput = None
    }, [] )


// Update
// ---------------------------------------

let update msg (model:Model) =
  match msg with
  | UpdateText(input) ->
    ( {model with InputText=input}, [])
  | CallService ->
    //Debug site
    let test = [| "1"; "a" |] |> toJson

    //Test a service; we select here based on what's in the model
    let service = 
      match model.Service with
      | SRL -> Process.GetSRL
      | DependencyParser -> Process.GetDependencyParse
      | Coreference -> Process.GetCoreference
      | SentenceSplitter -> Process.GetSentences
      | CleanText -> Process.CleanText >> Process.Promisify
      | Acronym -> Process.GetAcronymMap >> Process.Promisify
      | Reverse -> Process.DoSimpleComputation >> Process.Promisify
      | NLP -> Process.GetNLP
      | InternalAPI -> Process.GetClozeInternal model.JsonInput
      | ExternalAPI -> Process.GetClozeAPI model.JsonInput

    //we use the status code from the server instead of a separate error handler `Cmd.OfPromise.either`
    ( 
      {model with Status=""}, 
      //[]
      Cmd.OfPromise.perform service model.InputText ( fun result -> result |> ServiceResult )
    )
  | ServiceResult(code,json)->
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
  | LoadParseFile(fileList) -> 
      let fileReadCommand dispatch =
        let fileReader = Browser.Dom.FileReader.Create ()
        fileReader.onload <- fun _ -> fileReader.result |> unbox<string> |> SetParseJson |> dispatch
        fileReader.readAsText fileList.[0]
      ( model, [fileReadCommand] )
  | SetParseJson(json) ->
    ( { model with JsonInput = Some(json)}, [])
  | ClearParse ->
    ( { model with JsonResult = ""; JsonInput = None}, [] )

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
        Fulma.Column.column [ Column.Width  (Screen.All, Column.IsThreeFifths )  ] [
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
          Fulma.File.file [ 
              Fulma.File.HasName 
              //Key allows us to reload a file after clearing it
              Fulma.File.Props [ Key ( if model.JsonInput.IsSome then "loaded" else "empty"); OnChange (fun ev ->  LoadParseFile !!ev.target?files  |> dispatch ) ] 
              ] [ 
              Fulma.File.label [ ] [ 
                Fulma.File.input [ Props [ Accept ".json" ]]
                Fulma.File.cta [ ] [ 
                  Fulma.File.icon [ ] [ 
                    Icon.icon [ ] [ 
                      Fa.i [ Fa.Solid.Upload ] []
                      ]
                  ]
                  Fulma.File.label [ ] [ str "Load Parse" ] ]
                Fulma.File.name [ ] [ str (match model.JsonInput with | Some(_) -> "Status: Loaded" | None -> "Status: Empty" ) ] 
                Button.button [ 
                  Button.Color IsPrimary
                  Button.OnClick (fun _ -> dispatch ClearParse )
                  ] [ str "Clear Parse" ]
              ] 
            ] 
        ]
        //Select and run service
        Fulma.Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [
          Field.div [ ]
                [ Label.label [ ]
                    [ str "Service" ]
                  Control.div [ ]
                     [ Select.select [  ]
                        [ select [ DefaultValue model.Service ; OnChange (fun ev  -> ServiceChange( !!ev.Value ) |> dispatch) ]
                            [ 
                              option [ Value Service.ExternalAPI ] [ str "External API" ]
                              option [ Value Service.InternalAPI ] [ str "Internal API" ]
                              option [ Value Service.NLP ] [ str "Composite NLP" ]
                              option [ Value Service.SRL ] [ str "SRL Parse" ]
                              option [ Value Service.DependencyParser ] [ str "Dependency Parse" ]
                              option [ Value Service.Coreference ] [ str "Coreference" ] 
                              option [ Value Service.SentenceSplitter ] [ str "Sentence Splitter" ] 
                              option [ Value Service.CleanText  ] [ str "Clean Text" ] 
                              option [ Value Service.Acronym  ] [ str "Acronym" ] 
                              option [ Value Service.Reverse  ] [ str "Reverse" ] 
                            ] ] ] ]
          Button.button [ 
            Button.Color IsPrimary
            Button.OnClick (fun _ -> dispatch CallService )
            ] [ str "Get Results" ]
        ]
        // View model state and get results
        Fulma.Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [
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
