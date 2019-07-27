module App

open Fable.Core
open Fable.Core.JsInterop

open Browser
open Browser.Types
open Browser.Url

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Elmish.Debug
open Elmish.HMR
open Thoth.Json
// open Elmish.Cmd

open Fulma
open Fable.FontAwesome
open Fable.FontAwesome.Free

open Process
open System.IO
open Fable.Import
open System.Runtime.CompilerServices
open Fable.React.ReactiveComponents
open Fable.Import
open System.Text
open System

//Fable 2 transition
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)


// Todo: tests
let randomFeature() = [1;2;3]

// Domain
// ---------------------------------------

type Model = 
  {
    ///Input string to test the parser service
    InputText : string
    ///Result from the parser service
    ParseResult : string
    ///Simple function to test fable imports into meteor
    SimpleFun : string
    Status : string
  }

type Msg =
    | UpdateText of string
    | ProcessText
    | ParseResult of int * string
    // | ErrorResult of int * string
        
let init () : Model * Cmd<Msg> =
  ( { 
      InputText = ""
      ParseResult = ""
      SimpleFun = ""
      Status = ""
    }, [] )


// Update
// ---------------------------------------
let update msg (model:Model) =
  match msg with
  | UpdateText(input) ->
    ( {model with InputText=input}, [])
  | ProcessText ->
    let simpleFun = Process.DoSimpleComputation model.InputText
    //we use the status code from the server instead of a separate error handler `Cmd.OfPromise.either`
    ( 
      {model with SimpleFun=simpleFun; Status=""}, 
      //[]
      Cmd.OfPromise.perform Process.GetParse model.InputText ( fun result -> result |> ParseResult )
    )
  | ParseResult(code,json)->
    ( {model with ParseResult=json; Status=code.ToString()}, [])

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
      //editing 
      Fulma.Columns.columns [] [        
        Fulma.Column.column [ Column.Width  (Screen.All, Column.IsThreeFifths )  ] [
          str "Text"
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
        ]
        Fulma.Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [
          Button.button [ 
            Button.Color IsPrimary
            Button.OnClick (fun _ -> dispatch ProcessText )
            ] [ str "Get Results" ]
        ]
        Fulma.Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [
          //debuggy but also generally useful
          pre [  Style [FontSize 10.0 ] ] [ str (model |> toJson) ]
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
