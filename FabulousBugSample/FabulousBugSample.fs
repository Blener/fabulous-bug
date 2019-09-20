// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace FabulousBugSample

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

module App =
    type PageData =
        { Id: int; FirstValue: string; SecondValue: string }
        static member Empty =
            { Id = 0; FirstValue = ""; SecondValue = "" }
    
    let mockData =
        [ { Id = 1; FirstValue = "First Value 1"; SecondValue = "Second Value 1" }
          { Id = 2; FirstValue = "First Value 2"; SecondValue = "Second Value 2" }
          { Id = 3; FirstValue = "First Value 3"; SecondValue = "Second Value 3" }
          { Id = 4; FirstValue = "First Value 4"; SecondValue = "Second Value 4" }
          { Id = 5; FirstValue = "First Value 5"; SecondValue = "Second Value 5" } ]
    
    type Model = 
      { PageModel: PageData }

    type Msg = 
        | LoadPageData of int
        | PageDataLoaded of PageData
        | SetFirstValue of string
        | SetSecondValue of string
        
    type CmdMsg =
        | LoadPageDataCmd of int

    let mapCmdMsg cmd =
        match cmd with
        | LoadPageDataCmd id ->
            let data = mockData |> List.find (fun x -> x.Id = id)
            Cmd.ofMsg (PageDataLoaded data)
    
    let initModel = { PageModel = PageData.Empty }

    let init () = initModel, []

    let update msg model =
        match msg with
        | LoadPageData id ->
            { model with PageModel = PageData.Empty }, [ LoadPageDataCmd id ]
        | PageDataLoaded data ->
            { model with PageModel = data }, []
        | SetFirstValue value ->
            { model with PageModel = { model.PageModel with FirstValue = value } }, []
        | SetSecondValue value ->
            { model with PageModel = { model.PageModel with SecondValue = value } }, []

    let view (model: Model) dispatch =
        let attachChangeEvent condition changeEvent =
            if condition then debounce 250 changeEvent |> Some
            else None
        
        let attachChangeEvent = attachChangeEvent true 
        
        View.ContentPage(
          content = View.StackLayout(padding = 20.0, verticalOptions = LayoutOptions.Center,
            children = [
                View.Button("Load Data 1", fun () -> LoadPageData 1 |> dispatch)
                View.Button("Load Data 2", fun () -> LoadPageData 2 |> dispatch)
                View.Button("Load Data 3", fun () -> LoadPageData 3 |> dispatch)
                View.Button("Load Data 4", fun () -> LoadPageData 4 |> dispatch)
                View.Button("Load Data 5", fun () -> LoadPageData 5 |> dispatch)
                View.Entry(model.PageModel.FirstValue,
                           ?textChanged = attachChangeEvent(fun (args:TextChangedEventArgs) -> args.NewTextValue
                                                                                               |> SetFirstValue
                                                                                               |> dispatch))
                View.Entry(model.PageModel.SecondValue,
                           ?textChanged = attachChangeEvent(fun (args:TextChangedEventArgs) -> args.NewTextValue
                                                                                               |> SetSecondValue
                                                                                               |> dispatch))
            ]))

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgramWithCmdMsg init update view mapCmdMsg

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/tools.html for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/models.html for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


