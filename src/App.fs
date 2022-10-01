module App

open Browser.Dom
open Fetch
open Thoth.Fetch
open Feliz

type 't Deferred = NotStarted | InProgress | Ready of 't

[<ReactComponent>]
let Counter () =
    let (count, setCount) = React.useState (0)

    Html.div [ Html.h1 count
               Html.button [ prop.text "Increment"
                             prop.onClick (fun _ -> setCount (count + 1)) ] ]



[<ReactComponent>]
let Message () =
    let (message, setMessage) = React.useState (NotStarted)

    Html.div [ Html.button [ prop.text "Get a message from the API"
                             prop.onClick
                                 (fun _ ->
                                     promise {
                                         setMessage InProgress
                                         let! message =
                                             Fetch.get (
                                                 "/api/GetMessage?name=FSharpie",
                                                 headers = [ HttpRequestHeaders.Accept "application/json" ]
                                             )

                                         setMessage (message |> Ready)
                                         return ()
                                     }
                                     |> ignore) ]
               match message with
                | NotStarted -> Html.none
                | InProgress -> Html.div "Executing..."
                | Ready (msg:string) -> Html.p msg
                   ]

[<ReactComponent>]
let App () = React.fragment [ Counter(); Message() ]

open Browser.Dom

ReactDOM.render (App(), document.getElementById "root")
