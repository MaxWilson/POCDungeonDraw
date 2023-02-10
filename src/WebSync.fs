module WebSync

open Fable.Core
open Browser.Types
open Fable.Core.JsInterop
open Thoth.Json

type PubSubMessage = {
    ``type``: string
    data: string option
    }
type PubSubPayload = {
    ``type``: string
    group: string
    noEcho: bool
    dataType: string
    data: string
    }
let mutable socket: WebSocket option = None

let connect(clientUrl, groupName:string, onOpen, (onMsg: string -> unit)) =
    printfn "Connecting to pubsub!"
    let isConnected =
        match socket with
        | None -> false
        | Some socket -> socket.readyState = WebSocketState.OPEN
    let s = Browser.WebSocket.WebSocket.Create(clientUrl, U2.Case1 "json.webpubsub.azure.v1")
    s.onmessage <-
        fun (event: MessageEvent) ->
            if event.``type`` = "message" then
                match Decode.Auto.fromString (event.data |> unbox) with
                | Ok (message: PubSubMessage) when message.``type`` = "message" ->
                    printfn $"Received message: '{message.data}'"
                    System.Console.WriteLine(message.data)
                    message.data |> unbox |> onMsg
                | _ -> () // ignore. It's not user data.
    s.onopen <-
        fun _ ->
            printfn "Connected to pubsub"
            onOpen()
            let jsonString = Encode.Auto.toString(0, {| ``type`` = "joinGroup"; group = groupName |})
            s.send(jsonString)
    s.onclose <-
        fun _ ->
            printfn "Disconnecting from pubsub"
            socket <- None
    s.onerror <-
        fun (event: Event) ->
            printfn $"Pubsub error: '{event}'"
            System.Console.WriteLine(event)
    socket <- Some s
    let transmit jsonTxt =
        let json = Encode.Auto.toString(0, { ``type`` = "sendToGroup"; group = groupName; noEcho = true; dataType = "text"; data = jsonTxt })
        s.send(json)
    transmit

(* TEMPLATE JavaScript (imperative)
  const isConnected = webSocket?.readyState === WebSocket.OPEN;
  if (!isConnected) {
    let tokenResponse = await fetch(
      `/api/CreateTokenRequest/${hubName}/${groupName}/${user.id}`
    );
    let clientUrl = await tokenResponse.text();
    webSocket = new WebSocket(clientUrl, "json.webpubsub.azure.v1");
    webSocket.onopen = () => {
      console.log("Connected 🎉");
      select("#connectButton").elt.innerText = "Disconnect";
      webSocket.send(
        JSON.stringify({
          type: "joinGroup",
          group: groupName,
        })
      );
      addUser(user.id, user.strokeColor);
      webSocket.send(
        JSON.stringify({
          type: "sendToGroup",
          group: groupName,
          noEcho: true,
          data: {
            messageType: joinedMessage,
            clientId: user.id,
            color: user.strokeColor,
          }
        })
      );
    };
    webSocket.onclose = (event) => {
      console.log(`Disconnected 😿, code=${event.code}`);
      select("#connectButton").elt.innerText = "Connect";
    };
    webSocket.onmessage = (event) => {
      const message = JSON.parse(event.data);
      if (message.type === "message") {
        switch (message.data.messageType) {
          case hoverPositionMessage:
            setUserPosition(
              message.data.clientId,
              message.data.color,
              message.data.x,
              message.data.y
            );
            break;
          case clickPositionMessage:
            clickCell(message.data.x, message.data.y);
            break;
          case changeColorPaletteMessage:
            handleChangeColorPalette(
              message.data.paletteId,
              message.data.colors
            );
            break;
          case resetMessage:
            resetGrid();
            break;
          case joinedMessage:
            addUser(message.data.clientId, message.data.color);
            break;
          default:
            break;
        }
      }
    };
    webSocket.onerror = (event) => {
      this.error = `WebSocket error ${event.message}`;
    };
  } else {
    webSocket.close();
    disconnectUser();
  }

*)