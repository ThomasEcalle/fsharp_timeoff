module Client.Historic.State

open TimeOff
open System
open Fable.Core.JsInterop
open Fable.PowerPack
open Fable.PowerPack.Fetch.Fetch_types
open Elmish
open Thoth.Json

open TimeOff
open TimeOff.AuthTypes
open Client
open Client.Historic.Types

let getUserHistoric token userName =
    promise {
        let url = ServerUrls.UserHistoric
        
        let bodyToSend : BodyToSend = {
          UserId = userName  
          Year = "2019"
        }
        
        //let body = Encode.Auto.toString(0, bodyToSend)
        
        let props = 
                  [ RequestProperties.Method HttpMethod.GET
                    Fetch.requestHeaders [
                      HttpRequestHeaders.Authorization ("Bearer " + token)
                    ]
                    //RequestProperties.Body !^body 
                    ]

        let urlAndParams = string (sprintf "%s?UserId=%s&Year=%s" url bodyToSend.UserId bodyToSend.Year)
        let! res = Fetch.fetch (urlAndParams) props
       
        let! txt = res.text()
        return Decode.Auto.unsafeFromString<List<RequestEvent>> txt
    }

let init userData userToDisplay : Model * Cmd<Msg> =
  { UserData = userData; UserToDisplay = defaultArg userToDisplay userData.UserName; Historic = None }, Cmd.ofMsg FetchHistoric

let update msg model =
  match msg with
  | FetchHistoric ->
      model, Cmd.ofPromise (getUserHistoric model.UserData.Token) model.UserToDisplay DisplayHistoric NetworkError
  | DisplayHistoric historic ->
      printf "Display historic"
      { model with Historic = Some historic }, []
  | NetworkError error ->
    printfn "[Balance.State][Network error] %s" error.Message
    model, Cmd.none
