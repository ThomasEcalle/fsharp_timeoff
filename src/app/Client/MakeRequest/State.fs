module Client.MakeRequest.State

open TimeOff
open System
open Fable.Core.JsInterop
open Fable.PowerPack
open Fable.PowerPack.Fetch.Fetch_types
open Elmish
open Fable.Import
open Thoth.Json

open System
open System.Data.SqlTypes
open System.Data.SqlTypes
open TimeOff.AuthTypes
open Client.MakeRequest.Types
open Fable.PowerPack
open Fable.PowerPack

let sendRequest (request: TimeOffRequest, token: JWT) =
  promise {
      if String.IsNullOrEmpty request.UserId then return! failwithf "UserId is required" else
      if String.IsNullOrEmpty (string(request.Start.Date)) then return! failwithf "You need to fill in a start." else
      if String.IsNullOrEmpty (string(request.End.Date)) then return! failwithf "You need to fill in an end." else

      let body = Encode.Auto.toString(2, request)

      let props = 
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [
              HttpRequestHeaders.ContentType "application/json" 
              HttpRequestHeaders.Authorization ("Bearer " + token)]
            RequestProperties.Body !^body ]
      
      try
          let! response = Fetch.fetch ServerUrls.SendRequest props

          if not response.Ok then
              return! failwithf "Error: %d" response.Status
          else  
              let! request = response.text() |> Promise.map Decode.Auto.fromString<TimeOffRequest>
              match request with
              | Result.Ok request -> return request
              | _ -> return! failwithf "Could not send request."
      with
      | _ -> return! failwithf "Could not send request."
  }

let init (user: UserData option) = 
    match user with
    | None ->
        { Timeoff = { Start = { Date = DateTime(2018,01,01); HalfDay = AM }; End = { Date = DateTime(2018,01,01); HalfDay = AM }; RequestId = Guid.NewGuid(); UserId = "";}
          UserData = user.Value
          SuccessMsg = ""
          ErrorMsg = "" 
        }, Cmd.none
    | Some user ->
        { Timeoff = { Start = { Date = DateTime(2018,01,01); HalfDay = AM }; End = { Date = DateTime(2018,01,01); HalfDay = AM }; RequestId = Guid.NewGuid(); UserId = user.UserName;}
          UserData = user
          SuccessMsg = ""
          ErrorMsg = "" 
        }, Cmd.none

let sendRequestCmd request model = 
  Cmd.ofPromise sendRequest (request, model.UserData.Token) RequestSuccess RequestError

let update msg model =
    Browser.console.warn("Update = " + string(msg));
    match msg with
    | RequestSuccess requestData ->
        { model with  Timeoff = requestData; SuccessMsg = "Request well created !"}, []
        
    | SetStartDate startDate ->
        let startDateTime = (DateTime.Parse startDate);
               
        { model with Timeoff = { model.Timeoff with Start = {model.Timeoff.Start with Date = startDateTime}; } }, Cmd.none
        
    | SetEndDate endDate ->
        let endDateTime = (DateTime.Parse endDate);
                      
        { model with Timeoff = { model.Timeoff with End = {model.Timeoff.End with Date = endDateTime};} }, Cmd.none
    
    | SetStartDateHalf startDateHalf ->
            let result = match startDateHalf with
                          | "AM" -> AM
                          | _ -> PM;
                          
            { model with Timeoff = { model.Timeoff with Start = {model.Timeoff.Start with HalfDay = result};} }, Cmd.none
            
    | SetEndDateHalf endDateHalf ->
                let result = match endDateHalf with
                              | "AM" -> AM
                              | _ -> PM;
                              
                { model with Timeoff = { model.Timeoff with End = {model.Timeoff.End with HalfDay = result};} }, Cmd.none
                
    | ClickRequest ->
        model, sendRequestCmd model.Timeoff model
        
    | RequestError error ->
        { model with ErrorMsg = error.Message}, Cmd.none