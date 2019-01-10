module Client.Historic.View


open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome

open Fable.Import.Browser
open TimeOff
open Types

let root model dispatch =
  match model.Historic with
  | Some historic ->
    let lines historic = 
      let line (requestEvent: RequestEvent) =
        let request = requestEvent.Request
        let (requestType, eventDate) = match requestEvent with 
                                        | RequestCreated (request, date) -> ("New Request", date)
                                        | RequestValidated (request, date) -> ("Request Validated", date)
                                        | RequestCanceled (request, date) -> ("Request Cancelled", date)
                                        | RequestCancellationSent (request, date) -> ("Ask for request cancellation", date)
        tr [ ]
          [
            td [] [ str (eventDate.Date.Day.ToString() + "/" + eventDate.Date.Month.ToString() + "/" + eventDate.Date.Year.ToString())]
            td [] [ str (request.Start.Date.Day.ToString() + "/" + request.Start.Date.Month.ToString() + "/" + request.Start.Date.Year.ToString() + " " + request.Start.HalfDay.ToString())]
            td [] [ str (request.End.Date.Day.ToString() + "/" + request.End.Date.Month.ToString() + "/" + request.End.Date.Year.ToString() + " " + request.Start.HalfDay.ToString()) ]
            td [] [ str (requestType) ]
          ]
      div []
          [
            Table.table [ Table.IsBordered
                          Table.IsStriped ]
              [
                thead []
                  [
                    yield tr []
                      [
                        th [] [str "Date"]
                        th [] [str "From"]
                        th [] [str "To"]
                        th [] [str "Event"]
                      ]
                  ]
                tbody []
                  [
                    for event in historic do
                       yield line event
                    
                  ]
              ]
          ]
          
    div []
        [ Heading.h3 [ ]
            [ str "Historic for employee "; str model.UserToDisplay ]
          lines historic 
        ]
    
  | None ->
    div []
      [
        Icon.faIcon [ Icon.Size Size.IsLarge ]
                    [ Fa.icon Fa.I.Spinner
                      Fa.pulse ]
        str (sprintf "Loading historic for user %s" model.UserToDisplay)
      ]