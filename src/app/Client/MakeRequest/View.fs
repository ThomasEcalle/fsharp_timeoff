module Client.MakeRequest.View

open System
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome

open Client
open Types

let root model (dispatch: Msg -> unit) = 
  let buttonActive = Button.Color IsPrimary 

  let notification =
    if not (String.IsNullOrEmpty model.ErrorMsg) then
      Notification.notification [ Notification.Color IsDanger ] [
        div [] [ str model.ErrorMsg ]
      ]
    else if not (String.IsNullOrEmpty model.SuccessMsg) then
      Notification.notification [ Notification.Color IsSuccess ] [
              div [] [ str model.SuccessMsg ]
            ]
    else 
      Notification.notification [ Notification.Color IsInfo ] [
                    div [] [ str "Please enter dates in future" ]
                  ]

  form [ ] [
  
    Heading.h3 [ ]
              [ str "Make a Request" ]
    notification
    
    Field.div [ ]
      [ Label.label [ ]
          [ str "Start Date" ]
        Control.div [ Control.HasIconLeft ]
          [ Input.input [ Input.Type Input.Date
                          Input.Id "startDate"
                          Input.Placeholder "Start Date"
                          Input.Props [
                            OnChange (fun ev -> dispatch (SetStartDate !!ev.target?value))
                             ] ]
            Icon.faIcon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.icon Fa.I.Calendar ] ] ]
    
    Field.div [ ]
      [ Label.label [ ]
          [ str "Start Date HalfDay" ]
        Control.div [ ]
          [ Select.select [ 
            Select.Props [
              OnChange (fun ev -> dispatch (SetStartDateHalf !!ev.target?value))
             ]
          ]
              [ select 
                  [ DefaultValue (string(model.Timeoff.Start.HalfDay)) ]
                  [ 
                    option [ Value "AM" ] [ str "AM" ]
                    option [ Value "PM"] [ str "PM" ]
                  ] 
              ] 
          ] 
      ]
                
    Field.div [ ]
      [ Label.label [ ]
          [ str "End Date" ]
        Control.div [ Control.HasIconLeft ]
          [ Input.input [ Input.Type Input.Date
                          Input.Placeholder "End Date"
                          Input.Id "endDate"
                          Input.Props [
                            OnChange (fun ev -> dispatch (SetEndDate !!ev.target?value))
                            OnEnter ClickRequest dispatch ] ]
            Icon.faIcon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.icon Fa.I.Calendar ] ] ]
    
    Field.div [ ]
          [ Label.label [ ]
              [ str "End Date HalfDay" ]
            Control.div [ ]
              [ Select.select [ 
                Select.Props [
                  OnChange (fun ev -> dispatch (SetEndDateHalf !!ev.target?value))
                 ]
              ]
                  [ select 
                      [ DefaultValue (string(model.Timeoff.Start.HalfDay)) ]
                      [ 
                        option [ Value "AM" ] [ str "AM" ]
                        option [ Value "PM"] [ str "PM" ]
                      ] 
                  ] 
              ] 
          ]
     
    div [ ClassName "text-center" ] [
        Button.a [buttonActive; Button.OnClick  (fun _ -> dispatch ClickRequest)] [ str "Send Request" ]
    ]
  ]    
