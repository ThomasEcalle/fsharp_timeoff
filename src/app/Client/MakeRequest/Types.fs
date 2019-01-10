module Client.MakeRequest.Types
open System

open TimeOff
open TimeOff.AuthTypes

type Model = { 
    UserData: UserData
    Timeoff : TimeOffRequest
    ErrorMsg : string 
    SuccessMsg : string 
}

type Msg =
  | SetStartDate of string
  | SetEndDate of string
  | SetStartDateHalf of string
  | SetEndDateHalf of string
  | RequestSuccess of TimeOffRequest
  | RequestError of exn
  | ClickRequest
