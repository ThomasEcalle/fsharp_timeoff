namespace TimeOff

open System

// First, we define our domain
type UserId = string

type User =
    | Employee of UserId
    | Manager

type HalfDay = | AM | PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

[<CLIMutable>]
type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}

[<CLIMutable>]
type UserVacationBalance = {
  Year : int
  UserName : UserId
  BalanceYear: int
  CarriedOver: float
  TakenToDate: float
  Planned: float
  CurrentBalance: float
}


// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | CancelRequest of UserId * Guid
    | AskToCancelRequest of UserId * Guid
    | ValidateRequest of UserId * Guid with
    member this.UserId : UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | AskToCancelRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest * DateTime
    | RequestCanceled of TimeOffRequest * DateTime
    | RequestCancellationSent of TimeOffRequest * DateTime
    | RequestValidated of TimeOffRequest  * DateTime with
    member this.Request : TimeOffRequest =
        match this with
        | RequestCreated (request, date) -> request
        | RequestValidated (request, date) -> request
        | RequestCanceled (request, date) -> request
        | RequestCancellationSent (request, date) -> request
    member this.EventDate : DateTime =
            match this with
            | RequestCreated (request, date) -> date
            | RequestValidated (request, date) -> date
            | RequestCanceled (request, date) -> date
            | RequestCancellationSent (request, date) -> date

type IDateProvider =
   abstract member getDate: unit -> DateTime
   
type TodayDateProvider() =    
    interface IDateProvider with 
        member this.getDate()  = 
            DateTime.Today

type DummyDateProvider() =    
     interface IDateProvider with 
         member this.getDate()  = 
             DateTime(2019, 03, 05)
             

module Utils =      
    let getRequestDuration request = 
            if request.Start.Date > request.End.Date then
                0.0
            else 
                let days = (request.End.Date - request.Start.Date).TotalDays
                let endDelta = match request.End.HalfDay with 
                                | AM -> 0.5
                                | PM -> 1.0
                let startDelta = match request.Start.HalfDay with 
                                    | AM -> 0.0
                                    | PM -> 0.5
                (days + endDelta) - startDelta