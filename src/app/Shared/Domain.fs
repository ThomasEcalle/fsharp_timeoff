﻿namespace TimeOff

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
    | RequestCreated of TimeOffRequest
    | RequestCanceled of TimeOffRequest
    | RequestCancellationSent of TimeOffRequest
    | RequestValidated of TimeOffRequest with
    member this.Request : TimeOffRequest =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCanceled request -> request
        | RequestCancellationSent request -> request

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