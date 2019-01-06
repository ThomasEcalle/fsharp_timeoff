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
  UserName : UserId
  BalanceYear: int
  CarriedOver: float
  TakenToDate: float
  Planned: float
  CurrentBalance: float
}

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