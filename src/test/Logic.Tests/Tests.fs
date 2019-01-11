module TimeOff.Tests

open Expecto
open System

type TodayDateProvider() =    
    interface IDateProvider with 
        member this.getDate()  = 
            DateTime.Today

let dateProvider = new TodayDateProvider()
let todayDateProvider =  dateProvider :> IDateProvider

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide dateProvider userRequestsState user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 3, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 3, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 3, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 3, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 3, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 3, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
    
    test "Requests lapsing over each other should overlap" {
          let request1 = {
            UserId = "jdoe"
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2019, 3, 1); HalfDay = AM }
            End = { Date = DateTime(2019, 3, 8); HalfDay = PM }
          }
    
          let request2 = {
            UserId = "jdoe"
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2019, 3, 2); HalfDay = PM }
            End = { Date = DateTime(2019, 3, 5); HalfDay = PM }
          }
    
          Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap"
        }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 03, 05); HalfDay = AM }
        End = { Date = DateTime(2019, 03, 05); HalfDay = AM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated (request, todayDateProvider.getDate())]) "The request should have been created"
    }
    test "A request cannot be created in past" {
          let request = {
            UserId = "jdoe"
            RequestId = Guid.Empty
            Start = { Date = DateTime(2018, 11, 28); HalfDay = AM }
            End = { Date = DateTime(2018, 11, 28); HalfDay = PM } }
    
          Given [ ]
          |> ConnectedAs (Employee "jdoe")
          |> When (RequestTimeOff request)
          |> Then (Error "The request starts in the past") "The request should have not been created"
        }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 3, 05); HalfDay = AM }
        End = { Date = DateTime(2019, 3, 05); HalfDay = PM } }

      Given [ RequestCreated (request, todayDateProvider.getDate()) ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated (request, todayDateProvider.getDate())]) "The request should have been validated"
    }
  ]

[<Tests>]
let cancelationTests =
  testList "Cancelation tests" [
  
    test "A request is cancelled by Manager" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 3, 05); HalfDay = AM }
        End = { Date = DateTime(2019, 3, 05); HalfDay = PM } 
        }

      Given [ RequestCreated (request, todayDateProvider.getDate()) ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCanceled (request, todayDateProvider.getDate())]) "The request should have been canceled by the Manager"
    }
    
    test "A request is cancelled by an Employee" {
          let request = {
            UserId = "thomas"
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2019, 3, 28); HalfDay = AM }
            End = { Date = DateTime(2019, 3, 28); HalfDay = PM } 
            }
    
          Given [ RequestCreated (request, todayDateProvider.getDate()) ]
          |> ConnectedAs (Employee "thomas")
          |> When (CancelRequest ("thomas", request.RequestId))
          |> Then (Ok [RequestCanceled (request, todayDateProvider.getDate())]) "The request should have been canceled by the employee"
     }
        
    test "An employee cannot cancel today's timeoff" {
      let dateProvider = new TodayDateProvider()
      let todayDateProvider =  dateProvider :> IDateProvider
      let request = {
        UserId = "thomas"
        RequestId = Guid.NewGuid()
        Start = { Date = todayDateProvider.getDate(); HalfDay = AM }
        End = { Date = todayDateProvider.getDate(); HalfDay = PM }
      }
      
      Given [RequestCreated (request, todayDateProvider.getDate())]
      |> ConnectedAs (Employee "thomas")
      |> When (CancelRequest ("thomas", request.RequestId))
      |> Then (Error "Unable to cancel timeoff") "The request should not have been canceled by the employee"
    }
    
    test "An employee can ask to cancel today's timeoff" {
          let request = {
            UserId = "thomas"
            RequestId = Guid.NewGuid()
            Start = { Date = todayDateProvider.getDate(); HalfDay = AM }
            End = { Date = todayDateProvider.getDate(); HalfDay = PM }
          }
          
          Given [RequestCreated (request, todayDateProvider.getDate())]
          |> ConnectedAs (Employee "thomas")
          |> When (AskToCancelRequest ("thomas", request.RequestId))
          |> Then (Ok [RequestCancellationSent (request, todayDateProvider.getDate())]) "The request should be waiting for cancellation"
        }
    
    test "A manager can confirm a cancellation demand" {
              let request = {
                UserId = "thomas"
                RequestId = Guid.NewGuid()
                Start = { Date = todayDateProvider.getDate(); HalfDay = AM }
                End = { Date = todayDateProvider.getDate(); HalfDay = PM }
              }
              
              Given [RequestCreated (request, todayDateProvider.getDate()); RequestCancellationSent (request, todayDateProvider.getDate())]
              |> ConnectedAs Manager
              |> When (CancelRequest ("thomas", request.RequestId))
              |> Then (Ok [RequestCanceled (request, todayDateProvider.getDate())]) "The request should be cancelled"
            }
        
  ]
  
  
  
[<Tests>]
let balanceTests = 
  testList "balance Tests" [
    test "Calcul one timeoff duration" {
       let request = {
          UserId = "thomas"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 3, 1); HalfDay = AM }
          End = { Date = DateTime(2019, 3, 14); HalfDay = PM }
        }
        
       Expect.equal (Utils.getRequestDuration request) 14.0 "The requests duration is not good"
    }
  ]