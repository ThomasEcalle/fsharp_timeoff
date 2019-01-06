namespace TimeOff

open System
   
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


// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =
    open System
    open System
    open System.Text.RegularExpressions
    open System.Runtime.ConstrainedExecution

    let NB_TIMEOFF_PER_MONTH = 2
    
    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Cancelled of TimeOffRequest
        | Validated of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Cancelled request
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Cancelled _ -> false
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCanceled request -> Cancelled request
        | RequestCancellationSent request -> state

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)


    let isContainedIn request1 request2 =
        request1.Start <= request2.End && request1.End >= request2.Start
        
    let overlapsWith request1 request2 =
        request1.Start = request2.Start 
        || request1.End = request2.End
        || request1.End = request2.Start
        || request1.Start = request2.End
        || request1 |> isContainedIn request2 
        || request2 |> isContainedIn request1
    
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
    
    let balanceYear (dateProvider : IDateProvider) = (dateProvider.getDate().Month - 1) * NB_TIMEOFF_PER_MONTH
        
    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
                            otherRequests
                            |> Seq.exists (overlapsWith request)
                            
    let createRequest (dateProvider: IDateProvider) activeUserRequests  request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= dateProvider.getDate() then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
    
    let cancelRequest requestState =
            match requestState with
            | PendingValidation request
            | Validated request ->
                Ok [RequestCanceled request]
            | _ ->
                Error "Request cannot be cancelled"
    
    let askToCancelRequest requestState =
                match requestState with
                | PendingValidation request
                | Validated request ->
                    Ok [RequestCancellationSent request]
                | _ ->
                    Error "Cannot ask to cancel request"
            
    let takenToDate (dateProvider: IDateProvider) (userRequests: UserRequestsState)  =
                userRequests
                |> Map.toSeq
                |> Seq.map (fun (_, state) -> state)
                |> Seq.where (fun state -> state.IsActive)
                |> Seq.map (fun state -> state.Request)
                |> Seq.where (fun request -> request.Start.Date <= dateProvider.getDate() && request.Start.Date.Year = dateProvider.getDate().Year)
                |> Seq.map (fun request -> getRequestDuration request) 
                |> Seq.sum
            
    let planned (dateProvider: IDateProvider) (userRequests: UserRequestsState)  =
            userRequests
            |> Map.toSeq
            |> Seq.map (fun (_, state) -> state)
            |> Seq.where (fun state -> state.IsActive)
            |> Seq.map (fun state -> state.Request)
            |> Seq.where (fun request -> request.Start.Date > dateProvider.getDate())
            |> Seq.map (fun request -> getRequestDuration request) 
            |> Seq.sum
    
    let daysObtainedTillFirstDayToThisYear (dateProvider: IDateProvider) = 
        let firstDay = DateTime(2018, 01, 01)
        let lastYearEnd = DateTime(dateProvider.getDate().Year, 01, 01)
        (((lastYearEnd.Year - firstDay.Year) * 12) + lastYearEnd.Month - firstDay.Month) * NB_TIMEOFF_PER_MONTH
            
    let carriedOver (dateProvider: IDateProvider) (userRequests: UserRequestsState)  =
                let daysObtainedTillFirstDay = daysObtainedTillFirstDayToThisYear dateProvider
                let totalTimeOff = 
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)
                    |> Seq.where (fun request -> request.Start.Date.Year < dateProvider.getDate().Year)
                    |> Seq.map (fun request -> getRequestDuration request) 
                    |> Seq.sum
                
                float(daysObtainedTillFirstDay) - totalTimeOff
            
            
            
    let decide (dateProvider: IDateProvider)(userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest dateProvider activeUserRequests request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
            | CancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                if user <> Manager && requestState.Request.Start.Date <= dateProvider.getDate() then
                        Error "Unable to cancel timeoff"
                else
                    cancelRequest requestState
            | AskToCancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                if user <> Manager && requestState.Request.Start.Date <= dateProvider.getDate() then
                        askToCancelRequest requestState
                else
                    Error "Unable to ask for cancellation"   
            
    let getBalance (dateProvider: IDateProvider)(userRequests: UserRequestsState) (user: User) (userId: UserId) =
        let relatedUserId = userId
        match user with
        | Employee userId when userId <> relatedUserId ->
                    Error "Unauthorized"
        | _ ->
            let balanceYear = balanceYear dateProvider
            let takenToDate = takenToDate dateProvider userRequests
            let planned = planned dateProvider userRequests
            let carriedOver = carriedOver dateProvider userRequests
            
            let balance : UserVacationBalance = {
              UserName = userId
              BalanceYear = balanceYear
              CarriedOver = carriedOver
              TakenToDate = takenToDate
              Planned = planned
              CurrentBalance = float(balanceYear) + carriedOver - (takenToDate + planned)
            }
            
            Ok[balance]
                  