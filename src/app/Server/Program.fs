module ServerCode.App

open TimeOff
open Storage.Events
open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.Serialization.Json
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open FSharp.Control.Tasks
open Giraffe
open Thoth.Json.Giraffe

// ---------------------------------
// Handlers
// ---------------------------------

module HttpHandlers =

    open Microsoft.AspNetCore.Http
    open TimeOff
    open TimeOff

    [<CLIMutable>]
    type UserAndRequestId = {
        UserId: UserId
        RequestId: Guid
    }
    
    [<CLIMutable>]
        type UserAndYear = {
            UserId: UserId
            Year: String
        }

    let requestTimeOff (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! timeOffRequest = ctx.BindJsonAsync<TimeOffRequest>()
                let command = RequestTimeOff timeOffRequest
                let result = handleCommand command
                match result with
                | Ok _ -> return! json timeOffRequest next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let validateRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = ValidateRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestValidated (timeOffRequest, date)] -> return! json (timeOffRequest, date) next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
    
    let cancelTimeoff (handleCommand: Command -> Result<RequestEvent list, string>) =
            fun (next: HttpFunc) (ctx: HttpContext) ->
                task {
                    let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                    let command = CancelRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                    let result = handleCommand command
                    match result with
                    | Ok requestCancelled -> return! json requestCancelled next ctx
                    | Error message ->
                        return! (BAD_REQUEST message) next ctx
                }

    let getUserBalance (retrieveBalance: User -> UserId -> Result<UserVacationBalance list, string>) (authentifiedUser: User) (userName: string) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let result = retrieveBalance authentifiedUser userName
                match result with
                    | Ok [balance] -> return! json balance next ctx
                    | Ok _ -> return! Successful.NO_CONTENT next ctx
                    | Error message ->
                        return! (BAD_REQUEST message) next ctx
            }
            
    let getUserHistoric (retrieveHistoric: User -> UserId -> DateTime -> seq<RequestEvent>) (authentifiedUser: User)  =
            fun (next: HttpFunc) (ctx: HttpContext) ->
                task {
                    let userAndYear = ctx.BindQueryString<UserAndYear>()
                    let result = retrieveHistoric authentifiedUser userAndYear.UserId (new DateTime(int(userAndYear.Year), 1, 1))
                    return! json result next ctx
                }

// ---------------------------------
// Web app
// ---------------------------------

let webApp (eventStore: IStore<UserId, RequestEvent>) =

    //eventStore.GetStream("thomas").Clear()

    let dateProvider = new DummyDateProvider()
    
    let getUserState (userId: UserId) = 
        let eventStream = eventStore.GetStream(userId)
        eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty
        
    let addToState userId events = 
        let eventStream = eventStore.GetStream(userId)
        eventStream.Append(events)
        
    let retrieveBalance  (authentifiedUser: User) (userName: UserId) =
        let state = getUserState userName
        //eventStore.GetStream(userName).Clear()
        Logic.getBalance dateProvider state authentifiedUser userName
        
    let retrieveHistoric (authentifiedUser: User) (userName: UserId) (date: DateTime) =
        let state = getUserState userName
        eventStore.GetStream(userName).ReadAll() |> Logic.historicForYear date
        
    let handleCommand (user: User) (command: Command) =
        let userId = command.UserId
        let state = getUserState userId

        // Decide how to handle the command
        let result = Logic.decide dateProvider state user command

        // Save events in case of success
        match result with
        | Ok events -> addToState userId events
        | _ -> ()

        // Finally, return the result
        result

    choose [
        subRoute "/api"
            (choose [
                route "/users/login/" >=> POST >=> Auth.Handlers.login
                subRoute "/timeoff"
                    (Auth.Handlers.requiresJwtTokenForAPI (fun user ->
                        choose [
                            POST >=>
                                (choose [                        
                                    routex "/request/?" >=> HttpHandlers.requestTimeOff (handleCommand user)
                                    routex "/validate-request/?" >=> HttpHandlers.validateRequest (handleCommand user)
                                    routex "/cancel/?" >=> HttpHandlers.cancelTimeoff (handleCommand user)
                                ])
                            GET >=> routef "/user-balance/%s" (HttpHandlers.getUserBalance retrieveBalance user)
                            GET >=> routex "/user-historic/?" >=> HttpHandlers.getUserHistoric retrieveHistoric user
                        ]
                    ))
            ])
        RequestErrors.NOT_FOUND "Not found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (eventStore: IStore<UserId, RequestEvent>) (app: IApplicationBuilder) =
    let webApp = webApp eventStore
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services.AddCors() |> ignore
    services.AddGiraffe() |> ignore
    services.AddSingleton<IJsonSerializer>(ThothSerializer()) |> ignore

let configureLogging (builder: ILoggingBuilder) =
    let filter (l: LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()

    //let eventStore = InMemoryStore.Create<UserId, RequestEvent>()
    let storagePath = System.IO.Path.Combine(contentRoot, "../../../.storage", "userRequests")
    let eventStore = FileSystemStore.Create<UserId, RequestEvent>(storagePath, id)

    let webRoot = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder>(configureApp eventStore))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0