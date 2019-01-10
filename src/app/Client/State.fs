﻿module Client.State

open Elmish
open Elmish.Browser.Navigation
open Fable.Import
open Global
open Types

let private stayOnCurrentPage model =
  model, Navigation.modifyUrl (Pages.toPath model.Navigation.CurrentPage)

/// The navigation logic of the application given a page identity parsed from the .../#info
/// information in the URL.
let urlUpdate (result: Page option) model =
  match result with
  | None ->
    Browser.console.error("Error parsing url: " + Browser.window.location.href)
    stayOnCurrentPage model
  | Some (Page.Login as page) ->
   let m, cmd = Login.State.init model.Navigation.User
   { model with Navigation = { model.Navigation with CurrentPage = page }; TransientPageModel = LoginModel m }, Cmd.map LoginMsg cmd
  | Some (Page.Balance userName as page) ->
    match model.Navigation.User with
    | Some user ->
      let m, cmd = Balance.State.init user userName
      { model with Navigation = { model.Navigation with CurrentPage = page }; TransientPageModel = BalanceModel m }, Cmd.map BalanceMsg cmd
    | None ->
      stayOnCurrentPage model
  | Some (Page.Historic userName as page) ->
      match model.Navigation.User with
      | Some user ->
        let m, cmd = Historic.State.init user userName
        { model with Navigation = { model.Navigation with CurrentPage = page }; TransientPageModel = HistoricModel m }, Cmd.map HistoricMsg cmd
      | None ->
        stayOnCurrentPage model
  | Some page ->
    { model with Navigation = { model.Navigation with CurrentPage = page }; TransientPageModel = NoPageModel }, []

let init result =
  let (home, homeCmd) = MakeRequest.State.init()
  let (model, cmd) =
    urlUpdate result
      {
        Navigation =
          {
            User = LocalStorage.load "user"
            CurrentPage = Page.MakeRequest
          }
        TransientPageModel = NoPageModel
        Home = home }
  model, Cmd.batch [ cmd
                     Cmd.map MakeRequestMsg homeCmd ]
let loadUser () =
    LocalStorage.load "user"

let saveUserCmd user =
    Cmd.ofFunc (LocalStorage.save "user") user (fun _ -> LoggedIn user) StorageFailure
    |> Cmd.map GlobalMsg

let deleteUserCmd =
    Cmd.ofFunc LocalStorage.delete "user" (fun _ -> LoggedOut) StorageFailure
    |> Cmd.map GlobalMsg

let update msg model =
  match msg, model.TransientPageModel with
  | GlobalMsg (StorageFailure e), _ ->
      printfn "Unable to access local storage: %A" e
      model, Cmd.none

  | GlobalMsg (LoggedIn newUser), _ ->
    { model with Navigation = { model.Navigation with User = Some newUser } }, Navigation.newUrl (Pages.toPath Page.MakeRequest)

  | GlobalMsg LoggedOut, _ ->
    { model with Navigation = { model.Navigation with User = None } }, Navigation.newUrl (Pages.toPath Page.MakeRequest)

  | GlobalMsg Logout, _ ->
    model, deleteUserCmd

  | LoginMsg msg, LoginModel loginModel ->
   let (loginModel, cmd) = Login.State.update LoginMsg saveUserCmd msg loginModel
   { model with TransientPageModel = LoginModel loginModel }, cmd
  | LoginMsg _, _ -> model, Cmd.none

  | BalanceMsg msg, BalanceModel balanceModel ->
    let (balanceModel, balanceCmd) = Balance.State.update msg balanceModel
    { model with TransientPageModel = BalanceModel balanceModel }, Cmd.map BalanceMsg balanceCmd
  | BalanceMsg _, _ -> model, Cmd.none
  
  | HistoricMsg msg, HistoricModel historicModel ->
      let (historicModel, historicCmd) = Historic.State.update msg historicModel
      { model with TransientPageModel = HistoricModel historicModel }, Cmd.map HistoricMsg historicCmd
  | HistoricMsg _, _ -> model, Cmd.none

  | MakeRequestMsg msg, _ ->
    let (home, homeCmd) = MakeRequest.State.update msg model.Home
    { model with Home = home }, Cmd.map MakeRequestMsg homeCmd
