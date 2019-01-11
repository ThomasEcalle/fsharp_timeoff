module Client.Historic.Types

open System
open System.Globalization
open TimeOff
open TimeOff.AuthTypes

type Model = {
  UserData: UserData
  UserToDisplay: string
  Historic: List<RequestEvent> option
}

[<CLIMutable>]
type BodyToSend = {
  UserId: string
  Year: string
}

type Msg =
  | FetchHistoric
  | DisplayHistoric of List<RequestEvent>
  | CancelRequest of Guid
  | NetworkError of exn
