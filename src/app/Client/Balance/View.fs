module Client.Balance.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome

open Types

let root model dispatch =
  match model.Balance with
  | Some balance ->
    div []
      [ Heading.h3 [ ]
            [ str "Balance for employee "; str balance.UserName ]
        Box.box' []
          [
            Table.table [ Table.IsNarrow ]
              [
                tbody [ ]
                  [
                    tr [ ]
                      [
                        th [ ClassName "has-text-right" ] [ str "User name" ]
                        td [] []
                        td [ ] [ str balance.UserName ]
                      ]
                    tr [ ]
                      [
                        th [ ClassName "has-text-right" ] [ str (sprintf "Carried over from %d" (balance.Year - 1)) ]
                        td [] []
                        td [ ] [ str (sprintf "%.2f" balance.CarriedOver) ]
                      ]
                    tr [ ]
                      [
                        th [ ClassName "has-text-right" ] [ str (sprintf "Portion of %d allotment accrued to date" balance.Year) ]
                        td [] [ str "+" ]
                        td [ ] [ str (sprintf "%d" balance.BalanceYear) ]
                      ]
                    tr [ ]
                      [
                        th [ ClassName "has-text-right" ] [ str "Taken to date" ]
                        td [] [ str "-" ]
                        td [ ] [ str (sprintf "%.2f" balance.TakenToDate) ]
                      ]
                    tr [ ]
                            [
                              th [ ClassName "has-text-right" ] [ str "Planned" ]
                              td [] [ str "-" ]
                              td [ ] [ str (sprintf "%.2f" balance.Planned) ]
                            ]
                    tr [ ]
                      [
                        th [ ClassName "has-text-right" ] [ str "Current balance" ]
                        td [] [ str "=" ]
                        td [ ] [ str (sprintf "%.2f" balance.CurrentBalance) ]
                      ]
                  ]
              ]
          ]
      ]
    
  | None ->
    div []
      [
        Icon.faIcon [ Icon.Size Size.IsLarge ]
                    [ Fa.icon Fa.I.Spinner
                      Fa.pulse ]
        str (sprintf "Loading vacation balance for user %s" model.UserToDisplay)
      ]