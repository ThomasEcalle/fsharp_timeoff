module Client.Employees.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma

open Client
open System.ComponentModel

let employeeList dispatch =
  let employeeLine (employeeName: string) =
    let userName = employeeName
    tr [ ]
      [
        td [] [ str userName ]
        td [] [ str "Employee "; str (userName) ]
        td [] [ a [ Href (Pages.toPath (Page.Balance (Some userName))) ] [ str "View balance" ] ]
        td [] [ a [ Href (Pages.toPath (Page.Historic (Some userName))) ] [ str "View requests" ] ]
      ]

  div []
    [
      Table.table [ Table.IsBordered
                    Table.IsStriped ]
        [
          thead []
            [
              yield tr []
                [
                  th [] [str "User"]
                  th [] [str "Employee"]
                  th [] []
                ]
            ]
          tbody []
            [
              yield employeeLine ("thomas")
              for employeeNumber in 2..5 do
                yield employeeLine (sprintf "employee%d" employeeNumber)
            ]
        ]
    ]

let root dispatch =
  div []
    [ Heading.h3 [ ]
        [ str "Employees list" ]
      employeeList dispatch ]
