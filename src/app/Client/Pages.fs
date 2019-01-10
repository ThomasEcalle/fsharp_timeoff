namespace Client

open Elmish.Browser.UrlParser

/// The different pages of the application. If you add a new page, then add an entry here.
[<RequireQualifiedAccess>]
type Page =
    | MakeRequest
    | Login
    | About
    | Employees
    | Balance of userName:string option
    | Historic of userName:string option

module Pages =
    let toPath =
        function
        | Page.MakeRequest -> "#makeRequest"
        | Page.About -> "#about"
        | Page.Login -> "#login"
        | Page.Employees -> "#employees"
        | Page.Balance None -> "#balance"
        | Page.Balance (Some userName) -> sprintf "#balance/%s" userName
        | Page.Historic None -> "#historic"
        | Page.Historic (Some userName) -> sprintf "#historic/%s" userName

    /// The URL is turned into a Result.
    let pageParser : Parser<Page -> Page,_> =
        oneOf [
            map Page.MakeRequest (s "makeRequest")
            map Page.Login (s "login")
            map Page.About (s "about")
            map Page.Employees (s "employees")
            map (Page.Balance << Some) (s "balance" </> str)
            map (Page.Balance None) (s "balance")
            map (Page.Historic << Some) (s "historic" </> str)
            map (Page.Historic None) (s "historic")
        ]

    let urlParser location =
        match parseHash pageParser location with
        | Some page -> Some page
        | None -> parsePath (oneOf [ map Page.MakeRequest (s "") ]) location
