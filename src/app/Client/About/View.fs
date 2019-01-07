module Client.About.View

open Fable.Helpers.React
open Fulma

let root =
  Container.container [ ]
    [ Content.content [ ]
        [ Heading.h3 [ ]
            [ str "About page" ]
          p [ ]
            [ str "YO" ] ] ]
