module Main exposing (main)

import Html exposing (Html, div, text)



-- MAIN


main : Html message
main =
    view initialModel



-- main: Program () Model Msg
-- main = Browser.sandbox
--     { init = initialModel
--     , view = view
--     , update = update
--     }


-- MODEL


type alias ChipherAlphabet =
    String


type alias PlainAlphabet =
    String


type alias Model =
    { chipher : ChipherAlphabet
    , plain : PlainAlphabet
    , pairs : Maybe (List ( ChipherAlphabet, PlainAlphabet ))
    , text : String
    }


initialModel : Model
initialModel =
    { chipher = ""
    , plain = ""
    , pairs = Maybe.Nothing
    , text = "abcd"
    }



-- VIEW


view : Model -> Html message
view _ =
    div [] [ text "hello" ]
