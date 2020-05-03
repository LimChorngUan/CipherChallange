module Main exposing (main)

import Html exposing (Html, div, h1, text, button, p)



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


type alias CipherAlphabet =
    String


type alias PlainAlphabet =
    String


type alias Model =
    { cipher : CipherAlphabet
    , plain : PlainAlphabet
    , pairs : Maybe (List ( CipherAlphabet, PlainAlphabet ))
    , text : String
    }


initialModel : Model
initialModel =
    { cipher = ""
    , plain = ""
    , pairs = Maybe.Nothing
    , text = "abcd"
    }



-- VIEW


view : Model -> Html message
view _ =
    div []
        [ h1 [] [ text "Cipher Challange Stage 1: Simple Monoalphabetic Substitution Cipher"]
        , p [] [ text "Explanation here"]
        , div [] cipherButtonsView
        , div [] plainButtonsView
        ]


cipherButtonsView : List (Html message)
cipherButtonsView =
    List.map charButtonView genAllUpperCaseChars


plainButtonsView : List (Html message)
plainButtonsView =
    List.map charButtonView genAllLowerCaseChars



charButtonView : Char -> Html message
charButtonView char =
    button [] [ text (String.fromChar char) ]


-- HELPER


generateChars : List Int -> List Char
generateChars unicodes =
    List.map Char.fromCode unicodes


genAllUpperCaseChars : List Char
genAllUpperCaseChars =
    generateChars (List.range 65 90)


genAllLowerCaseChars : List Char
genAllLowerCaseChars =
    generateChars (List.range 97 122)