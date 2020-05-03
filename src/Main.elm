module Main exposing (main)

import Html exposing (Html, button, div, h1, h2, p, span, text)



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


type alias CipherLetter =
    String


type alias PlainLetter =
    String


type alias Model =
    { cipher : CipherLetter
    , plain : PlainLetter
    , pairs : List ( CipherLetter, PlainLetter )
    , text : String
    }


initialModel : Model
initialModel =
    { cipher = ""
    , plain = ""
    , pairs = [ ( "A", "b" ), ( "C", "d" ) ]
    , text = "abcd"
    }



-- ----- VIEW -----


view : Model -> Html message
view model =
    div []
        [ h1 [] [ text "Cipher Challange Stage 1: Simple Monoalphabetic Substitution Cipher" ]
        , p [] [ text "Explanation goes here" ]
        , div [] cipherButtonsView
        , div [] plainButtonsView
        , button [] [ text "Confirm" ]
        , div []
            [ h2 [] [ text "Cipher-Plain pairs" ]
            , div [] (cipherPlainPairsView model.pairs)
            ]
        , button [] [ text "Reset" ]
        , div [] [ text "Cipher text goes here" ]
        ]



-- alphabet button view


charButtonView : Char -> Html message
charButtonView char =
    button [] [ text (String.fromChar char) ]


cipherButtonsView : List (Html message)
cipherButtonsView =
    List.map charButtonView genAllUpperCaseChars


plainButtonsView : List (Html message)
plainButtonsView =
    List.map charButtonView genAllLowerCaseChars



-- cipher-plain pairs view


cipherPlainPairView : ( CipherLetter, PlainLetter ) -> Html message
cipherPlainPairView ( cipher, plain ) =
    div []
        [ span [] [ text (cipher ++ " -> " ++ plain) ]
        , button [] [ text "X" ]
        ]


cipherPlainPairsView : List ( CipherLetter, PlainLetter ) -> List (Html message)
cipherPlainPairsView pairs =
    List.map cipherPlainPairView pairs



-- ----- HELPER -----


generateChars : List Int -> List Char
generateChars unicodes =
    List.map Char.fromCode unicodes


genAllUpperCaseChars : List Char
genAllUpperCaseChars =
    generateChars (List.range 65 90)


genAllLowerCaseChars : List Char
genAllLowerCaseChars =
    generateChars (List.range 97 122)
